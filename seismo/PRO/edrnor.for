c--------------------------------------------------------------------------
c  read edr format of USGS, output in nordic format
c--------------------------------------------------------------------------c
c
c  input format read is as show on stanadrd screen
c
c  For detail on parameters and variables naames, see rea.inc
c
c  
c
c changes
c  17/05/2007 lot - changed == to .eq.
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input file
      integer strike,dip,slip
      character*80 text

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer i                           ! counter

!_________________________________________________________________________
      integer          flin_region,nms,nstat,mm,jj,nphas,nstat2
      integer          LNBLNK,fp,lp,lc,nhead
      integer          scol(3)

      character*2      line_id
      character*1      ev_lat_dir
      character*1      ev_lon_dir
      character*1      depth_flag
      character*1      ver_flag
      character*1      loc_quality
      character*5      src_code, station
      character*8      phase

      real             ev_lat,dist
      real             ev_lon
      real             standrd_dev, azimu

      logical          finish_job

!_________________________________________________________________________

c
c   open output file

       open(2,file='edrnor.out',status='unknown')
    
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

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event form file unit 1
c

      read(1,'(a)',end=1000) text      
      if(text(1:2).eq.'HY' .and. nevent .gt. 0 ) goto 77
!_________________________________________________________________

66    if(text(1:2).eq.'HY') then
!          print*,'HY   >>>'
          nstat=0
          nphas=0
          nevent=nevent+1               ! count events

          call rea_hyp_clear(1)

        read(text,101)line_id,hyp_year(1),hyp_month(1),hyp_day(1),
     +       hyp_hour(1),hyp_min(1),hyp_sec(1),loc_quality,ev_lat,
     +       ev_lat_dir,ev_lon,ev_lon_dir,hyp_depth(1),depth_flag,
     +       standrd_dev,rea_nstat, ver_flag,flin_region,src_code

        if(ev_lat_dir .eq. 'N')then
             hyp_lat(1)= ev_lat 
        else if(ev_lat_dir .eq. 'S')then
             hyp_lat(1)= -ev_lat 
        endif

        if(ev_lon_dir .eq. 'E')then
             hyp_lon(1)= ev_lon 
        else if(ev_lon_dir .eq. 'W')then
             hyp_lon(1)= -ev_lon 
        endif

          hyp_dist_id(1)='D'
          hyp_fix_org(1)=' '
          rea_nhyp=1
          nhead = 1
          rea_nhead = nhead
          rea_id_line=' '
          hyp_epi_flag(1)=' '
        goto 50
!-------------------------------------
      else if(text(1:2).eq.'E ') then
!          print*,'E    >>>'

        read(text,102)line_id,hyp_sec_err(1),hyp_lat_err(1),
     +       hyp_lon_err(1),hyp_depth_err(1),hyp_mag(1,1),
     +       hyp_nstat(1),hyp_mag(2,1),nms


        hyp_mag_type(1,1)= 'B'
        hyp_mag_type(2,1)= 'S'
        goto 50
!-------------------------------------
      else if(text(1:2).eq.'L ') then
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'A ') then
!         read(text,104)line_id,rea_nphase,rea_nstat,hyp_gap(1)
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'C ') then
         read(text,105)line_id,rea_comment(1)
         rea_ncomment = 1
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'AH') then
!!        read(text,101)line_id,hyp_year(2),hyp_month(2),hyp_day(2),
!!     +       hyp_hour(2),hyp_min(2),hyp_sec(2),loc_quality,ev_lat,
!!     +       ev_lat_dir,ev_lon,ev_lon_dir,hyp_depth(2),depth_flag,
!!     +       standrd_dev,nstat2, ver_flag,flin_region,src_code
!!        if(ev_lat_dir .eq. 'N')then
!!             hyp_lat(2)= ev_lat 
!!        else if(ev_lat_dir .eq. 'S')then
!!             hyp_lat(2)= -ev_lat 
!!        endif
!!        if(ev_lon_dir .eq. 'E')then
!!             hyp_lon(2)= ev_lon 
!!        else if(ev_lon_dir .eq. 'W')then
!!             hyp_lon(2)= -ev_lon 
!!        endif
!!          nhead = nhead + 1
!!          hyp_dist_id(nhead)='D'
!!          rea_nhyp=rea_nhyp+1
!!          hyp_fix_org(nhead)=' '
!!          hyp_epi_flag(nhead)=' '
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'AE') then
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'Dp') then
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'Dt') then
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'Da') then
!        read(text,110)line_id,strike1,dip1,slip1,strike2,dip2,slip2
!               rea_nfault = 2
!               rea_fault(1)=' '
!               write(rea_fault(1),'(3f10.0)') 
!              rea_fault(1)(71:76)='HARWAR'
!              rea_fault(1)(80:80)='F'
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'Dc') then
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'S ') then
          scol(1) = 8
          scol(2) = 26
          scol(3) = 44

       do jj = 1,3
        if(text(scol(jj):scol(jj)+1).ne.'D=' .and.
     +     text(scol(jj):scol(jj)+7).ne.'        ' )then
     
           nphas=nphas + 1

        if(jj.eq.1)then
        read(text,114)line_id,phase,
     +      rea_hour(nphas),rea_min(nphas),
     +       rea_sec(nphas)
        else if(jj.eq.2)then
        read(text,115)line_id,phase,
     +       rea_hour(nphas),rea_min(nphas),
     +       rea_sec(nphas)
        else if(jj.eq.2)then
        read(text,116)line_id,phase,
     +       rea_hour(nphas),rea_min(nphas),
     +       rea_sec(nphas)
       endif

             rea_auto(nphas) = ' '
             rea_onset(nphas) = ' '
             rea_polarity(nphas)= ' '
             lc =  lnblnk(phase)
          if(phase(lc:lc).eq.'c'.or.phase(lc:lc).eq.'d')then
             if(phase(lc:lc).eq.'c')rea_polarity(nphas)='C'
             if(phase(lc:lc).eq.'d')rea_polarity(nphas)='D'
             lp = lnblnk(phase)-1 
          else
             lp = lnblnk(phase) 
          endif
          if(phase(1:1).eq.'e'.or.phase(1:1).eq.'i')then
             if(phase(1:1).eq.'e')rea_onset(nphas) = 'E'
             if(phase(1:1).eq.'i')rea_onset(nphas) = 'I'
             fp = 2
          else
             fp = 1
          endif
             rea_phase(nphas) = phase(fp:lp)

           rea_az(nphas) = azimu
           rea_weight_in(nphas) = ' '   
           rea_weight_out(nphas) = ' '   
           rea_stat(nphas) = station
           rea_dist(nphas) = dist*111.4
           rea_co(nphas) = 'BZ'
       endif

       end do
         goto 50
!-------------------------------------
      else if(text(1:2).eq.'M ') then

         goto 50
!-------------------------------------
      else if(text(1:2).eq.'P ') then
         nstat=nstat + 1
         nphas=nphas + 1

        read(text,112)line_id,station,
     +      phase,rea_hour(nphas),
     +      rea_min(nphas),rea_sec(nphas),rea_res(nphas),dist,
     +      azimu,rea_per(nphas),rea_amp(nphas)

             rea_az(nphas) = azimu
             rea_weight_in(nphas) = ' '   
             rea_weight_out(nphas) = ' '   
             rea_dist(nphas) = dist*111.4
             rea_stat(nphas) = station
             rea_co(nphas) = 'BZ'
             rea_auto(nphas) = ' '
             rea_onset(nphas) = ' '
             rea_polarity(nphas)= ' '
             lc =  lnblnk(phase)
          if(phase(lc:lc).eq.'c'.or.phase(lc:lc).eq.'d')then
             if(phase(lc:lc).eq.'c')rea_polarity(nphas)='C'
             if(phase(lc:lc).eq.'d')rea_polarity(nphas)='D'
             lp = lnblnk(phase)-1 
          else
             lp = lnblnk(phase) 
          endif
          if(phase(1:1).eq.'e'.or.phase(1:1).eq.'i')then
             if(phase(1:1).eq.'e')rea_onset(nphas) = 'E'
             if(phase(1:1).eq.'i')rea_onset(nphas) = 'I'
             fp = 2
          else
             fp = 1
          endif
             rea_phase(nphas) = phase(fp:lp)

         goto 50
!-------------------------------------
      else
         goto 50
      endif
!_________________________________________________________________
c
c   write out 
c
 1000 continue
      finish_job = .true.

77      continue


          rea_nphase = nphas
!          rea_nstat  = nphas
!          rea_nrecord = nphas
        call rea_event_out(2,all,data,code)
c

!       endif  


c   get next event
c
      if( .not.finish_job) goto 66
c
c     end of file
c
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is edrnor.out'

101     format (a2,i4,i2,i2,1x,i2,i2,f5.2,a1,f6.3,a1,1x,f7.3,a1,1x,f5.1,
     +         a1,f4.2,i3,a1,i3,a5)


!102     format (a2,f5.2,1x,f6.2,1x,f6.2,1x,f5.1,1x,f3.1,1x,i3,
!     +        1x,f3.1,i3,f3.1,a2,a4,f3.1,a2,a4)
102     format (a2,f5.2,1x,f6.2,1x,f6.2,1x,f5.1,1x,f3.1,1x,i3,
     +        1x,f3.1,i3)
103     format (a2,f6.2,f5.2,e8.2,f6.2,f5.2,e8.2,f6.2,f5.2,e8.2,1x)
!104     format (a2,i4,1x,i3,f5.1,1x,f3.1,a2,a5,1x,a1,i7,
!     +          a1,i7,a1,i7,a1,a8)
104     format (a2,i4,1x,i3,f5.1)
105     format (a2, a58)
106     format (a2, i8, 1x,d9.2,a1,f6.3,a1,1x,f7.3,a1,a1,f5.1,
     +        a1,f4.2,i3,i4,a5)
107     format (a2,f5.2,1x,f6.2,1x,f6.2,1x,f5.1,1x,f5.1,f3.1,
     +        a2,5x,f3.1,a2,12x)
108     format (a2,a4,a1,i1,f7.1,f2.1,f4.2,a1,f3.2,f5.2,a1,
     +        f3.2,f4.1,f2.1,i3,i3,i2,i3,f3.1,f2.1,f2.1,i2 )
109     format (a2,1x,i2,1x,a2,f4.2,f3.2,a2,f4.2,f3.2,a2,
     +        f4.2,f3.2,a2,f4.2,f3.2,a2,f4.2,f3.2,a2,f4.2,f3.2)
!110    format (a2,1x,i2,f4.2,f3.2,i2,i3,f4.2,f3.2,i2,i2,f4.2, 
!     +        f3.2,i2,i3,1x,i3,i2,i4,i3,i2,i4)
110    format (a2,39x,i3,i2,i4,i3,i2,i4)
111    format (a2, a58 )
!112    format (a2,a5,a8,f9.2,1x,f5.1,a1,1x,f6.2,1x,
!     + f5.1,f4.1,f7.2,1x,f3.1,a1)
112    format (a2,a5,a8,i2,i2,f5.2,1x,f5.1,   2x,f6.2,1x,
     + f5.1,f4.1,f7.2)
113    format (a2,5x,a1,1x,f4.1,f7.2,1x,a1,1x,f4.1,f7.2,
     + 1x,a1,1x,f4.1,f7.2,1x,a3,1x,f3.1,a1,3x)
!114    format (a2,5x,a8,f9.2,1x,a8,f9.2,1x,a8,f9.2)
!115   format (a2,5x,a2,f5.1,a1,f9.2,1x,a2,f5.1,a1,
!     + f9.2,1x,a2,f5.1,a1,f9.2)
114    format (a2, 5x,a8,i2,i2,f5.2)
115    format (a2,23x,a8,i2,i2,f5.2)
116    format (a2,41x,a8,i2,i2,f5.2)

      stop
      end
!_________________________________________________________________
!_________________________________________________________________

*
* $Id: lnblnk.F,v 1.2 2002/10/14 14:57:23 hristov dead $
*
* $Log: lnblnk.F,v $
* Revision 1.2  2002/10/14 14:57:23  hristov
* Merging the VirtualMC branch to the main development branch (HEAD)
*
* Revision 1.1  1999/05/18 15:55:30  fca
* Initial revision
*
* Revision 1.1.1.1  1996/02/15 17:50:38  mclareni
* Kernlib
*
*
!#include "kerngen/pilot.h"
      FUNCTION LNBLNK (CHV)
C
C CERN PROGLIB# M432    LNBLNK          .VERSION KERNFOR  4.21  890323
C ORIG. 04/10/88, JZ
C
C-    Find last non-blank character in CHV

      CHARACTER    CHV*(*)

      N = LEN(CHV)

      DO 17  JJ= N,1,-1
      IF (CHV(JJ:JJ).NE.' ') GO TO 99
   17 CONTINUE
      JJ = 0

   99 LNBLNK = JJ
      RETURN
      END
!_________________________________________________________________
!_________________________________________________________________

