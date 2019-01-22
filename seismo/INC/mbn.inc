c
c include file for mb pn/sn computation
c
c mbpn = log10(A) - A log 10 (B/Δ) [km] + S + E + C
c
      integer maxstat            ! number of stations max
      parameter(maxstat=99)

      real mbpnscale(3) ! scale parameters pn
      real mbsnscale(3) ! scale parameters sn
      real mbpn_gv(2),mbsn_gv(2) ! group velocity windows for p and s
      real mbn_s(maxstat,2)      ! station corrections P and S
      integer mbn_nstat          ! number of stations with correction
      integer mbn_nsource        ! number of source areas with correction
      character*5 mbn_stations(maxstat)    ! station codes for corrections
      character*80 mbn_source_name(20)     ! name of source areas
      real mbn_e(20,6)           ! source correction area and P and S values
      real mbpn(maxstat),mbsn(maxstat) ! station magnitudes
      integer nmbpn,nmbsn,nmbn   ! number of station magnitudes
      real mbnmed,mbnmedsd       ! median magnitude
      real mbnav,mbnavsd         ! average magnitude
      real mbn(maxstat)          ! combined pn and sn

      common /mbn1/mbn_source_name,mbn_stations
      common /mbn2/mbpnscale,mbsnscale,mbpn_gv,mbsn_gv,
     &        mbn_s,mbn_e,mbn_nstat,mbn_nsource,
     &        mbpn,mbsn,mbnav,mbnavsd,mbnmed,mbnmedsd
    
