c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine component(incomp,outcomp)
c
c     Routine to convert from component notations of datafile to
c     notations as used in S files
c
c     Written by C. Lindholm,  Oct. 1989
c     update
c     oct 8 93 by jh : include BV components
c     jan 4 94       : include SV ---------
c     mar 95         : include transverse and radial
c     oct 95         : include B  Z etc
c     jan 8          : include LV Z etc
c     april 5, 98    : include LH and SH
c     sep 98         : ----------------- version 7.0 check ------------
c                      always return a 2 component code, even if not defined
c                      then use first and last letter fo input component
c
c     Input:  incomp   :  Datafile type component
c     Output: outcomp  :  S file type component
c
      implicit  none
      character*4 incomp
      character*2 outcomp

      outcomp = '  '
      if(incomp.eq.'S  Z')outcomp = 'SZ'
      if(incomp.eq.'S  N')outcomp = 'SN'
      if(incomp.eq.'S  E')outcomp = 'SE'
      if(incomp.eq.'L  Z')outcomp = 'LZ'
      if(incomp.eq.'L  N')outcomp = 'LN'
      if(incomp.eq.'L  E')outcomp = 'LE'
      if(incomp.eq.'AL Z')outcomp = 'AZ'
      if(incomp.eq.'AL N')outcomp = 'AN'
      if(incomp.eq.'AL E')outcomp = 'AE'
      if(incomp.eq.'AH Z')outcomp = 'AZ'
      if(incomp.eq.'AH N')outcomp = 'AN'
      if(incomp.eq.'AH E')outcomp = 'AE'
      if(incomp.eq.'A  Z')outcomp = 'AZ'
      if(incomp.eq.'A  N')outcomp = 'AN'
      if(incomp.eq.'A  E')outcomp = 'AE'
      if(incomp.eq.'BV Z')outcomp = 'BZ'
      if(incomp.eq.'BV N')outcomp = 'BN'
      if(incomp.eq.'BV E')outcomp = 'BE'
      if(incomp.eq.'BH Z')outcomp = 'BZ'
      if(incomp.eq.'BH N')outcomp = 'BN'
      if(incomp.eq.'BH E')outcomp = 'BE'
      if(incomp.eq.'B  Z')outcomp = 'BZ'
      if(incomp.eq.'B  N')outcomp = 'BN'
      if(incomp.eq.'B  E')outcomp = 'BE'
      if(incomp.eq.'SV Z')outcomp = 'SZ'
      if(incomp.eq.'SV N')outcomp = 'SN'
      if(incomp.eq.'SV E')outcomp = 'SE'
      if(incomp.eq.'SH Z')outcomp = 'SZ'
      if(incomp.eq.'SH N')outcomp = 'SN'
      if(incomp.eq.'SH E')outcomp = 'SE'
      if(incomp.eq.'LV Z')outcomp = 'VZ'
      if(incomp.eq.'LV N')outcomp = 'VN'
      if(incomp.eq.'LV E')outcomp = 'VE'
c
c  rotated components
c
      if(incomp.eq.'S  R')outcomp = 'SR'
      if(incomp.eq.'S  T')outcomp = 'ST'
      if(incomp.eq.'L  R')outcomp = 'LR'
      if(incomp.eq.'L  T')outcomp = 'LT'
      if(incomp.eq.'AL R')outcomp = 'AR'
      if(incomp.eq.'AL T')outcomp = 'AT'
      if(incomp.eq.'AH R')outcomp = 'AR'
      if(incomp.eq.'AH T')outcomp = 'AT'
      if(incomp.eq.'A  R')outcomp = 'AR'
      if(incomp.eq.'A  T')outcomp = 'AT'
      if(incomp.eq.'BV R')outcomp = 'BR'
      if(incomp.eq.'BV T')outcomp = 'BT'
      if(incomp.eq.'SV R')outcomp = 'SR'
      if(incomp.eq.'SV T')outcomp = 'ST'
c
c   if no component is recognized, use first and last letter of
c   component
c
      if(outcomp.eq.'  ') then
         outcomp(1:1)=incomp(1:1)
         outcomp(2:2)=incomp(4:4)
      endif
      return
      end

