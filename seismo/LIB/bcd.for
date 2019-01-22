      subroutine bcd(bcd_byte,number)
c
c input one byte bcd coded byte,bcd_byte and return the
c corresponding 2 digit integer number
c
c changes:
c   sep 30, 99 lo: test under linux
c
c
      implicit none
      character*1 bcd_byte
      character*2 bcd2     ! input byte plus null byte
      integer number
      integer*2 bcd_number
      integer i1,i2
      logical sun,pc,linux  ! computer type
      equivalence (bcd2,bcd_number)
c
      call computer_type(sun,pc,linux)
c
c   must put one byte into 2 to be able to get the unsigned integer from
c   then highest 4 bits, swap if on sun
c
      if(pc.or.linux) then
         bcd2(1:1)=bcd_byte
         bcd2(2:2)=char(0)
      elseif (sun) then
         bcd2(2:2)=bcd_byte
         bcd2(1:1)=char(0)
      endif

c
      i2=bcd_number/16       ! get highest digit
      i1=bcd_number - 16*i2  ! get lowest digit
      number=i1+i2*10

c
      return
      end

