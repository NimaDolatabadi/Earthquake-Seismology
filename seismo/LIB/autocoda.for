c
c   automatic coda determination by lo, 1998
c
c
c  updates
c
c  sep 98 by jh  : ---------- version 7.0 check ---------------------
c                  remove test output to 99
c
      subroutine auto_coda 
      implicit none
      include 'mulplt.inc'

      integer c                                   ! counter
      real p_time
      integer p_sample
      real lta,slta
 
      integer l_sta
      real sta,ssta,old_sta

c      open(99,file='junk',status='unknown')

c      write(99,*) 'coda filter low ',coda_l
c      write(99,*) 'coda filter high ',coda_h
c      write(99,*) 'coda ratio ',coda_ratio
c      write(99,*) 'coda sta ',coda_sta

c      auto_coda_time = numb_samp / rate /2
      auto_coda_time = -1

      p_time = 9999999
c
c  check for p phase and get start time relative to trace start
c
      
      do c=1,nphas
        if (phase(c)(2:2).eq.'P') then
 
          if (pictim(c).lt.p_time) p_time = pictim(c)
 
        endif
      enddo

      if (p_time.eq.9999999) goto 999

c
c calculate sample of P-phase
c
      p_sample= p_time * rate

c
c  take complete trace and filter
c
      filt = 8
      flow(8) = coda_l
      fhigh(8) = coda_h

      call trans_filt(1)

c
c get LTA
c
      slta=0
      do c=1,p_sample-1

        slta = slta + y(c) * y(c)
      enddo

      lta= slta / (p_sample-1)

c      write(99,*) 'lta ',lta

c
c  length of coda_sta in samples
c
      l_sta=abs(coda_sta*rate)

      ssta=0

      do c=p_sample,p_sample+l_sta
        ssta=ssta + y(c) * y(c)
      enddo

c
c  get start sta
c
      old_sta = ssta / l_sta

      c=p_sample+l_sta+1     ! start from coda_sta
 100  continue

      c=c+1

      sta= (y(c) * y(c) - y(c-l_sta) * y(c-l_sta))
     &     / l_sta    + old_sta

      old_sta = sta

c      write(99,*) 'c ',c,' sta ',sta
c
c  get ratio of STA/LTA
c

      if (sta/lta.lt.coda_ratio) then

        auto_coda_time = c / rate
        goto 999
      endif


      if (c.eq.numb_samp) goto 999

      goto 100

 999  continue

c      write(99,*) 'auto_coda_time ',auto_coda_time

c      close(99)


      return
      end
