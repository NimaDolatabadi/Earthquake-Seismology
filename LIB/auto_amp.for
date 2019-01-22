      subroutine auto_amp(y,n,rate,fmin,fmax,maxcros,
     *yfirst,ylast,ifirst,ilast)
c
c   Determine maximum peak to peak amplitude and corresponding period
c   It is assumed that DC has been removed
c   First finds the absolute maximum in signal and then searches nearby
c   for the next max extreme assuming the maxcros condition 
c
c     input:  y:     data vector
c             n:     number of points in y
c             rate:  sample rate
c             fmin,fmax: frequency range to seach in
c             maxcros: max number of zero crossings between max and min amp
c
c     output: yfirst,ylast: extreme values found, can be positive or negative
c             ifirst,ilast: sample number of first and last extreme
c                           if ifirst is zero, no valid reading
c
c     j. havskov, december 2000
c
c
      implicit none
      real y(*)
      real yfirst,ylast
      real rate,fmin,fmax,ymin,ymax
      integer n,ifirst,ilast,maxcros
      real min_value,max_value         ! help variables
      integer n1,n2                    ! range of samples corresponding to
c                                        frequency band
      integer max_sign                 ! sign of max_value
      integer i,i1,i2,k                ! counters

      ifirst=0
      if(maxcros.eq.0) maxcros=1       ! at least one crossing
c
c   calculate sample range corresponding to frequency range, remember
c   range is only half the period
c
      
      if(fmax.eq.0.0) then
         n1=1                ! highest possible frequency
      else
         n1=rate/(fmax*2)
         if(n1.eq.0) n1=1
      endif
      if(fmin.eq.0) then
         n2=n
      else
         n2=rate/(fmin*2)
         if(n2.eq.0) n2=n
      endif

c
c  find largest absolute extreme
c
       max_value=0

c
       do i=1,n
          if(abs(y(i)).gt.abs(max_value)) then
             max_value=y(i)
             ifirst=i
          endif
       enddo
       max_sign=abs(max_value)/max_value
c
c   search around the extreme, extreme must have opposite sign of the previous
c   extreme
c

c
c   seach above in time
c

c
c   range acording to frequency band
c
       i1=ifirst+n1
       if(i1.gt.n) i1=n
       i2=ifirst+n2
       if(i2.gt.n) i2=n
c
c   find limits set by zero crossings
c
       k=0
       do i=i1,i2
          if((y(i-1).gt.0.0.and.y(i).le.0.0).or.
     *       (y(i-1).lt.0.0.and.y(i).gt.0.0)) then
                 k=k+1
                 if(k.gt.maxcros) then
                    i2=i
                    goto 10
                 endif
          endif
       enddo
 10    continue

       if(i1.eq.i2) then
          ifirst=0
          return
       endif
c
       min_value=0
       do i=i1,i2
          if((max_sign.gt.0.and.y(i).lt.0).or.
     *    (max_sign.lt.0.and.y(i).gt.0)) then
          if(abs(y(i)).gt.abs(min_value)) then
             min_value=y(i)
             ilast=i
          endif
          endif
       enddo
c
c   search below in time
c

       i1=ifirst-n1
       if(i1.le.0) i1=1
       i2=ifirst-n2
       if(i2.lt.0) i2=1
c
c   find limits set by zero crossings
c
       k=0
       do i=i1,i2,-1
          if((y(i+1).gt.0.0.and.y(i).le.0.0).or.
     *       (y(i+1).lt.0.0.and.y(i).ge.0.0)) then
                 k=k+1
                 if(k.gt.maxcros) then
                    i2=i
                    goto 11
                 endif
          endif
       enddo
 11    continue
       if(i1.eq.i2) then
          ifirst=0
          return
       endif
       
c
       do i=i2,i1
          if((max_sign.gt.0.and.y(i).lt.0).or.
     *    (max_sign.lt.0.and.y(i).gt.0)) then
          if(abs(y(i)).gt.abs(min_value)) then
             min_value=y(i)
             ilast=i
          endif
          endif
       enddo

c
c   find which one is first and last
c
       if(ifirst.gt.ilast) then
          i=ifirst
          ifirst=ilast
          ilast=i
          yfirst=min_value
          ylast=max_value
       else
          yfirst=max_value
          ylast=min_value
       endif
c
c   check if value is reasonable
c
       if(ifirst.eq.1.or.ifirst.eq.n.or.ilast.eq.1.or.ilast.eq.n)
     * ifirst=0

       return
       end



c
c ###########################################################################
c
      subroutine auto_amp2(y,n,rate,fmin,fmax,maxcros,
     *yfirst,ylast,ifirst,ilast)
c
c   Determine maximum peak to peak amplitude and corresponding period
c   DC does not need to be removed
c   Finds the maximum difference between two consecutive extremes
c   Input is the same as for auto_amp subroutine, maxcros used differently.
c
c     input:  y:     data vector
c             n:     number of points in y
c             rate:  sample rate
c             fmin,fmax: frequency range to search in
c             maxcros: max number of non-consecutive extremes
c             maxcros > 1 allows to search for non-consecutive extremes as well
c             maxcros=2...comparing 1st and 4th extreme in addit. to consec.ext
c             maxcros=3...comparing 1st and 6th extreme in addition to previous
c
c     output: yfirst,ylast: extreme values found, can be positive or negative
c             ifirst,ilast: sample number of first and last extreme
c                           if ifirst is zero, no valid reading
c
c     Jan Michalek, January 2016
c
c
      implicit none
      real y(*)
      real yfirst,ylast
      real rate,fmin,fmax,ymin,ymax
      integer n,maxcros,ifirst,ilast
      real max_value                   ! help variables
      integer n1,n2                    ! range of samples corresponding to
c                                        frequency band
      integer i,k,l,m                  ! counters
      real dy(n)                       ! difference vector
      integer di(n)                    ! difference index 
      real loc_ext(n)                  ! local extreme vector
      real p2p(n)                      ! peak-to-peak
      
      ifirst=0
      if(maxcros.eq.0) maxcros=1       ! consecutive extremes only

c
c find all local extremes 
c     
      do i=1,n-1
         dy(i)=y(i+1)-y(i)             ! difference
      enddo 
      k=0
      do i=1,n-2
         if(dy(i).gt.0.0.and.dy(i+1).lt.0.0.or.
     *      dy(i).lt.0.0.and.dy(i+1).gt.0.0) then
             k=k+1
             loc_ext(k)=y(i+1)           ! all local extremes
             di(k)=i+1                   ! position
         endif
      enddo

c
c   find the highest difference
c
       max_value=0       
       do l=1,maxcros
         do i=1,k-(l*2-1)
           m=i+(l*2)-1
           p2p(i)=abs(loc_ext(i)-loc_ext(m)) 
           if(p2p(i).gt.max_value) then
             max_value=p2p(i)
             yfirst=loc_ext(i)
             ifirst=di(i)
             ylast=loc_ext(m)
             ilast=di(m)
             if(i.gt.1.or.i.lt.n) then
              ! despike
               call despike(y(ifirst-1),y(ifirst),y(ifirst+1))
               call despike(y(ilast-1),y(ilast),y(ilast+1))
             endif
           endif
         enddo        
       enddo

c
c   check if value is reasonable
c
       if(ifirst.eq.1.or.ifirst.eq.n.or.ilast.eq.1.or.ilast.eq.n)
     * ifirst=0
  
       return
       end



c ###########################################################################
c   DESPIKE - remove spikes from signal
c 
      subroutine despike(x1,x2,x3)
c     Checks for spike at x2 position; in case that x2 is
c     n-times larger than average from surrounding points
c     the x2 value is substituted by avg(x1,x2)
c     and a warning is printed
c
c     Jan Michalek, January 2016
   
      implicit none
      real x1,x2,x3
      real avg,n                  ! variables
      
      n=5.0
      avg=(abs(x1)+abs(x3))/2.0      
      if(abs(x2).gt.avg*n) then
        write(6,'(a,f12.2)')'::auto_amp:   spike:',x2
        x2=avg
        write(6,'(a,f12.2)')'::auto_amp:despiked:',x2
      endif
      return
      end





