c################################################################               
c                                                                               
      subroutine abstim(timestring,fstart,time)
c                                                                               
c   converts a data base timestring to to abs time, time,                       
c   from 1900. if fstart ne 0, the time string is the data                      
c   base file name, else a simple time string                                   
c                                                                               
c   oct 91 by j. havskov                                                        
c                                                                               
c   updates:     
c   aug 15 92 by jh : bgs type                                                               
c   jan 98    by jh : year 2000
c   aug 98          : --------- version 7.0 check --------------
c                   : long file names
c   2010-01-12 pv   : removed a few tabulators
c             
      implicit none
      character*(*) timestring
      double precision time
      integer fstart
      integer year,month,day,hour,min,isec
      real sec
c                                                                               
c   read time                                                                   
c
       if(fstart.eq.0) then
         read(timestring(1:14),
     *   '(i4,5i2)') year,month,day,hour,min,isec
       else
           read(timestring(fstart:fstart+19),
     *    '(i2,1x,2i2,1x,i2,3x,i4,i2)')
     *    day,hour,min,isec,year,month
       endif
       sec=isec
c                                                                               
c   convert                                                                     
c                                                                               
      call timsec(year,month,day,hour,min,sec,time)
      return
      end
