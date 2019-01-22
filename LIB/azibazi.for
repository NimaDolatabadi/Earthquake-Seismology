      subroutine azibazi(alats,alongs,alatr,alongr,delta,azim,bazim)
c
c
c   changes
c
c   sep 98 by jh ----------- version 7.0 check ------------------
c                no changes
c   feb 12, 2008, jh: check for range of acos
c    
c   INPUT:  alats, alongs: latitude and longitude of the source
c           alatr, alongr: latitude and longitude of the receiver
c   OUTPUT: delta: the epicentral distance
c           azim : the azimuth at the source
c           bazim: the back-azimuth at the station
c   Note: all the elements are angles given in degrees. 
c         this subroutine does not account for the Earth's ellipticity
c
c   There is no garantee that the subroutine will work in absolutely all
c   cases, but it has been tested and should work across the line of change
c   of day for example.
c   December 1994. Valerie Maupin.
c
      degpi = 180./3.14159
c
      alon = alongr-alongs
      if (alon.lt.-180.) alon=alon+360.
      if (alon.gt.180.) alon=alon-360.
      if (alon.ge.0.)  then
        c = alon/degpi
        aa = (90. - alats)/degpi
        bb = (90. - alatr)/degpi
      else
        c = -alon/degpi
        aa = (90. - alatr)/degpi
        bb = (90. - alats)/degpi
      endif
      cc = cos(aa)*cos(bb) + sin(aa)*sin(bb)*cos(c)
      if(cc.gt.1.0) then
        write(6,*) 'acos',cc
        cc=1.0
      endif
      if(cc.lt.-1.0) then
        write(6,*) 'acos',cc
        cc=-1.0
      endif
 
      cc = acos(cc)
      a = (cos(aa) - cos(bb)*cos(cc))/(sin(bb)*sin(cc))
      if(a.gt.1.0) then
        write(6,*) 'acos',a
        a=1.0
      endif
      if(a.lt.-1.0) then
        write(6,*) 'acos',a
        a=-1.0
      endif
      a = acos(a)
      b = (cos(bb) - cos(aa)*cos(cc))/(sin(aa)*sin(cc))
      if(b.gt.1.0) then
        write(6,*) 'acos',b
        b=1.0
      endif
      if(b.lt.-1.0) then
        write(6,*) 'acos',b
        b=-1.0
      endif
      b = acos(b)
c
c ... calculate the epicentral distance 
c                 you must choose between degrees or kilometers 
c                 and comment out one of these two lines 
      delta = cc*degpi
c     delta = cc*6371. 
c
c ... calculate the azimut and backazimuth
      if (alon.ge.0.)   then
        azim = b*degpi
        bazim = 360. - a*degpi
      else
        azim = 360. - a*degpi
        bazim = b*degpi
      endif
c
c.......calculate the coordinates of the mid-point
c                          (if this element is of interest to you,
c                          add (alat,along) in the argument list)
c
      cc=cc/2.
      dd = cos(aa)*cos(cc) + sin(aa)*sin(cc)*cos(b)
      if(dd.gt.1.0) then
        write(6,*) 'acos',dd
        dd=1.0
      endif
      if(dd.lt.-1.0) then
        write(6,*) 'acos',dd
        dd=-1.0
      endif
      dd = acos(dd)
      alat= 90. - dd*degpi
      c3 = (sin(bb)*cos(cc)- sin(cc)*cos(bb)*cos(a))/sin(dd)
      if(c3.gt.1.0) then
        write(6,*) 'acos',c3
        c3=1.0
      endif
      if(c3.lt.-1.0) then
        write(6,*) 'acos',c3
        c3=-1.0
      endif
      c3 = acos(c3)
      if (alon.ge.0.)  along = alongr - c3*degpi
      if (alon.lt.0.)  along = alongs - c3*degpi
      if (along.lt.-180.) along=along+360.
      if (along.gt.180.) along=along-360.
c
      return 
      end

