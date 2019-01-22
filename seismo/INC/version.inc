
c
c   updates
c
c  march 29 2006 jh  incease version_text to 12
c
      character*12 version_text
      character*20 version_date,out_version_date
      logical version_new
      parameter(version_text='SEISAN 11.0') ! also sets dislin text
      parameter(version_date='June  2018')
c
c   version_new : true  : always take date from include file
c                 false : take date from program
c
      parameter(version_new=.true.)

      common /verblock/out_version_date

