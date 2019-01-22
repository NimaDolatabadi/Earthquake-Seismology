c   The "j" parameters (1 st line) are intended to be user settable:
c        jsrc   Maximum number of discrete model slowness samples above
c               the maximum source depth of interest.
c        jseg   Maximum number of different types of travel-times
c               considered.
c        jbrn   Maximum number of different travel-time branches to be
c               searched.
c        jout   Maximum length of all travel-time branches strung
c               together.
c        jtsm   Maximum length of the tau depth increments.
c 
c   The "j" parameters (1 st line) are intended to be user settable:
c        jsrc   Maximum number of discrete model slowness samples above
c               the maximum source depth of interest.
c        jseg   Maximum number of different types of travel-times
c               considered.
c        jbrn   Maximum number of different travel-time branches to be
c               searched.
c        jout   Maximum length of all travel-time branches strung
c               together.
c        jtsm   Maximum length of the tau depth increments.
c        jxsm   Maximum number of x-values needed for the depth
c               increments.
c        jbrnu  Maximum length of the up-going branches.
c        jbrna  Maximum length of branches which may need
c               re-interpolation.
c
      parameter(jsrc=150,jseg=30,jbrn=100,jout=2500,jtsm=350,jxsm=jbrn,
     1 jbrnu=jbrn,jbrna=jbrn)
      parameter(jmod=160,jslo=350,jdep=450,jbrh=20,jlvz=5)
c   The parameters actually used are all derivatives of the "j"
c   parameters and cannot be changed by the user.
      parameter(nmd0=jmod,nsl1=jslo+1,ndp1=jdep+1,nsr0=jsrc)
      parameter(nbr1=jbrh+2,nbr2=jbrh*2,ncp0=2*(jbrh+jlvz),nlvz0=jlvz)
c   A few derived parameters are also needed.
      parameter(jrec=jtsm+jxsm,jtsm0=jtsm+1)
