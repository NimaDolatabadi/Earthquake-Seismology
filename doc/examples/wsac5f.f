	program wsac5f
	implicit none
	
	integer NCOMP
	parameter(NCOMP=11)

	integer NDATA
	parameter(NDATA=4000)

	real sdata(NDATA,NCOMP+1), xdummy(NDATA)
	CHARACTER KNAME(NCOMP+1)*10
	real evla, evlo, stla, stlo
	character*11 kevnm, kstnm 
	real b, delta
	real cmpaz, cmpinc
	integer npts
	integer nerr, j, i

	DATA KNAME/'STAZ','STBZ','STCZ','STDZ','STEZ',
     &           'STFZ','STGZ','STHZ','STHN','STHE','STHN','STNQ' /	

	b      = 0.0
	delta  = 0.25
	cmpaz  = 0.0
	cmpinc = 0.0
	npts  = NDATA
	evla   = -23.56
	evlo   = 123.56
	
	call newhdr () ;
	call setihv("IFTYPE", "ITIME", nerr)
	call setihv("IZTYPE", "IB",    nerr)
	call setfhv("B",      b,       nerr)
	call setlhv("LEVEN",  .TRUE.,  nerr)
	call setfhv("DELTA",  delta,   nerr)
  
	kevnm = "Event Name"
  
	call setnhv("NPTS",   npts,  nerr)
	call setfhv("EVLA",   evla,   nerr)
	call setfhv("EVLO",   evlo,   nerr)
	call setkhv("KEVNM",  kevnm,  nerr)
	call setfhv("CMPAZ",  cmpaz,  nerr)
	call setfhv("CMPINC", cmpinc, nerr)

	do j = 1,NCOMP-2
	   kstnm = kname(j)  
	   call setkhv ( "KSTNM", kstnm, nerr)
	   stla = j * 10
	   stlo = j * 20
	   do i = 1,NDATA
	      sdata(i,j) = 1.0 * rand()
	   enddo
	   call setfhv ( "STLA" , stla , nerr )
	   call setfhv ( "STLO" , stlo , nerr )
	   call wsac0 ( kstnm, xdummy, sdata(1,j), nerr)
	enddo
  
	cmpinc = 90.0
	call setfhv("CMPINC", cmpinc, nerr) 
	j = 9
	do i = 1,NDATA
	   sdata(i,j) = 1.0 * rand()
	enddo
	call wsac0(kname(9), xdummy, sdata(1,9), nerr)
  
	cmpaz = 90.0
	call setfhv("CMPAZ", cmpaz, nerr) 
	j = 10
	do i = 1,NDATA
	   sdata(i,j) = 1.0 * rand()
	enddo
	call wsac0(kname(10), xdummy, sdata(1,10), nerr)
	
	end
