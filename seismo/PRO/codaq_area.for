c
c   reads codaq.area and sorts and average results in lat-lon bins
c   jh feb 2012
c
c   changes
c  mar 22 2015 jh: format change
c  jan 13 2016 jh: dimension 15000 to 10 in subroutine qzerO
c  jan 2  2018 jh: dim problem, make contour map with epimap, more options
c
      implicit none
      include 'seisan.inc'
      character*120 txt          ! general text
      integer seiclen            ! function
      character*60 top_directory 
      character*1 dchar          ! directory separation character
      integer ndim               ! dimension for grid
      parameter (ndim=50)
      integer nqgrid             ! max numkber of poitns in one cell
      parameter (nqgrid=1500)
      real g(ndim,ndim)          ! grid points
      real ff(10)                ! frequencies to use
      real xq(nqgrid)            ! q-values in one grid point at one frequency
      real xqsd(10)              ! sd of xq
      integer nnq(10)            ! number of q's in one grid point
      integer  nqq(ndim)         ! total number of q in on grid point



      integer nfreq              ! number of frequencies
      real f,freq                ! frequecy to use
      integer n_used             ! q values used
      character*80 text          
      integer nq(10,ndim,ndim)   ! number of q-values in each grid point and f
      integer nq_min             ! minimum number of values for average
      integer nq0_min            ! min frequecy points to calculate Q0
      real q(10,ndim,ndim,nqgrid)  ! q values in each grid point and f
      real q0(ndim,ndim)         ! q0 in each grid point
      real qmin,qmax             ! min and max q0 in grid

      real qq0(ndim,ndim)        ! ------------- for average
      real av
      integer nav

      real range, dlat,dlon      ! range to plot and plot grid size

      real qalpha(ndim,ndim)     ! qalpha in each grid point
      real lat,lon               ! lat  lon of observation point
      real q1,x1,x2              ! help variables
      real minlat,maxlat,dellat  ! grid
      real minlon,maxlon,dellon  ! grid
      integer nlat,nlon          ! number of points in grid 
      real cs                    ! constant ss (qalpha) value
      real crms                  ! rms when using a constant ss
      real sdcq                  ! standard deviation in qc
      real cq                    ! q0 whan using a constant ss
      real sdq0                  ! standard deviation in q0
      real ss         
      real sdss                  ! standard deviation in ss
      real corr                  ! correlation coefficient
      real rms                   ! rms error of fit
      integer number             ! if 1 print numbers
      integer extrapolate        ! if 1 extrapolate q0 or q10
      integer plot_q10           ! if 1, plot q10 instead of q0
      real x,xx                  ! help variables
      integer i,j,k,n,if,m

c
c   get seisan defaults
c
      call get_seisan_def

                                                                               
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)                                                
      call dir_char(dchar)   ! directory separation character
c   
      do if=1,10
      do i=1,ndim
        do k=1,ndim
           g(i,k)=0.0
           q0(i,k)=0.0
           qalpha(i,k)=0.0
           do m=1,ndim
              q(if,i,k,m)=0
           enddo
           nq(if,i,k)=0.0
        enddo
      enddo
      enddo
      n=0
      n_used=0
c
c   get frequencies
c
      open(1,file='codaq1.out',status='old',err=25)
      goto 26
 25   continue
      write(6,*)'codaq1.out must be present'
      stop
 26   continue      
      do i=1,100
         read(1,'(a)') text
         if(text(1:5).eq.' Freq') then
           backspace 1
           goto 1
         endif
      enddo
 1    continue
      read(1,'(7x,10f12.1)') ff
      close(1)
      nfreq=0
      do i=1,10
        if(ff(i).ne.0.0) nfreq=nfreq+1
      enddo 
      write(6,'(a,10f8.2)') 'Frequencies', (ff(i),i=1,nfreq)
c
c  get grid parameters from file, else from keyboard
c
      open(1,file='codaq_area.inp',status='old',err=2)
      read(1,*,end=11) text
      read(1,*,end=11) nq_min
      read(1,*,end=11) text
      read(1,*,end=11) nq0_min
      read(1,*,end=11) text
      read(1,*,end=11) cs
      read(1,*,end=11) text
      read(1,*,end=11) number
      read(1,*,end=11) text
      read(1,*,end=11) extrapolate
      read(1,*,end=11) text
      read(1,*,end=11) plot_q10
      read(1,*,end=11) text
      read(1,*,end=11) minlat,maxlat,dellat
      read(1,*,end=11) text
      read(1,*,end=11) minlon,maxlon,dellon
      close(1)
      goto 12
 11   continue
      write(6,*)'End of file for codaq_area.inp'
      stop
 12   continue

      if(nq_min.eq.0) nq_min=1
      if(nq0_min.lt.2) nq0_min=2
c
c  check if not too many
c
      nlat=(maxlat-minlat+0.01)/dellat
      if(nlat.gt.ndim) then
         write(6,*) 'Too large grid'
         stop  
      endif

      nlon=(maxlon-minlon+0.01)/dellon
      if(nlon.gt.ndim) then
         write(6,*) 'Too large grid'
         stop
      endif

      write(6,*) 
     *'Min no for grid average, min no of freq for Q0 calculation',
     * nq_min,nq0_min
       write(6,*)'Constant in Q-relation, print number of observations'
     * ,cs, number
       write(6,*)'Extrapolate, plot Q0',extrapolate, 
     * plot_q10

      if(nq_min.eq.0) nq_min=1
      if(nq0_min.lt.2) nq0_min=2

      write(6,*) 'Lat range and delta', minlat,maxlat,dellat
      write(6,*) 'Lon range and delta', minlon,maxlon,dellon
      goto 9 

c
c  input form keyboard
c
 2    continue
      
      write(6,*)
     *'Min no for grid avearge, min no of freq for Q0 calculation'
      read(5,*) nq_min,nq0_min
      write(6,*)
     *'Constant qalpha in Q=Q0*f**qalpha relation, 0 means do not use'
      read(5,*) cs
      write(6,*)'Print numbers of observations for each Q0: 1 else 0'
      read(5,*) number
      write(6,*)'Extrapolate q0 or q10 for plot, 0=no, 1=yes'
      read(5,*) extrapolate
      write(6,*)'Plot Q0 (0) or q10 (1)'
      read(5,*) plot_q10
c
c   grid to use
c
 5    continue
      write(6,*) 'Latitude range and grid size'
      read(5,*) minlat,maxlat,dellat
c
c  check if not too many
c
      nlat=(maxlat-minlat+0.01)/dellat
      if(nlat.gt.ndim) then
         write(6,*) 'Too large grid'
         goto 5
      endif

 6    continue
      write(6,*) 'Longitude range and grid size'
      read(5,*) minlon,maxlon,dellon

      nlon=(maxlon-minlon+0.01)/dellon
      if(nlon.gt.ndim) then
         write(6,*) 'Too large grid'
         goto 6
      endif

 9    continue
      open(1,file='codaq.area',status='old') 
c
c  start reading file
c
 10   continue
      read(1,'(25x,4f7.1)',end=20) lat,lon,f,q1
c
c   find which index for frequency
c
      do i=1,nfreq
        if(f.eq.ff(i)) if=i
      enddo

c
c   find which gridpoint it belongs to, first initialize pointer
c
         k=0
         j=0
         do i=1,nlat
            x1=minlat+(i-1)*dellat
            x2=x1+dellat
            if(lat.ge.x1.and.lat.lt.x2)k=i
         enddo
         do i=1,nlon
            x1=minlon+(i-1)*dellon
            x2=x1+dellon
            if(lon.ge.x1.and.lon.lt.x2)j=i
         enddo
         n=n+1
c
c   only use if in grid
c
         if(k.gt.0.and.j.gt.0) then
            nq(if,k,j)=nq(if,k,j)+1
            if(nq(if,k,j).gt.nqgrid) then
                 write(6,*) 'Number of grid values:',nq(if,k,j) 
                 write(6,*)
     *          'Too many Q values in one grid, max is ',nqgrid            
                stop
            endif
            q(if,k,j,nq(if,k,j))=1.0/q1  ! store inverse values for average
            n_used=n_used+1
         endif
c
c   go for next 
c
      goto 10

c
c   no more data
c
 20   continue
      write(6,*)'Number of q-data in input file',n
      write(6,*)'Number of q-data inside grid  ',n_used
c
c   average 1/q values 
c
      do if=1,nfreq
         do i=1,nlat
           do j=1,nlon
              if(nq(if,i,j).ge.nq_min)then
                  do m=1,nq(if,i,j)
                      xq(m)=q(if,i,j,m)  ! still 1/q
                  enddo
c                  write(17,*) if,i,j,(1.0/xq(m),m=1,nq(if,i,j))
c
c   put av in index 1 and sd in index 2
c
                  call sdv(nq(if,i,j),xq,q(if,i,j,1),
     *            q(if,i,j,2))

                  q(if,i,j,2)=
     *            q(if,i,j,2)/(q(if,i,j,1)*q(if,i,j,1))  ! since 1/values
c
                  q(if,i,j,1)=1.0/q(if,i,j,1)     ! back to q
              else
                  q(if,i,j,1)=0.0
                  q(if,i,j,2)=0.0
              endif
           enddo
         enddo
      enddo
c

c
c   calculate q0 and qalpha each grid point
c
      do i=1,nlat
        do j=1,nlon
c
c  transfer to variables for fit
c
           k=0 
           do if=1,nfreq
             xq(if)= q(if,i,j,1)
             xqsd(if)=q(if,i,j,2)
             nnq(if)= nq(if,i,j)
             if(nnq(if).gt.0) k=k+1  ! count how many frequencies have data
           enddo
c
c   at least nq0_min frequencies must have data
c

           if(k.ge.nq0_min) then
              call qzero
     *        (nfreq,ff,xq,nnq,xqsd,cs,cq,crms,sdcq,q0(i,j),sdq0,
     *        qalpha(i,j),sdss,corr,rms)
           endif
c
c   use fixed qalpha (=cs) if cs gt 0
c
           if(cs.gt.0.0) q0(i,j)=cq
           cq=0.0
        enddo
      enddo
c
c   write to files
c
      open(2,file='codaq_area.out',status='unknown')
      open(3,file='codaq_grid.out',status='unknown')
c
      do if=1,nfreq
         write(2,*)'freq=',ff(if)
         write(3,*)'freq=',ff(if)
         write(2,'(6x,100f6.1)')
     *   (minlon+(i-1)*dellon+dellon/2.0,i=1,nlon)
         do i=nlat,1,-1
           write(2,'(f6.1,100i6)')
     *     minlat+(i-1)*dellat+dellat/2.0,(int(q(if,i,j,1)),
     *     j=1,nlon)
         enddo
         do i=1,nlat
            do j=1,nlon
               write(3,'(2f10.3,2f10.1,i10)') 
     *         minlat+(i-1)*dellat+dellat/2.0,
     *         minlon+(j-1)*dellon+dellon/2.0,q(if,i,j,1),
     *         q(if,i,j,2),nq(if,i,j)
            enddo
         enddo
      enddo
c
c    write Q0 values
c
     
      write(6,*)
      write(2,*)'Q0'
      write(3,*)'Q0'
         write(2,'(6x,100f6.1)')(minlon+(i-1)*
     *   dellon+dellon/2.0,i=1,nlon)
         do i=nlat,1,-1
           write(2,'(f6.1,100i6)')
     *     minlat+(i-1)*dellat+dellat/2.0,
     *     (int(q0(i,j)),j=1,nlon)
c
c   number of observatios
c
           do j=1,nlon
              k=0
c
c   count all observations for that cell and possibly print
c
              do if=1,nfreq
                 if(nq(if,i,j).gt.0) k=k+nq(if,i,j)
              enddo
              nqq(j)=k
           enddo
           if(number.eq.1) write(2,'(6x,100i6)')(-nqq(j),j=1,nlon)
         enddo


         do i=1,nlat
            do j=1,nlon
               write(3,'(2f10.3,f10.1)')
     *         minlat+(i-1)*dellat+dellat/2.0,
     *         minlon+(j-1)*dellon+dellon/2.0,q0(i,j)
            enddo
         enddo

c
c   write Q10 
c
     
      write(6,*)
      write(2,*)'Q10'
      write(3,*)'Q10'
         write(2,'(6x,100f6.1)')(minlon+(i-1)*
     *   dellon+dellon/2.0,i=1,nlon)
         do i=nlat,1,-1
           write(2,'(f6.1,100i6)')
     *     minlat+(i-1)*dellat+dellat/2.0,
     *     (int(q0(i,j)*10.0**qalpha(i,j)),j=1,nlon)
         enddo

         do i=1,nlat
            do j=1,nlon
               write(3,'(2f10.3,f10.1)')
     *         minlat+(i-1)*dellat+dellat/2.0,
     *         minlon+(j-1)*dellon+dellon/2.0,q0(i,j)
     *         *10.0**qalpha(i,j)
            enddo
         enddo
c
c   save in Q0 if that is what should be plotted
c
         if(plot_q10.eq.1) then

            do i=1,nlat
               do j=1,nlon
                  q0(i,j)=q0(i,j)*10.0**qalpha(i,j)
               enddo
            enddo
         endif

         qmin=10000
         qmax=0

         do i=1,nlat
            do j=1,nlon
c
c   find min and max value
c
               if(q0(i,j).ne.0.0) then
                  if(q0(i,j).lt.qmin)qmin=q0(i,j)
                  if(q0(i,j).gt.qmax)qmax=q0(i,j)
               endif
            enddo
         enddo

c
c   put a value in each point by averaging with points around
c   if not jump to 300
c
         if(extrapolate.eq.0) goto 300
 
 200     continue
         do i=1,nlat
            do j=1,nlon
               qq0(i,j)=0.0
               if(q0(i,j).eq.0.0) then
                 av=0.0
                 nav=0
                 if(i-1.gt.0) then
                    if(q0(i-1,j).gt.0.0) then
                       av=av+q0(i-1,j)
                       nav=nav+1
                    endif
                 endif
                 if(i+1.le.nlat) then
                    if(q0(i+1,j).gt.0.0) then
                       av=av+q0(i+1,j)
                       nav=nav+1
                    endif
                 endif
                 if(j-1.gt.0) then
                    if(q0(i,j-1).gt.0.0) then
                       av=av+q0(i,j-1)
                       nav=nav+1
                    endif
                 endif
                 if(j+1.le.nlon) then
                    if(q0(i,j+1).gt.0.0) then
                       av=av+q0(i,j+1)
                       nav=nav+1
                    endif
                 endif
                 if(nav.gt.0) then 
                    qq0(i,j)=av/nav
                 else
                    qq0(i,j)=0.0
                 endif
               else
                 qq0(i,j)=q0(i,j)
               endif               
            enddo
         enddo
c
c  back to original array
c
         do i=1,nlat
            do j=1,nlon
               q0(i,j)=qq0(i,j)
            enddo
         enddo
c
c   check if still zeros, then redo
c
         
         do i=1,nlat
            do j=1,nlon
               if(q0(i,j).eq.0.0) goto 200
            enddo
         enddo

c
c   write new values
c
         if(plot_q10.eq.0) write(2,*)'Q0 extrapolated'
         if(plot_q10.eq.1) write(2,*)'Q10 extrapolated'


         write(2,'(6x,100f6.1)')(minlon+(i-1)*
     *   dellon+dellon/2.0,i=1,nlon)
         do i=nlat,1,-1
           write(2,'(f6.1,100i6)')
     *     minlat+(i-1)*dellat+dellat/2.0,
     *     (int(q0(i,j)),j=1,nlon)
         enddo

c
c   jump here if no extrapolate
c
 300     continue 

      write(6,*)
      write(2,*)'Qalpha'
      write(3,*)'Qalpha'
         write(2,'(6x,100f6.1)')(minlon+(i-1)*dellon+
     *   dellon/2.0,i=1,nlon)
         do i=nlat,1,-1
           write(2,'(f6.1,100f6.2)')
     *     minlat+(i-1)*dellat+dellat/2.0,(qalpha(i,j),j=1,nlon)
         enddo
         do i=1,nlat
            do j=1,nlon
               write(3,'(2f10.3,f10.2)')
     *         minlat+(i-1)*dellat+dellat/2.0,
     *         minlon+(j-1)*dellon+dellon/2.0,qalpha(i,j)        
            enddo
        enddo
        close(2)
        close(3)

         do i=1,nlat
            do j=1,nlon
c
c   find min and max value
c
               if(q0(i,j).ne.0.0) then
                  if(q0(i,j).lt.qmin)qmin=q0(i,j)
                  if(q0(i,j).gt.qmax)qmax=q0(i,j)
               endif
            enddo
         enddo

c
c   write to contour file
c
         open(4,file='codaq_area.contour',status='unknown')
         
        write(4,'(a)')' Fields to use'
         write(4,'(a,2f10.3,i10)')
     *   ' Latitude range and number of values    ',
     *   minlat,maxlat,nlat
         write(4,'(a,2f10.3,i10)')
     *   ' Longitude range and number of values   ',
     *   minlon,maxlon,nlon
c
c   find values for contour
c
         write(6,'(a,2f8.1)') 'Range of Q values to plot', qmin,qmax
         range=1.25*(qmax-qmin)
         k=range/10.0
         k=k/10
         if(k.eq.0) k=1
         k=k*10
         
         i=qmax/10
         x=i*10+k
         
         do i=1,14
           xx=int(x)- k*(i-3)
           write(4,'(a,f6.1)')
     *   ' Contour level to plot and color          ',xx
          enddo

         do i=1,nlat
            do j=1,nlon
               write(4,*) minlon+(j-1)*dellon,
     *         minlat+(i-1)*dellat,q0(i,j)
            enddo
         enddo
         
       close(4)

       range=qmax-qmin
       xx=range/3.0
       open(11,file='codaq_area.red',status='unknown')
       open(12,file='codaq_area.green',status='unknown')
       open(13,file='codaq_area.blue',status='unknown')
c
c
c
c   make a 'epicenter file' with points in grid where there is data
c
         open(4,file='codaq_area.dat', status='unknown')
         do i=1,nlat
            do j=1,nlon
               k=0
c
c   count all observations for that cell
c
               do if=1,nfreq
                  if(nq(if,i,j).gt.0) k=k+nq(if,i,j)
               enddo
               if(k.gt.0) then
                  text=' '              
                  write(text,'(23x,f7.3,f8.3)') 
     *            minlat+(i-1)*dellat+dellat/2.0,
     *            minlon+(j-1)*dellon+dellon/2.0
                  text(1:20)=' 2000 0101 0101 10.5'
                  text(40:43)='05.0'
c
c   make symbol size proportional with number of observations
c
                  x=alog10(float(k))
                  x=x*3.0-1.0
                  if(x.lt.0.0) x=1.0
                  if(x.gt.9.0) x=9.0
                  write(text(57:59),'(f3.1)') x
     
                  text(80:80)='1'
                  text(22:22)='L'
                  if(q0(i,j).gt.qmin.and.q0(i,j).le.qmin+xx)
     *            write(11,'(a)') text
                  if(q0(i,j).gt.qmin+xx.and.q0(i,j).le.qmin+2*xx)
     *            write(12,'(a)') text
                  if(q0(i,j).gt.qmin+2*xx.and.q0(i,j).le.qmin+3*xx)
     *            write(13,'(a)') text
                  write(4,'(a)') text
                  write(4,*)' '
               endif
            enddo
         enddo
         close(4)
         close(11)
         close(12)
         close(13)
c
c   make plot script. projection and map file from SEISAN.DEF
c
      open(4,file='codaq_area_epimap.inp',status='unknown')
       
       write(4,'(i2)') map_proj
       write(4,'(f6.1,1x,f6.1)') minlat,maxlat
       write(4,'(f7.1,1x,f7.1)') minlon,maxlon
       write(4,*) '         '
       range=maxlat-minlat
       if(range.ge.100.0)                    dlat=20.0
       if(range.lt.100.0.and.range.ge. 50.0) dlat=10.0
       if(range.lt.50.0. and.range.ge. 10.0) dlat= 5.0
       if(range.lt.10.0. and.range.ge.  5.0) dlat= 1.0
       if(range.lt.5.0.  and.range.ge.  1.0) dlat= 0.5
       if(range.lt.1.0.  and.range.ge.  0.5) dlat= 0.1
       if(range.lt.0.5)                      dlat=0.05
       range=maxlon-minlon
       if(range.ge.200.0)                    dlon=40.0
       if(range.lt.200.0.and.range.ge.100.0) dlon=20.0
       if(range.lt.100.0.and.range.ge. 50.0) dlon=10.0
       if(range.lt.50.0. and.range.ge. 10.0) dlon= 5.0
       if(range.lt.10.0. and.range.ge.  5.0) dlon= 1.0
       if(range.lt.5.0.  and.range.ge.  1.0) dlon= 0.5
       if(range.lt.1.0.  and.range.ge.  0.5) dlon= 0.1
       if(range.lt.0.5)                      dlon=0.05
c
c  here chose q-grid instead of automatic grid
c
       dlat=dellat
       dlon=dellon

       write(4,'(2f7.2)') dlat,dlon
       write(4,'(2f7.2)') dlat,dlon
       write(4,*)'   '

       txt  = top_directory(:seiclen(top_directory))
     &           // dchar // "DAT" // dchar //
     &           map_file(:seiclen(map_file)) // ".MAP"
       write(4,'(a80)') txt
       write(4,'(a31)') '$$$$$$end of contour list$$$$$$'
       if(plot_q10.eq.0) write(4,'(a,2f7.1)') 
     *'Coda Q0, range of Q0 values to plot', qmin,qmax
       if(plot_q10.eq.1) write(4,'(a,2f7.1)')
     *'Coda Q010, range of Q10 values to plot', qmin,qmax
       write(4,*)'    '
       write(4,'(a)') 'codaq_area.contour'
       write(4,*)'    '
c       write(4,'(a)') 'codaq_area.dat'   ! epi file
       write(4,'(a)') 'codaq_area.red  3'
       write(4,'(a)') 'codaq_area.blue  1'
       write(4,'(a)') 'codaq_area.green  2'
       write(4,'(a38)') '$$$$$$end of epicentre file list$$$$$$'
       write(4,*) '   '
       write(*,*) 'Writing codaq_area_epimap.inp'
       close(4)           
       call systemc("epimap codaq_area_epimap.inp",28)

   

 
c
      write(6,*)
      write(6,*) 'File with area grid:    codaq_area.out'
      write(6,*) 'File with grid points: codaq_grid.out'
      write(6,*) 'File with epimap commands: codaq_area_epimap.inp'
      stop
      end 


        subroutine qzero_old
     *  (nfreq,fre,q,nq,cs,cq,crms,sdcq,q0,sdq0,ss,sdss,corr,rms)
c
c   calculates the parameters in the relationship
c   q = q0*f**ss with ss variable or ss fixed
c
c   input:
c   nfreq:      number of frequencies
c   fre:        the frequencies
c   q:          q - values
c   nq:         number of q values for that frequncy
c   cs:         constant ss value
c
c   output:
c   crms        rms when using a constant ss
c   sdqc:       standard deviation in qc
c   cq          q0 whan using a constant ss
c   q0:         q0
c   sdq0        standard deviation in q0
c   ss:         ss
c   sdss:       standard deviation in ss
c   corr:       correlation coefficient
c   rms:        rms error of fit
c
        dimension q(10),fre(10),nq(10),x(15000),y(15000),z(15000)
        dimension sigmay(15000)

        integer check
        check=0
        nn=0
        ndif=0
        cq=0.0
c
c   weight by using each value nq times jun 94, now use weight by number
c
        do 20 i=1,nfreq
           number=nq(i)
           if(q(i).gt.0.0) then
              ndif=ndif+1
c             do 5 k=1,number
                 nn=nn+1
                 if(check.eq.1)
     *           write(3,*)' qzero: fre(i),q(i)',fre(i),q(i)
                 x(nn)=alog10(fre(i))
                 y(nn)=alog10(q(i))
                 sigmay(nn)=1.0/sqrt(float(number))
                 cq=cq+y(nn)-cs*x(nn)
c5             continue
           endif
 20      continue
         if(ndif.gt.1) then
c            call lsqlin(nn,x,y,q0,ss,corr,rms)
            if(check.eq.1) write(3,*)' call linfit'
            call linfit(x,y,sigmay,nn,1,q0,sigmaq0,ss,sdss,corr)
            if(check.eq.1)write(3,*)' linfit called,q0,ss',q0,ss
c           write(6,*)'q0=',10**aq,'sq',sigmaa,'b=',bq,'sb=',sigmab,rcor
c           write(3,*)'q0=',10**aq,'sq',sigmaa,'b=',bq,'sb=',sigmab,rcor
c           if(check.eq.1) write(6,*)' calculate z(i)'
c           do 25 i=1,nn
c             z(i)=10**(y(i)-ss*x(i))
c25         continue
c           if(check.eq.1) write(6,*)' now call sdv for q0 '
c           call sdv(nn,z,av,sdq0)
c           if(check.eq.1) write(6,*)' sdv called, av,sd=', av,sdq0
c           do 26 i=1,nn
c             if(x(i).ne.0.0) then
c                z(i)=(y(i)-q0)/x(i)
c             else
c                z(i)=ss
c             endif
c26         continue
c           if(check.eq.1) write(6,*)' call sdv for ss'
c           call sdv(nn,z,av,sdss)
            q0=10.0**q0
            sdq0=(10**sigmaq0-1.0)*q0
         else
            q0=0.0
            sdq0=0.0
            ss=0.0
            sdss=0.0
            corr=0.0
            rms=0.0
         endif
         if(nn.gt.0) then
            q1=cq/float(nn)
            cq=10.0**q1
            do 30 l=1,nn
               crms=crms+(y(l)-q1-cs*x(l))*(y(l)-q1-cs*x(l))
               z(l)= 10**(y(l)-cs*x(l))
 30         continue
            if(check.eq.1)  write(6,*)' call sdv for cq'
            call sdv(nn,z,av,sdcq)
            crms=sqrt(crms/float(nn))
         else
            cq=0.0
            sdcq=0.0
            crms=0.0
         endif
         return
         end
c

c
c
C  SUBROUTINE LINFIT
C  
C  PURPOSE
C   MAKE A LEAST-SQUARES FIT TO DATA WITH A STRAIGHT LINE
C       Y = A + B*X
C
C  USAGE
C    CALL LINFIT (X, Y, SIGMAY, NPTS, MODE, A, SIGMAA, B, SIGMAB ,R)
C
C  DESCRIPTIONMETERS
C      
C       X       - ARRAY OF DATA POINTS FOR INDEPENDENT VARIABLE 
C       Y       - ARRAY OF DATA POINTS FOR DEPENDENT VARIABLE
C       SIGMAY  - ARRAY OF STANDARD DEVIATIONS FOR Y DATA POINTS
C       NPTS    - NUMBER OF PAIRS OF DATA POINTS
C       MODE    - DETERMINES METHOD OF WEIGHTING LEAST-SQUARES FIT
C                 +1 (INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C                  0 (NO WEIGHTING) WEIGHT(I)=1.
C                 -1 (STATISTICAL)  WEIGHT(I)=1./Y(I)
C       A       - Y INTERCEPT OF FITTED STRAIGHT LINE
C       SIGMAA  - STANDARD DEVIATION OF A
C       B       - SLOPE OF FITTED STRAIGHT LINE 
C       SIBMAB  - STANDARD DEVIATION OF B
C       R       - LINEAR CORRELATION COEFFICIENT
C
C  SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C    NONE
C  
C  MODIFICATIONS FOR FORTRAN II
C    OMIT DOUBLE PRECISION SPECIFICATIONS
C    CHANGE DSQRT TO SQRTF IN STATEMENTS 67,68, AND 71

      subroutine linfit(x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
      dimension x(*),y(*),sigmay(*)
c
c   accumulate weighed sums
c
 11   sum=0.0
      sumx=0.0
      sumy=0.0
      sumx2=0.0
      sumxy=0.0
      sumy2=0.0
 21   continue
      do 50 i=1,npts
	x1=x(i)
	y1=y(i)
	if(mode) 31,36,38
 31     if(y1) 34,36,32
 32     weight=1.0/y1
	go to 41
 34     weight=1.0/(-y1)
	go to 41
 36     weight=1.0
	go to 41
 38     weight=1.0/sigmay(i)**2
 41     sum=sum+weight
	sumx=sumx+weight*x1
	sumy=sumy+weight*y1
	sumx2=sumx2+weight*x1*x1
	sumxy=sumxy+weight*x1*y1
	sumy2=sumy2+weight*y1*y1
 50   continue
c
c   calculate coefficients and standard deviations
c
 51   delta=sum*sumx2-sumx*sumx
      a=(sumx2*sumy-sumx*sumxy)/delta
 53   b=(sumxy*sum-sumx*sumy)/delta
c
c  patched up to use same varnce at all times jh jun 94
c
c61   if(mode) 62,64,62 
c62   varnce=1.0
c     go to 67
 64   c=npts-2
c
c   modified to not crash with only 2 points
c
      if(c.gt.0) then
	 varnce=(sumy2+a*a*sum+b*b*sumx2
     *   -2.0*(a*sumy+b*sumxy-a*b*sumx))/c
      else
	 varnce=0.0
      endif
 67   sigmaa=sqrt(varnce*sumx2/delta)
 68   sigmab=sqrt(varnce*sum/delta)
 71   r=(sum*sumxy-sumx*sumy)/
     *sqrt(delta*(sum*sumy2-sumy*sumy))
      return
      end


	subroutine qzero
     *  (nfreq,fre,q,nq,sd,cs,cq,crms,sdcq,q0,sdq0,ss,sdss,corr,rms)
c
c   may 2017 error calculation corrected
c
c   calculates the parameters in the relationship
c   q = q0*f**ss with ss variable or ss fixed
c 
c   input:
c   nfreq:      number of frequencies
c   fre:        the frequencies
c   q:          q - values
c   sd:         standard deviation
c   nq:         number of q values for that frequncy
c   cs:         constant ss value
c
c   output:
c   crms        rms when using a constant ss
c   sdqc:       standard deviation in qc
c   cq          q0 whan using a constant ss
c   q0:         q0
c   sdq0        standard deviation in q0
c   ss:         ss
c   sdss:       standard deviation in ss
c   corr:       correlation coefficient
c   rms:        rms error of fit
c
	dimension q(10),sd(10),fre(10),nq(10),x(45000),y(45000),z(45000)
	dimension sigmay(45000)
	integer check
   
	check=0
	nn=0
	ndif=0
	cq=0.0

c
c   weight by using each value nq times jun 94, now use weight by number, also wrong,
c   now use standard deviation/q-value
c

c   at least 2 values so sd is not zero

	do 20 i=1,nfreq
	   number=nq(i)
	   if(q(i).gt.0.0.and.sd(i).gt.0) then
	      ndif=ndif+1
		 nn=nn+1
		 if(check.eq.1) 
     *           write(3,*)' qzero: fre(i),q(i)',fre(i),q(i)

		 x(nn)=alog(fre(i))
		 y(nn)=alog(q(i))
c		 sigmay(nn)=1.0/sqrt(float(number))   ! wrong, used for many years

                 sigmay(nn)=sd(i)/q(i)

		 cq=cq+y(nn)-cs*x(nn)
	   endif
 20      continue
	 if(ndif.gt.1) then
            if(check.eq.1) write(3,*)' call linfit'
	    call linfit(x,y,sigmay,nn,1,q0,sigmaq0,ss,sdss,corr)

            if(check.eq.1)write(3,*)' linfit called,q0,ss',q0,ss

            q0=exp(q0)
c
c   this from may 2017
c
            sdq0=sigmaq0*q0
c
c   this was also wrong
c	    sdq0=(10**sigmaq0-1.0)*q0  !same as 10**(sigma+q0) -10**q0 with q0 the log value

	 else
	    q0=0.0
	    sdq0=0.0
	    ss=0.0
 	    sdss=0.0
	    corr=0.0
	    rms=0.0
	 endif
	 if(nn.gt.0) then
	    q1=cq/float(nn)
c 	    cq=10.0**q1
            cq=exp(q1)
	    do 30 l=1,nn
	       crms=crms+(y(l)-q1-cs*x(l))*(y(l)-q1-cs*x(l))
	       z(l)= exp((y(l)-cs*x(l)))
 30         continue
	    if(check.eq.1)  write(6,*)' call sdv for cq'
	    call sdv(nn,z,av,sdcq)
	    crms=sqrt(crms/float(nn)) 
	 else
	    cq=0.0
	    sdcq=0.0
	    crms=0.0
	 endif
	 return
	 end
