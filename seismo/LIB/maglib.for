
      subroutine svd_ml(nobs,amp,dist,stcoid,evid,ref,
     &  magscale,magscale_err,siteterm,siteterm_err,
     &  sourceterm,sourceterm_err,gdist,fixscale,fixsite,err)
c
c input:
c
c    nobs - number of observations
c    amp - amplitude in nm to calculate Ml
c    dist - event-station distance in km
c    stcoid - stream index
c    evid - event index
c    ref - distance reference: 1=dist, 2=amp, 3=mag
c
c output:
c
c    mag = log (amp) + a log(dist/refdist) + b (dist-refdist) +c
c                     (1)                   (2)               (3)
c    magscale_err - magscale error 
c    siteterm - site term
c    siteterm_err - site term error
c    sourceterm - source term
c    sourceterm_err - source term error
c
c    err - error: 0 if ok, -1 if not
c    
c  changes
c
c     05/07/2010 lo distance dependant geometrical spreading, assume sg for distance less than lgdist, then lg
c     24/08/2010 changed order of a for sg/lg
c     28/02/2013 jh max_x increased from 2000 to 3000
c
      implicit none
      
      integer nobs
      real amp(*),dist(*)
      integer evid(*),stcoid(*)
      real magscale(*),magscale_err(*)
      real sourceterm(*),sourceterm_err(*)
      real siteterm(*),siteterm_err(*)
      real fixscale(*),fixsite

      integer max_x,max_y             ! array dimensions for inversion
      parameter (max_x=3000)          ! size of model vector
      parameter (max_y=10*max_x)      ! number of observations
      real data_vector(max_y)         ! amplitude vector
      real kernel_matrix(max_y,max_x) ! kernel matrix
      real model_vector(max_x)        ! model vector
      real cvm(max_x,max_x)           ! covariance matrix
      integer maxsta                  ! maximum number of stations array dimensions
      parameter(maxsta=300)
      integer maxst                   ! actual maximum number of stations
      real v(max_x,max_x),w(max_y)    ! matrices used with SVD
      real thresh,wmax,tol            ! used when editing the w vector
      parameter (tol=1.E-5)
      integer mp,np                   ! number of cols and rows in matrices
      integer i,j,k                   ! counters
      integer maxev                   ! maximum event id
      integer err
      real ref(3)                     ! reference, 1:ref dist, 2=amp (WA mm), 3=mag
      real wagain
      parameter (wagain=2080.)
      logical flag
      real gdist(*)                   ! min distance to assume sn geometrical spreading
      integer ng(3)                   ! count sg

c
c init variable and arrays
c

c ml=3 gives 10 mmWA at 100km distance Richter                         
c ml=3 gives 17 mmWA at 17km distance (Hutton and Boore, 1987)
      if (ref(1).eq.0.) then
        ref(1)=100.
        ref(2)=1.
        ref(3)=3.
      endif
      maxst=0
      maxev=0
      err=0
c
c init 
c
      do i=1,nobs+3
        data_vector(i)=0.
        do j=1,max_x
          if (i.eq.1) model_vector(j)=0.
          kernel_matrix(i,j)=0.
        enddo
      enddo
      if (nobs.gt.max_y) then
        err=-1
        write(*,*) ' dimensions exceeded, stop '
        return
      endif
      ng(1)=0
      ng(2)=0
      ng(3)=0

c ML = log A + a log(dist/refdist) + b (dist-refdist) + c + S
c log A = ML - a log(dist/refdist) - b (dist-refdist) - c - S

c
c fill kernel matrix
c
      do i=1,nobs
        if (amp(i).ne.0.) then
          if (stcoid(i).gt.maxst) maxst=stcoid(i)
          if (evid(i).gt.maxev) maxev=evid(i)

          data_vector(i)=alog10(amp(i))              ! data
c
c try with distance dependant geometrical spreading
c
          if (dist(i).le.gdist(1)) then
            kernel_matrix(i,1)=-alog10(dist(i)/ref(1)) ! log distance
            ng(1)=ng(1)+1 ! count sg
            kernel_matrix(i,2)=0.
            kernel_matrix(i,3)=0.
          elseif  (dist(i).le.gdist(2)) then
            kernel_matrix(i,2)=-alog10(dist(i)/ref(1)) ! log distance
c            kernel_matrix(i,2)=-alog10((dist(i)-gdist(1))/ref(1)) ! log distance
            ng(2)=ng(2)+1 ! count sg
            kernel_matrix(i,1)=0.
c-alog10(gdist(1)/ref(1))
            kernel_matrix(i,3)=0.
          else
            kernel_matrix(i,3)=-alog10(dist(i)/ref(1)) ! log distance
c            kernel_matrix(i,3)=-alog10((dist(i)-gdist(2))/ref(1)) ! log distance
            kernel_matrix(i,1)=0.
c-alog10(gdist(1)/ref(1))
            kernel_matrix(i,2)=0.
c-alog10((gdist(2)-gdist(1))/ref(1))
            ng(3)=ng(3)+1 ! count lg
          endif

          kernel_matrix(i,4)=-(dist(i)-ref(1))       ! distance
c lo changed
c          kernel_matrix(mp,4)=1.                     ! -c
          if (fixsite.eq.0.) 
     &    kernel_matrix(i,5+stcoid(i))=-1.           ! station term
          kernel_matrix(i,5+maxsta+evid(i))=1.       ! event size term, ML
          if (5+maxsta+evid(i).gt.max_x) then
            write(*,*) ' dimensions exceeded, stop '
            err=-1
            return
          endif
c          write(25,*) i,stcoid(i),evid(i)
        endif
      enddo

c
c invert for Ml scale
c
        mp=nobs
c
c add constraint for constant added to source term
c at refdist log A = ML - c
c
        mp=mp+1
        data_vector(mp)=alog10(ref(2)/wagain*1E6)-ref(3) ! log A - ML
        kernel_matrix(mp,1)=0.
        kernel_matrix(mp,2)=0.
        kernel_matrix(mp,3)=0.    
        kernel_matrix(mp,4)=0.    
        kernel_matrix(mp,5)=-1.                        ! -c

c
c if no ng, fix mag scale parameter to 0
c
        if (ng(1).eq.0) then
          mp=mp+1
          data_vector(mp)=0.
          kernel_matrix(mp,1)=999.
          write(*,*) ' cant invert for geom spreading part 1 '
        endif
        if (ng(2).eq.0) then
          mp=mp+1
          data_vector(mp)=0.
          kernel_matrix(mp,2)=999.
          write(*,*) ' cant invert for geom spreading part 2 '
        endif
        if (ng(3).eq.0) then
          mp=mp+1
          data_vector(mp)=0.
          kernel_matrix(mp,3)=999.
          write(*,*) ' cant invert for geom spreading part 3 '
        endif
c
c fix magnitude scale parameters
c
        if (fixscale(1).gt.0..and.ng(1).gt.0) then
          mp=mp+1
          kernel_matrix(mp,1)=999.
          data_vector(mp)=fixscale(1)*999.
        endif
        if (fixscale(2).gt.0..and.ng(2).gt.0) then
          mp=mp+1
          kernel_matrix(mp,2)=999.
          data_vector(mp)=fixscale(2)*999.
        endif
        if (fixscale(3).gt.0..and.ng(3).gt.0) then
          mp=mp+1
          kernel_matrix(mp,3)=999.
          data_vector(mp)=fixscale(3)*999.
        endif
        if (fixscale(4).gt.0.) then
          mp=mp+1
          kernel_matrix(mp,4)=9999.
          data_vector(mp)=fixscale(4)*9999.
        endif

c
c add constraint for all site terms to sum up to 0
c
        mp=mp+1
        data_vector(mp)=0.
        do i=6,5+maxst
          kernel_matrix(mp,i)=1
        enddo
c
c remove empty columns from kernel matrix, shift event terms to the left
c
        do j=1,maxev
          do i=1,mp
            kernel_matrix(i,j+5+maxst)=kernel_matrix(i,j+5+maxsta)
            kernel_matrix(i,j+5+maxsta)=0.
          enddo
        enddo

        np=5+maxst+maxev
        write(*,*) ' number of stations/events ',maxst,maxev
        write(*,*) ' number of equations ',mp
        write(*,*) ' number of model parameters ',np
        write(*,*) ' debug ng(1,2,3) ',ng(1),ng(2),ng(3)
        if (np.gt.mp) then
          write(*,*) ' too few data, stop '
          stop
        endif
c
c check that no empty columns
c
        flag=.false.
        do i=1,np
          k=0
          do j=1,mp
            if (kernel_matrix(j,i).ne.0.) then
              flag=.true.
              k=k+1
            endif
          enddo
c          write(24,*) i,k
        enddo
        if (.not.flag) then
          write(*,*) ' empty column, stop '
          stop
        endif
c
c write out lin equations
c
c        do i=1,mp
c          write(23,*) data_vector(i),' = ',(kernel_matrix(i,j),j=1,np)
c        enddo
        write(*,*) ' calling svdcmpx '
        call svdcmpx(kernel_matrix,mp,np,max_y,max_x,w,v)
c
c edit w, remove small values
c
         wmax=0.
         do j=1,max_x
           if(w(j).gt.wmax)wmax=w(j)
         enddo
         thresh=tol*wmax
         do j=1,max_x
           if (w(j).lt.thresh)w(j)=0.
         enddo

c
c call svbksb
c
        write(*,*) ' calling svbksbx '
        call svbksbx(kernel_matrix,w,v,mp,np,max_y,max_x,
     *      data_vector,model_vector)

c
c call svdvar to get variance
c
        write(*,*) ' calling svdvar '
        call svdvar(v,np,max_x,w,cvm,max_x)
c
c set output
c
      magscale(1)=model_vector(1)
      magscale(2)=model_vector(2)
      magscale(3)=model_vector(3)
      magscale(4)=model_vector(4)
      magscale(5)=model_vector(5)

      magscale_err(1)=sqrt(cvm(1,1))
      magscale_err(2)=sqrt(cvm(2,2))
      magscale_err(3)=sqrt(cvm(3,3))
      magscale_err(4)=sqrt(cvm(4,4))
      magscale_err(5)=0.

c      write(*,*) ' cvm a1,a2 ',cvm(1,2),cvm(2,1)
c      write(*,*) ' cvm a1,b  ',cvm(1,3),cvm(3,1)
c      write(*,*) ' cvm a2,b  ',cvm(2,3),cvm(3,2)

      do i=1,maxst
        siteterm(i)=model_vector(i+5)
        siteterm_err(i)=sqrt(cvm(i+5,i+5))
      enddo
      do i=1,maxev
        sourceterm(i)=model_vector(i+5+maxst)
        sourceterm_err(i)=sqrt(cvm(i+5+maxst,i+5+maxst))
      enddo

      return
      end


