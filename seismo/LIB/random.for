c-----------------------------------------------------------------------

        function ran2(idum)
        implicit none
c------------------------------------------------------------
c Long period (> 2 x 10^18) random number generator of
c L'Ecuyer with Bays-Durham shuffle and added safeguards.
c Returns a uniform random deviate between 0.0 and 1.0
c (exclusive of the endpoint values). Call with idum a -ve
c number to initialise; thereafter, do not alter idum
c between successive deviates in a sequence. RNMZ should
c approximate the largest floating value that is less than
c 1
c
c Numerical Recipes in Fortran 77: The Art of Scientific
c Computing (1986-1992)
c------------------------------------------------------------

        integer idum
        integer im1,im2
        integer imm1
        integer ia1,ia2
        integer iq1,iq2
        integer ir1,ir2
        integer ntab
        integer ndiv

        real ran2
        real am
        real eps
        real rnmx

        parameter (im1=2147483563,im2=2147483399,am=1./im1,
     &  imm1=im1-1,ia1=40014,ia2=40692,iq1=53668,iq2=52774,
     &  ir1=12211,ir2=3791,ntab=32,ndiv=1+imm1/ntab,eps=1.2e-7,
     &  rnmx=1.-eps)

        integer idum2
        integer j,k
        integer iv(ntab)
        integer iy

        save iv,iy,idum2

        data idum2/123456789/, iv/ntab*0/, iy/0/

c Initialise

        if (idum.le.0) then

c Be sure to prevent idum=0
                idum=max(-idum,1)
                idum2=idum

c Load the shuffle table (after 8 warm ups)

                do j=ntab+8,1,-1
                        k=idum/iq1
                        idum=ia1*(idum-k*iq1)-k*ir1

                        if (idum.lt.0) idum=idum+im1
                        if (j.le.ntab) iv(j)=idum
                enddo

                iy=iv(1)

        endif

c Start here when not initialising

        k=idum/iq1

c Compute idum=mod(ia1*idum,im1) without overflows by Schrage's method

        idum=ia1*(idum-k*iq1)-k*ir1

        if (idum.lt.0) idum=idum+im1

        k=idum2/iq2

c Compute idum2=mod(ia2*idum2,im2) likewise

        idum2=ia2*(idum2-k*iq2)-k*ir2
        if (idum2.lt.0) idum2=idum2+im2

        j=1+iy/ndiv     ! will be in the range 1:ntab

c Here idum is shuffled, idum and idum2 are combined to generate
c output

        iy=iv(j)-idum2
        iv(j)=idum

        if (iy.lt.1) iy=iy+imm1

c Because users don't expect endpoint values

        ran2=min(am*iy,rnmx)

        return
        end

