c
c subroutine for spectrogram calculation
c
      subroutine spectrogram(index,code)
      implicit none
      include 'mulplt.inc'

      integer index  ! index in wav structure
      integer code   ! return 0 if ok, otherwise 1
      integer nt,nf  ! number of time and frequenct slices
      integer smp
      integer i,j,a,b,k,ind
      real taper
      real df
      integer n(100)
      real avamp(100)
      real x(max_sample) ! temp data
      integer ipow,npad
      complex y_com(max_sample)
      integer spstart,spstop
      real f,xx
      real maxamp,amp
      integer out
      real dcol
      integer col

      taper=10.
      nt=100 ! was 50
      nf=50  ! was 20
      spectro_ncol=256
      maxamp=-1.
      out=1

      open(out,file='spectro.out',status='unknown')
      write(*,*) ' nyquist ',wav_rate(index)/2.
      write(*,*) ' abs time ',wav_abs_time(index)
c
c find length of time slice in number of samples
c
      smp=int(wav_nsamp(index)/(nt-1)) 
      spectro_rate=1./(float(smp)/wav_rate(index))
      spectro_abstime=wav_abs_time(index)
c      write(*,*) ' debug smp ',smp
      df=wav_rate(index)/2./float(nf) 
      i=0 
      do while(i.le.nt-1)
        i=i+1 
        a=(i-1)*smp+1
        b=a+smp-1
        if (b.gt.wav_nsamp(index)) b=wav_nsamp(index)
        write(*,*) i,a,b
        j=0
c transfer samples for fft
        do k=a,b
          j=j+1
          if (b.le.wav_nsamp(index)) then
            x(j)=signal1(k)
          else
            x(j)=0.
          endif
        enddo
        call prepare(smp,x,taper,ipow,npad,y_com)

c
c------- Do FFT -------
c
        j = ipow
        do k = 1,20
          j = j/2
            if (j .eq. 1)then
              j = k
              go to 222
            endif
        enddo
222     continue
        call fft(j,-1.,y_com)
        write(*,*) y_com(1),j
c
c average for frequency bins
c
        do k=1,nf   ! init
          avamp(k)=0.
          n(k)=0
        enddo
        spstart = 2
        spstop = (ipow/2) + 1
        xx = ipow*1./wav_rate(index)
        do k = spstart,spstop
          f = (k-1)/xx
          ind=int(f*.999/df)+1
          n(ind)=n(ind)+1
c          write(*,*) df,f,ind,n(ind)
          amp=sqrt((1/wav_rate(index)**2)*y_com(k)*conjg(y_com(k)))
          avamp(ind)=avamp(ind)+amp
          write(*,*) ind,y_com(k)
        enddo
        do k=1,nf
          if (n(k).gt.0) then
            avamp(k)=avamp(k)/float(n(k))
            if (avamp(k).gt.maxamp) maxamp=avamp(k)
            spectro_amp(i,k)=avamp(k)
            write(55,*) (i-1)*smp+1,df*(k-1),avamp(k)
            write(out,*) i,k,avamp(k)
          endif
        enddo
      enddo
      code=0    ! all fine
      close(out)
c
c assign colors
c
      open(out,file='spectro.col',status='unknown')
      dcol=maxamp/spectro_ncol
      do i=1,nt
        do j=1,nf
          col=int(spectro_amp(i,j)/dcol)+1
          if (col.gt.spectro_ncol) col=spectro_ncol
          spectro_colind(i,j)=col
          write(out,*) i,j,col
        enddo
      enddo

      return
      end
