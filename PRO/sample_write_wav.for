c
c  sample program to illustrate the writing of seisan files
c
c   changes:
c 
c jan 25 2011 jh : was only wruite header 12 instead of all

      implicit none
      include 'seidim.inc'                    ! for giving dimensions
      include 'libsei.inc'                    ! for all seisan subroutines
      include 'waveform.inc'                  ! waveform data
      include 'seisan.inc'                    ! seisan general parameters
      character*80 data(max_data)             ! one s-file
      integer k,i
      character*5 net_code
      character*80 outfile
      character*80 mainhead(max_trace)
      character*1040 chead
      integer*4 idata(max_sample)

c set waveform variables
      wav_stat(1)='TEST'
      wav_comp(1)='BH Z'
      wav_year(1)=2010
      wav_month(1)=1
      wav_day(1)=1
      wav_hour(1)=0
      wav_min(1)=0
      wav_sec(1)=0.
      wav_nsamp(1)=10000
      wav_rate(1)=100.      
      wav_duration(i)=10.
      wav_nchan=1
      wav_cbyte(1)='4'

      net_code='NET'      

c set data
      do i=1,wav_nsamp(1)
        signal1(i)=float(i)
      enddo
 
c create header and get filename
      call wav_sheads(wav_nchan,net_code,outfile,mainhead,chead) 
      do i=1,12
        write(*,*) mainhead(i)
      enddo
      write(*,*) ' output file name ',outfile
c open output file
      open(1,file=outfile,status='unknown',form='unformatted') 
c write file header
      do k=1,12
        write(1)mainhead(k)
      enddo
      if(wav_out_nchan.gt.30) then
        k=(wav_out_nchan-31)/3+1
        do i=13,k+12
          write(1)mainhead(i)
        enddo
      endif
c write channel header
      write(1) chead
c convert data to integer
      do i=1,wav_nsamp(1)
        idata(i)=int(signal1(i))
      enddo
c write data
      write(1)(idata(i),i=1,wav_nsamp(1))
c close file
      close(1)
      
      stop
      end

