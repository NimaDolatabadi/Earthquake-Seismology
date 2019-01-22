c
c  sample program to illustrate how to read waveform files in SEISAN
c  all supported seisan formats can be read. the output file will give
c  a listing of all different channels found in a format which can be used 
c  for referencing archives
c
c  jh, June 2001
c
c  oct  20, 2004 jh : write out samples from channel 1 instead of 2
c  mar 5    2007 jh : add use of filenr.lis
c  jun 20 2011   jh : add call wav_init, fix bug index in write out
c  dec 27 2012   jh : add counting of different channels, output of 
c                     format for archive
c  04 01 2013 jh nrecord to auto_tr

c

      implicit none
      include 'seidim.inc'                    ! for giving dimensions
      include 'libsei.inc'                    ! for all seisan subroutines
      include 'waveform.inc'                  ! waveform data
      include 'seisan.inc'                    ! seisan general parameters
      integer nstat,nphase,nhead,nrecord,id   ! for reading s-file
      character*1 type,exp                    ! -----------------
      character*80 sfilname                   ! s-file name
      character*80 data(max_data)             ! one s-file
      integer npresent                        ! number of wav files present
      character*80 text                       ! general text string
      character*80 question                   ! question
      integer save_ncomp                      ! number of different components
      character*5 save_stat(1000)             ! list of different stations
      character*3 save_comp(1000)             ! their components
      character*2 save_network(1000)          ! their networks
      character*2 save_location(1000)         ! their location
      integer k,i,in
c
c  use a waveform file name directly or the waveform files in an s-file
c

c
c  initialize 
c
      call wav_init
      call wav_mem_init
      save_ncomp=1
      save_stat(save_ncomp)=' '
      save_comp(save_ncomp)=' '
      save_network(save_ncomp)=' '
      save_location(save_ncomp)=' '
c
c   get s-file name or wav name
c
      write(6,*)' S-file name, return for wav-name directly'
      read(5,'(a)') sfilname
      if(sfilname.ne.' ') then
c
c   open s-file
c
         open(1,file=sfilname,status='old')
c
c  read s-file
c
         call indata
     *   (1,nstat,nphase,nhead,nrecord,type,exp,data,id)
         close(1)

c
c   get waveform file names, could be several
c
         call auto_tr(data,nhead,nrecord,wav_nfiles,wav_filename)

      else
c
c   get file name directly, can be one or many
c
        in=0
        question=' File name, # or filenr.lis for all'
        call filename(question,wav_filename(1))
        if(wav_filename(1)(1:10).eq.'filenr.lis'.or.
     *  wav_filename(1)(1:10).eq.'FILENR.LIS') then
           open(8,file='filenr.lis',status='old',err=20)
           goto 21
 20        continue
           write(6,*)' No filenr.lis'
           stop
 21        continue
           in=1
        endif
        wav_nfiles=1     ! there is only one file at the time in this case
      endif
c
c   file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)')wav_filename(1) 
         if(wav_filename(1)(1:4).eq.'    ') goto 2000 ! the end
      endif
c
c   find how many waveform files are present,
c   replace the origial names without path, with the present files
c   full path like
c
       npresent=0                      ! no files initially
       do i=1,wav_nfiles
          call  get_full_wav_name(wav_filename(i),text)            
          if(text.ne.' ') then
             npresent=npresent+1
             wav_filename(npresent)=text
          endif
      enddo
c
c   print how many files were found
c
      wav_nfiles=npresent
      write(6,*)' Number of wav-files present', wav_nfiles

c
c   terminate if no waveform files
c
      if(wav_nfiles.eq.0) stop

c
      wav_nchan=0
c
c   loop to read all headers of all files
c
      do i=1,wav_nfiles
         write(6,'(1x,a)') wav_filename(i)
c
c   read all headers of file i
c
         call read_wav_header(i)
c
c   output possible errors
c
         if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *   wav_error_message
c
c   write out the format
c
         write(6,'(1x,a,a)') 'Format ',wav_file_format(i)

      enddo
c
      write(6,*)' Total number of channels available:',wav_nchan
c
c   terminate if no channels
c
      if(wav_nchan.eq.0) stop
c
c   write some input info for each channnel on screen
c
      write(6,'(a,a)')
     *'CHA STAT  COMP  YEAR MO DA HR MI    SEC   NSAMP ',
     *'    RATE    DELAY DURATION'
      do i=1,wav_nchan
         write(6,'(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i8,3f9.3)')
     *   i, wav_stat(i),wav_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i)
c
c   save all different channels so skip counting if same channel
c
         do k=1,save_ncomp
            if(wav_stat(i).eq.save_stat(k).and.
     *         wav_comp(i)(1:2)//wav_comp(i)(4:4).eq.save_comp(k).and.
     *         wav_network(i).eq.save_network(k).and.
     *         wav_location(i).eq.save_location(k))goto 30 ! found, skip
         enddo


         save_stat(save_ncomp)=wav_stat(i)
         save_comp(save_ncomp)=wav_comp(i)(1:2)//wav_comp(i)(4:4)
         save_network(save_ncomp)=wav_network(i)
         save_location(save_ncomp)=wav_location(i)  
         save_ncomp=save_ncomp+1     

 30      continue


      enddo
c
c
c----------------------------------------------------------
c  Data from any one channel can now be read, in example
c  read from channel 1
c-----------------------------------------------------------
c
      k=1
c
c   read whole channel
c
      call wav_read_channel(k)
c
c   write first 10 samples on screen, remember originaly they are integer
c   however, when read by SEISAN, they are converted to real
c
      write(6,*)
      write(6,*)' 10 first samples from channel 1'
      write(6,'(5f10.1)') (signal1(i),i=1,10)   ! signal1 is a common block variable 
      write(6,*)' 10 last samples from channel 1'
      write(6,'(5f10.1)') (signal1(i),i=wav_nsamp(1)-9,wav_nsamp(1))   ! signal1 is a common block variable 


c
c   if a list of files, go back to read next
c
      if(in.eq.1) goto 1000

c
c   the end, no more files
c
 2000 continue
  
c
      write(6,*)
      write(6,*) ' Number of different channels', save_ncomp-1
      write(6,*) ' Output in file sample_read_wav.out'
c
c   write line for archive as defined in seisan.def
c
      open(17,file='sample_read_wav.out', status='unknown')
      do i=1,save_ncomp-1

         write(17,'(a40,a5,a3,a2,a2)')
     *   'ARC_CHAN                                ',
     *   save_stat(i),save_comp(i),
     *   save_network(i),save_location(i)

      enddo
      stop
      end

