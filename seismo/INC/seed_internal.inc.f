ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c seed_internal.inc.f
c Parameters & internal variables of SEED readers
c
c DO NOT INCLUDE THIS FILE IN YOUR MAIN PROGRAM!!!
c
c Author: Rodrigo Canabrava
c E-mail: rlpcfr@yahoo.com.br
c Universitetet i Bergen - Department of Earth Sciences
c
c  changes
c
c dec 21 2005 jh  add seed_pc
c sep 25 2008 jh change dim to 1900000
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Parameters
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     BLK_SIZE is not a parameter, it can be changed during program
c     execution.
c     MAX_BLK_SIZE must be at most the size of the variable seed_record

      INTEGER*4 BLK_SIZE
      INTEGER*4 MAX_BLK_SIZE    ! Maximum block size supported
      INTEGER*4 MAX_SAMP        ! Maximum number of samples
      PARAMETER (MAX_BLK_SIZE = 32768)
      PARAMETER (MAX_SAMP = 1900000)


      INTEGER FRAME_SIZE        ! Frame size
      INTEGER STEIM_1, STEIM_2, INT_32BIT ! codes of encoding formats
      CHARACTER*5 S_STEIM
      PARAMETER (FRAME_SIZE = 64)
      PARAMETER (INT_32BIT = 3)
      PARAMETER (STEIM_1 = 10)
      PARAMETER (STEIM_2 = 11)
      PARAMETER (S_STEIM = 'STEIM')


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      
c Variables:
c "seed_record" holds one block of BLK_SIZE.
c The following fields make EQUIVALENCE to part of "seed_record",
c according to the file layout.
c
c The variables
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      character*32768 seed_record ! a block from the file

      common / seed_data_record / seed_record, BLK_SIZE, sample_rate, 
     &     hour, minute, second, questionable_time_tag

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Variables related to fixed header of Data Record
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      character*6    seq_number ! sequence number
      character      header_type ! header type
      character      continuation ! continuation code
      character*5    station    ! station name
      character*2    location   ! location id
      character*3    channel    ! channel id
      character*2    network    ! network id
      integer*2      year       ! year
      integer*2      day_of_year ! day of year
      character*1    hour_      ! hour as a char
      character*1    minute_    ! minute as a char
      character*1    second_    ! second as a char
      integer*2      fracsec    ! fraction of second
      integer*2      n_samples  ! number of samples
      integer*2      sample_rate_factor ! sample rate factor
      integer*2      sample_rate_multiplier ! sample rate multiplier
      character*1    activity_flags ! activity flags
      character*1    io_flags   ! input/output flags
      character*1    quality_flags ! quality flags
      character*1    n_blockettes ! number of blockettes
      integer*4      time_correction ! time correction
      integer*2      data_p     ! position of start of data
      integer*2      first_blk  ! position of first blockette
      logical        time_tag_flag ! set to get output only first time

      logical*1      questionable_time_tag ! "questionable time tag" flag
      logical*1      seed_pc    ! true if on as pc platform

      equivalence(seed_record(1:6), seq_number)
      equivalence(seed_record(7:7), header_type)
      equivalence(seed_record(8:8), continuation)
c     reserved byte skipped: seed_record(8)
      equivalence(seed_record(9:13), station)
      equivalence(seed_record(14:15), location)
      equivalence(seed_record(16:18), channel)
      equivalence(seed_record(19:20), network)
      equivalence(seed_record(21:22), year)
      equivalence(seed_record(23:24), day_of_year)
      equivalence(seed_record(25:25), hour_)
      equivalence(seed_record(26:26), minute_)
      equivalence(seed_record(27:27), second_)
c     reserved byte skipped: seed_record(28)
      equivalence(seed_record(29:30), fracsec)
      equivalence(seed_record(31:32), n_samples)
      equivalence(seed_record(33:34), sample_rate_factor)
      equivalence(seed_record(35:36), sample_rate_multiplier)
      equivalence(seed_record(37:37), activity_flags)
      equivalence(seed_record(38:38), io_flags)
      equivalence(seed_record(39:39), quality_flags)
      equivalence(seed_record(40:40), n_blockettes)
      equivalence(seed_record(41:44), time_correction)
      equivalence(seed_record(45:46), data_p)
      equivalence(seed_record(47:48), first_blk)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c The following fields have 1 byte in the header layout.
c This is a problem because not all compilers accept integer*1
c declarations. So, they are "equivalenced" to the seed_record as character 
c fields, and converted to integer.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer*2   hour          ! hour
      integer*2   minute        ! minute
      integer*2   second        ! second


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sample rate must be calculated from two factors in the header,
c so it needs an extra variable that will hold the result
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real*4 sample_rate        ! sample rate in real*4

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Blockette 1000
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer*2   encoding      ! encoding format
      integer*2   rec_length    ! record length as a power of 2
      logical     is_swapped    ! .TRUE. if file is in inversed byte order
      common / blk_1000 / encoding, rec_length, is_swapped, seed_pc


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Blockette 1001
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer*2 timing_quality  ! timing quality
      integer*2 usec            ! micro seconds error
      integer*2 n_frames        ! number of frames in data section

      common / blk_1001 / timing_quality, usec, n_frames
