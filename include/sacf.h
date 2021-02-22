      
      ! IIR Filter Prototypes
      ! @see xapiir
      character*2 SAC_BUTTERWORTH
      character*2 SAC_BESSEL
      character*2 SAC_CHEBYSHEV_TYPE_I
      character*2 SAC_CHEBYSHEV_TYPE_II
      parameter(  SAC_BUTTERWORTH       = 'BU' )
      parameter(  SAC_BESSEL            = 'BE' )
      parameter(  SAC_CHEBYSHEV_TYPE_I  = 'C1' )
      parameter(  SAC_CHEBYSHEV_TYPE_II = 'C2' )

      ! IIR Filter Types
      ! @see xapiir
      character*2 SAC_BANDPASS
      character*2 SAC_HIGHPASS
      character*2 SAC_LOWPASS
      character*2 SAC_BANDREJECT
      parameter(  SAC_BANDPASS   = 'BP' )
      parameter(  SAC_HIGHPASS   = 'HP' )
      parameter(  SAC_LOWPASS    = 'LP' )
      parameter(  SAC_BANDREJECT = 'BR' )

      ! FIR Filter Types
      ! @see firtrn
      character *7  SAC_HILBERT
      character *10 SAC_DERIVATIVE
      parameter ( SAC_HILBERT    = 'HILBERT' )
      parameter ( SAC_DERIVATIVE = 'DERIVATIVE' )

      ! Window Types
      character *7  SAC_HAMMING
      character *7  SAC_HANNING
      character *9  SAC_RECTANGLE
      character *6  SAC_COSINE
      character *10 SAC_TRIANGULAR
      parameter ( SAC_HAMMING    = 'HAMMING' )
      parameter ( SAC_HANNING    = 'HANNING' )
      parameter ( SAC_RECTANGLE  = 'RECTANGLE' )
      parameter ( SAC_COSINE     = 'COSINE' )
      parameter ( SAC_TRIANGULAR = 'TRIANGULAR' )

      ! Undefined Types
      real *4 SAC_REAL_UNDEFINED
      integer SAC_INTEGER_UNDEFINED
      integer SAC_NUMBER_UNDEFINED
      character *8 SAC_CHARACTER_UNDEFINED
      parameter ( SAC_REAL_UNDEFINED      =  -12345.0 )
      parameter ( SAC_INTEGER_UNDEFINED   =  -12345   )
      parameter ( SAC_NUMBER_UNDEFINED    =  -12345   )
      parameter ( SAC_CHARACTER_UNDEFINED = '-12345  ')
