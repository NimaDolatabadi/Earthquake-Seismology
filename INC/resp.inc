c
c definition of variables used in common block response 
c
      integer nfilt                    ! number of filters
      integer pole(10)                 ! filter poles
      real ffilt(10)                   ! filter frequencies
      real gain                        ! amplifier gain
      real gencon                      ! generator constant
      real dampin                      ! seismometer damping ratio
      real period                      ! seismometer natural period
      real regain                      ! gain recording media
      real g1hz                        ! total gain at 1 Hz 
      real freq_val(max_resp_value),gain_val(max_resp_value),
     &  phas_val(max_resp_value)       ! individual response values
      integer sentyp                   ! sensor type 2: seismometer, 3: acc
      integer npol,nzero               ! number of poles and zeros
      complex pol(max_resp_value),
     &  zero(max_resp_value)           ! complex PAZ
      real norm                        ! normalization constant for paz
      logical gse_resp                 ! true if gse format
      logical sei_resp                 ! true if seisan format
c      logical seed_resp                ! true if seed format
      integer nfa                      ! number of freq and amp
      logical ftab                     ! if true, force tabulated values

      common /response/nfilt,pole,ffilt,gain,gencon,dampin,period,ftab,
     *                 regain,g1hz,freq_val,gain_val,phas_val,sentyp,
     *                 npol,nzero,pol,zero,norm,gse_resp,sei_resp,nfa 

