
c
c defintion of variables used with findp
c

      logical lrmean,lrtr,lchfu,lrec,lstasq,  ! defintion of routines to be done
     &  lautocodasq,lautospec,lautophase,lautoamp,lautoloc,lclear
      logical spec_overwrite,phase_overwrite  ! flags for overwrite
      logical lwavout                         ! true if findp.wav create
      real kchfu                              ! k in char function
      real spec_dur_choice                    ! 0: time window, 2: group vel win
      real spectrum_length_p                  ! length of signal for auto momemnt
      real spectrum_length_s                  ! length of signal for auto momemnt
      real spectrum_pre_length                ! time window before signal
      real spfmin,spfmax                      ! spectral range, fixed unless sp_autofind.eq.1
      logical sp_autofind                     ! 1 to find spectral range
      real gvel_p(2),gvel_s(2)
      real dist_f_sel(2)
      real phase_mode,phase_infile
      integer ga_pop_size
      integer ga_n_generations
      integer ngrid_f,ngrid_om,grid_nloop
      real input_mode                         ! 1: input waveform data
                                              ! 2: s-file from env variable, sfile
                                              !    is output file
                                              ! 3: input nordic file, output is autosig.out
      logical spike_test                      ! true for check if spike
      real search_method                      ! 1: ga
                                              ! 2: grid
      real norm_fit                           ! norm to compute misfit
      logical lall_phases                     ! 1: write out all phases
      real ffixl,ffixh                        ! fixed f band
      real qcorner
      real dkappa


      common /autosig_block/ 
     &   gvel_p,gvel_s,dist_f_sel,
     &   spectrum_length_p,spectrum_length_s,spectrum_pre_length,
     &   spec_dur_choice,ffixl,ffixh,qcorner,dkappa,
     &   spfmin,spfmax,ga_pop_size,ga_n_generations,
     &   ngrid_f,ngrid_om,grid_nloop,
     &   lrmean,lrtr,phase_mode,phase_infile,input_mode,
     &   search_method,norm_fit,
     &   lchfu,lrec,lwavout,lstasq,lautocodasq,lautospec,lautophase,
     &   lall_phases,lclear,
     &   lautoamp,lautoloc,spike_test,spec_overwrite,phase_overwrite,
     &   sp_autofind


