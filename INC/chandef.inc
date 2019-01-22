
      character*5 def_in_stat(2000)  ! input station code in def file
      character*4 def_in_comp(2000)  ! input component code in def file
      character*5 def_out_stat(2000) ! output station code in def file
      character*4 def_out_comp(2000) ! output component code in def file
      integer   def_chan_number(2000)! channel number of current unit
      integer ndef_chan             ! number of channels defined
      common /def_chan/def_in_stat,def_in_comp,def_out_stat,
     *                 def_out_comp,def_chan_number,ndef_chan

