cJAB(BGS)Apr25 : waveform file header sizes...
c    Include file to set array dimensions for SEISAN system
c    jan 23, 95
c    dec 96        : new parameter for number of calibration files
c    nov 99        : remove bgs stuff
c    mar 29  00 jh : add dimensions for poles and zeros
c    may 12        : increased max_dirf to 2000
c    nov 7      lo : add cont parameters
c    mar 2   91 jh : max_dirf to 5000
c    mar 5   01 lo : max_trace to 500
c    oct 10     jh : max smaple to 600000
c    dec 6         : remove max_sample_dem
c    jun 23  03 jh : max_trace to 1000    
c    jan 16  04  jh: max_index to 20000
c    may 31  05 lo : max smaple to 1,200,000
c    nov 14 2006 jh: max_dirf to 9999
c    sep    2007 jh: add max_mem_sample
c    may 17 2011 lo: increased max_data from 4000 to 10000
c    jan 7  2013 jh: added variable max_nstation for station in station file
c    jan 28 2013 jh: max cal to 4000, remove max_dirf
c    mar 14 2013 jh: max index to 200000
c
c-- max array dimension for waveform file 
c 
       integer max_sample 
c   also change in seed_internal.inc.f
       parameter(max_sample=2400000)
c
c-- max dimension of memory array, shold be a few times larger than max_sample
c
       integer max_mem_sample
       parameter(max_mem_sample=30000000)

c-- max number of phases for one station or to be defined generally
       integer max_phase
c       parameter(max_phase=120)
       parameter(max_phase=200)
c-- max number of lines in a nordic file
       integer max_data
       parameter(max_data=10000)
c-- maximum number of trace headers = maximum number of channels = 
c   max number of wav files
       integer max_trace
       parameter(max_trace=1100)      ! change back to 1000
c-- maximum number of epicenters
       integer max_epi
       parameter(max_epi=90000)
c-- maximum number points in a polygon...
       integer max_polyn$
       parameter (max_polyn$=100)
c-- maximum number of events for eev or in one index file
       integer max_index$
       parameter (max_index$=200000)
c-- maximum number of calibration files
       integer max_cal_files$
       parameter (max_cal_files$=4000)
c-- maximum number of poles and zeros, and frequency amplitude values
       integer max_resp_value
       parameter (max_resp_value=2000)

c-- maximum number of stations in station file
       integer max_nstation
       parameter(max_nstation=20000)
