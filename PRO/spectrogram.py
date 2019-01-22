#!/usr/bin/python

#   script to make spectrograms. the code initially comes from Jari Kortstrom
#   options for window length etc makde by jens havskov, dec 2017
#
#Load relevant obspy libraries
import sys
from obspy import read

#   Get file name and options from command line.
#
#   1: file name
#   2: window length
#   3: filter frequency
#   4: plot trace

if len(sys.argv) < 5:
    print "Invalid command line. Usage: spectrogram file-name window filter-f plot-trace"
    sys.exit(1)

#   Read data file into memory. 
#   Seismograms of various formats (e.g. SAC, MiniSEED, GSE2, SEISAN, Q, etc.) can be imported into 
#   a Stream object using the read() function.

st = read(sys.argv[1])

window=sys.argv[2]
filter=sys.argv[3]
plottrace=sys.argv[4]

#   copy the trace to a another variable.

tr = st[0]

#   Make a copy of original trace if you need it later.

tr_filt = tr.copy()

#   Filter the trace if chosen

print'Filter is',float(filter)

if float(filter) > 0.0:
   tr_filt.filter('highpass', freq=float(filter), corners=2, zerophase=True)

#   cut the trace to a suitable lenght for the spectrogram plot
#   You should have some background noise visible before P-phase and after S-phase
#   to identify the sharpness of P and the duration of S.

#    start one sec after start of data ?

dt = tr_filt.stats.starttime+1

# use window sec of data from start

tr_filt = tr_filt.slice(dt, dt+float(window))

#    Plot the filtered seismogram if that is an option

if float(plottrace) > 0.0:
   tr_filt.plot()

#   Calculate and plot the spectrogram. Here with default parameters.
#   There is lots of options

tr_filt.spectrogram()

