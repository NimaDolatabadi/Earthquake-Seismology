##################################################
#
# SEISAN 11.0
#
# June 2018
#
# Makefile for SEISAN LIB directory
#
# To use DISLIN on Linux 
#  do not use xlib.c 
#  put in xget_screen_size_unix.c
#  put in dislinplot.for
#  make a small change in seisplot.for, see file
#  get appropriate libraries etc from www.dislin.de
#
# changes:
#
#   ...
#   27 Jan 11 jh: rename seis_unix, seis_sub
#   29 Jan 11 lo: cleanup and windows gfortran
#   24 Jan 11 lo: changed names of systems
#   03 Jun 11 jh: added -fbounds-check
#   01 Nov 11 jh: included libmseed routines in seisan
#   27 Nov 11 jh: add file guralp.for
#   23 Dec 11 jh: add fmap_subs.for
#    7 Feb 12 pv: changed solaris to gfortran and gcc
#    2 apr 13 jh: add auto_proc
#   10 feb 15 pv: add vincenty_wgs84
#    3 jun 15 pv: add seisan_logging
#   21 dec 16 on: add network.c 
#   11 apr 17 lo: add random
#   11 jan 18 lo: add hypmbn
#   10 may 18 lo: add networkflag (nf), and distribution target
#   
#
##################################################

##################################################
# THE ARCHITECTURE IS EXPECTED TO BE GIVEN BY
# THE ENVIRONMENTAL VARIABLE SEISARCH, WHICH
# MUST BE: 
#
#    solaris 
#    linux32
#    linux64
#    macosx 
#    macosxppc
#    windows
#
##################################################

##################################################
# PATH TO WHICH OUTPUT FILES ARE WRIITEN,
# THIS CAN BE SET TO $(SEISARCH) IF 
# SEVERAL OPERATING SYSTEMS COMPILE 
# SOURCE ON SHARED DISKS,
# OBVIOUSLY OUTPATH CAN BE USED TO WRITE
# OBJECTS TO ANY OTHER PLACE
#
#  check if a windows platform, else assume linux
#
ifeq ($(OS),Windows_NT)
   SEISARCH = windows
   OUTPATH = .
else
   OUTPATH = $(SEISARCH)
endif
OUTPATH = .

##################################################
# COMPILERS AND FLAGS
#
#  fbounds -check finds too many problems, to be fixed
#
###############  lo
#debug_flags = -g -fbounds-check
debug_flags = -g

# following defintions apply to distribution target
distribution: debug_flags = 
#####distribution: OUTPATH = .


fc_linux64   = gfortran -c -I../INC -fdollar-ok $(debug_flags) -fno-automatic -o $@
fc_linux32   = gfortran -c -I../INC -fdollar-ok $(debug_flags) -fno-automatic -o $@
fc_solaris = gfortran -c -I../INC -fdollar-ok $(debug_flags) -fno-automatic -o $@
fc_macosx    = gfortran -m64 -c -I../INC -fdollar-ok $(debug_flags) -fno-automatic -o $@
fc_macosxppc = gfortran -c -I../INC -fdollar-ok $(debug_flags) -fno-automatic -o $@
fc_windows   = gfortran -c -I../INC -fdollar-ok $(debug_flags) -fno-automatic -o $@
#####fc_gfortran = gfortran -c -I../INC -fdollar-ok $(debug_flags) -fno-automatic -o $@

cc_linux64   = gcc -c -I../INC $(debug_flags) -o $@
cc_linux32   = gcc -c -I../INC $(debug_flags) -o $@
cc_solaris   = gcc -c -I../INC $(debug_flags) -o $@
cc_macosx    = gcc -m64 -c -I../INC -I/usr/X11R6/include $(debug_flags) -o $@
cc_macosxppc = gcc -c -I../INC -I/usr/X11R6/include $(debug_flags) -o $@
cc_windows   = gcc -c -I../INC $(debug_flags) -o $@
#####cc_gfortran = gcc -c -I../INC $(debug_flags) -o $@

fc = $(fc_$(SEISARCH))
cc = $(cc_$(SEISARCH))

######################
# network flag
nf_linux64 = -lwsock32
nf_linux32 = -lwsock32
nf_windows = -lwsock32
nf_solaris = -lwsock32
nf_macosx =
nf_macosxppc =
nf = $(nf_$(SEISARCH))
######################

##################################################
# FILES SPECFIC TO PLATFORM
linux64_ONLY   = $(OUTPATH)/comp_linux.o $(OUTPATH)/xlib.o
linux32_ONLY   = $(OUTPATH)/comp_linux.o $(OUTPATH)/xlib.o
solaris_ONLY   = $(OUTPATH)/comp_sun.o $(OUTPATH)/xlib.o
macosx_ONLY    = $(OUTPATH)/comp_linux.o $(OUTPATH)/xlib.o
macosxppc_ONLY = $(OUTPATH)/comp_sun.o $(OUTPATH)/xlib.o
windows_ONLY   = $(OUTPATH)/comp_pc.o $(OUTPATH)/dislinplot.o $(OUTPATH)/xget_screen_size_pc.o
#
#####gfortran_ONLY = $(OUTPATH)/comp_linux.o $(OUTPATH)/xlib.o

PLATFORM_DEP_OBJ = $($(SEISARCH)_ONLY)

##################################################
# NAME OF ARCHIVE
archive = seisan.a

# not included hypinv2.for

#
# LIST OF OBJECTS THAT ARE COMMON ON ALL PLATFORMS
# AND THAT ARE PART OF THE ARCHIVE
#
SUBS = $(OUTPATH)/abstim.o $(OUTPATH)/auto_tr.o \
  $(OUTPATH)/auto_amp.o $(OUTPATH)/autocoda.o \
  $(OUTPATH)/auto_proc.o  \
  $(OUTPATH)/autofil.o $(OUTPATH)/azibazi.o \
  $(OUTPATH)/bcd.o $(OUTPATH)/bndpas.o \
  $(OUTPATH)/check_s.o \
  $(OUTPATH)/compdecomp.o $(OUTPATH)/codecoutil.o  \
  $(OUTPATH)/componen.o \
  $(OUTPATH)/conv_def.o $(OUTPATH)/fft.o  \
  $(OUTPATH)/eev_sub.o $(OUTPATH)/err_ellipse.o \
  $(OUTPATH)/filename.o $(OUTPATH)/findchan.o \
  $(OUTPATH)/findevin.o $(OUTPATH)/focmec_exe_sub.o \
  $(OUTPATH)/fmap_subs.o \
  $(OUTPATH)/ga_lib.o $(OUTPATH)/get_baz.o \
  $(OUTPATH)/guralp.o \
  $(OUTPATH)/gmap_sub.o $(OUTPATH)/gmt.o $(OUTPATH)/gse_io.o \
  $(OUTPATH)/gserespl.o $(OUTPATH)/gsesei_lib.o \
  $(OUTPATH)/general.o \
  $(OUTPATH)/hypmain.o \
  $(OUTPATH)/hypmbn.o \
  $(OUTPATH)/hypoloc.o $(OUTPATH)/hypoloc1.o \
  $(OUTPATH)/hyposub1.o $(OUTPATH)/hyposub2.o \
  $(OUTPATH)/hyposub3.o $(OUTPATH)/hyposub4.o \
  $(OUTPATH)/hyposub6.o \
  $(OUTPATH)/hypo71sub.o $(OUTPATH)/make_hypoc_brief.o \
  $(OUTPATH)/iscloc_wrap.o  $(OUTPATH)/iscloc.o \
  $(OUTPATH)/inc_id.o $(OUTPATH)/indata.o \
  $(OUTPATH)/isf_isc.o  $(OUTPATH)/isfnor_lib.o \
  $(OUTPATH)/lsqlin.o $(OUTPATH)/libsei.o $(OUTPATH)/libmseed.o \
  $(OUTPATH)/lgstr.o $(OUTPATH)/maglib.o\
  $(OUTPATH)/maxlik.o $(OUTPATH)/mb_att.o \
  $(OUTPATH)/merge_f.o  $(OUTPATH)/mfhead.o \
  $(OUTPATH)/mul_spec.o \
  $(OUTPATH)/abstim.o \
  $(OUTPATH)/network.o \
  $(OUTPATH)/nortype.o $(OUTPATH)/picsub.o \
  $(OUTPATH)/plot_foc.o \
  $(OUTPATH)/random.o \
  $(OUTPATH)/polos.o $(OUTPATH)/quarrycheck.o \
  $(OUTPATH)/recfil.o  $(OUTPATH)/rea.o \
  $(OUTPATH)/rea2.o $(OUTPATH)/removedc.o \
  $(OUTPATH)/sei_mes.o $(OUTPATH)/seiplot.o \
  $(OUTPATH)/seisinc.o $(OUTPATH)/sfilname.o \
  $(OUTPATH)/seisan_logging.o \
  $(OUTPATH)/seismseed.o $(OUTPATH)/sfilname.o \
  $(OUTPATH)/shead.o $(OUTPATH)/sheads.o \
  $(OUTPATH)/sacsei_lib.o $(OUTPATH)/sig_spec.o \
  $(OUTPATH)/sacsubf.o $(OUTPATH)/seisanarch.o \
  $(OUTPATH)/spec_dist.o $(OUTPATH)/stat_loc.o \
  $(OUTPATH)/swap.o $(OUTPATH)/syntsel.o \
  $(OUTPATH)/sys_resp.o $(OUTPATH)/systime.o \
  $(OUTPATH)/svd.o $(OUTPATH)/timerout.o \
  $(OUTPATH)/xy_plot.o $(OUTPATH)/respfil.o \
  $(OUTPATH)/seis_sub.o $(OUTPATH)/sfil.o \
  $(OUTPATH)/tau.o $(OUTPATH)/text_sort.o \
  $(OUTPATH)/vincenty_wgs84.o \
  $(OUTPATH)/volcano.o $(OUTPATH)/wave.o 

#
# INCLUDE FILES
#
INCS = ../INC/*.inc ../INC/*.f ../INC/*INC 

info:
	@echo --------------------------------------------------
	@echo "SEISAN Makefile FOR LIB DIRETORY, OPTIONS ARE:"
	@echo " "
	@echo "   all - compiles all, and creates archive"
	@echo "   distribution - like all, but without debug flag"
	@echo "   $(OUTPATH)/<object> - create object"
	@echo "   $(OUTPATH)/$(archive) - create archive"
	@echo "   clean - remove executables"
	@echo " "
	@echo "OUTPUT PATH IS SET TO $(OUTPATH) "
	@echo " "
	@echo "The platform is set through the environmental "
	@echo "variable SEISARCH. The following are supported:"
	@echo "linux64, linux32, solaris, macosx, macosxppc, windows"
	@echo "e.g. type: setenv SEISARCH linux32 (csh)"
	@echo "setenv SEISARCH linux32"
	@echo "make all"
	@echo "to compile for a linux 32 bit computer."
	@echo --------------------------------------------------
	@echo "$(test)"

all: output $(OUTPATH) $(PLATFORM_DEP_OBJ) $(SUBS) $(NON_ARCHIVE_SUBS) \
     $(OUTPATH)/$(archive) 

distribution: all

output:
	@echo -------------------------------------------------
	@echo ---- COMPILING SEISAN LIBRARIES -----------------
	@echo -------------------------------------------------

linux64: 
	mkdir linux64
linux32: 
	mkdir linux32
solaris: 
	mkdir solaris
windows: 
	mkdir windows
macosx:
	mkdir macosx
macosxppc:
	mkdir macosxppc
gfortran: 
	mkdir gfortran

#
# fortran object files
#
$(OUTPATH)/abstim.o: abstim.for $(INCS)
	$(fc) abstim.for 

$(OUTPATH)/auto_proc.o: auto_proc.for $(INCS)
	$(fc) auto_proc.for 

$(OUTPATH)/auto_tr.o: auto_tr.for $(INCS)
	$(fc) auto_tr.for 

$(OUTPATH)/auto_amp.o: auto_amp.for $(INCS)
	$(fc) auto_amp.for

$(OUTPATH)/autocoda.o: autocoda.for $(INCS)
	$(fc) autocoda.for

$(OUTPATH)/autofil.o: autofil.for $(INCS)
	$(fc) autofil.for

$(OUTPATH)/azibazi.o: azibazi.for $(INCS)
	$(fc) azibazi.for

$(OUTPATH)/bcd.o: bcd.for $(INCS)
	$(fc) bcd.for

$(OUTPATH)/bndpas.o: bndpas.for $(INCS)
	$(fc) bndpas.for

$(OUTPATH)/check_s.o: check_s.for $(INCS)
	$(fc) check_s.for

$(OUTPATH)/codecoutil.o: codecoutil.for $(INCS)
	$(fc) codecoutil.for

$(OUTPATH)/compdecomp.o: compdecomp.for $(INCS)
	$(fc) compdecomp.for

$(OUTPATH)/comp_linux.o: comp_linux.for $(INCS)
	$(fc) comp_linux.for

$(OUTPATH)/comp_pc.o: comp_pc.for $(INCS)
	$(fc) comp_pc.for

$(OUTPATH)/comp_sun.o: comp_sun.for $(INCS)
	$(fc) comp_sun.for

$(OUTPATH)/componen.o: componen.for $(INCS)
	$(fc) componen.for

$(OUTPATH)/conv_def.o: conv_def.for $(INCS)
	$(fc) conv_def.for

$(OUTPATH)/dislinplot.o: dislinplot.for $(INCS)
	$(fc) dislinplot.for

$(OUTPATH)/err_ellipse.o: err_ellipse.for $(INCS)
	$(fc) err_ellipse.for

$(OUTPATH)/eev_sub.o: eev_sub.for $(INCS)
	$(fc) eev_sub.for

$(OUTPATH)/fft.o: fft.for $(INCS)
	$(fc) fft.for

$(OUTPATH)/filename.o: filename.for $(INCS)
	$(fc) filename.for

$(OUTPATH)/findchan.o: findchan.for $(INCS)
	$(fc) findchan.for

$(OUTPATH)/fmap_subs.o: fmap_subs.for $(INCS)
	$(fc) fmap_subs.for

$(OUTPATH)/findevin.o: findevin.for $(INCS)
	$(fc) findevin.for

$(OUTPATH)/focmec_exe_sub.o: focmec_exe_sub.for
	$(fc) focmec_exe_sub.for

$(OUTPATH)/ga_lib.o: ga_lib.for $(INCS)
	$(fc) ga_lib.for

$(OUTPATH)/general.o: general.for $(INCS)
	$(fc) general.for
	
$(OUTPATH)/get_baz.o: get_baz.for $(INCS)
	$(fc) get_baz.for
	
$(OUTPATH)/gmap_sub.o: gmap_sub.for $(INCS)
	$(fc) gmap_sub.for

$(OUTPATH)/gmt.o: gmt.for $(INCS)
	$(fc) gmt.for

$(OUTPATH)/getenv2.o: getenv2.for
	$(fc) getenv2.for
	
$(OUTPATH)/gse_io.o: gse_io.for $(INCS)
	$(fc) gse_io.for

$(OUTPATH)/gsesei_lib.o: gsesei_lib.for $(INCS)
	$(fc) gsesei_lib.for

$(OUTPATH)/gserespl.o: gserespl.for $(INCS)
	$(fc) gserespl.for

$(OUTPATH)/guralp.o: guralp.for $(INCS)
	$(fc) guralp.for

$(OUTPATH)/hypinv1.o: hypinv1.for $(INCS)
	$(fc) hypinv1.for

$(OUTPATH)/hypinv2.o: hypinv2.for $(INCS)
	$(fc) hypinv2.for

$(OUTPATH)/hypinv3.o: hypinv3.for $(INCS)
	$(fc) hypinv3.for

$(OUTPATH)/hypoloc.o: hypoloc.for $(INCS)
	$(fc) hypoloc.for

$(OUTPATH)/hypoloc1.o: hypoloc1.for $(INCS)
	$(fc) hypoloc1.for

$(OUTPATH)/hypmain.o: hypmain.for $(INCS)
	$(fc) hypmain.for

$(OUTPATH)/hypmbn.o: hypmbn.for $(INCS)
	$(fc) hypmbn.for

$(OUTPATH)/hyposub1.o: hyposub1.for $(INCS)
	$(fc) hyposub1.for

$(OUTPATH)/hyposub2.o: hyposub2.for $(INCS)
	$(fc) hyposub2.for

$(OUTPATH)/hyposub3.o: hyposub3.for $(INCS)
	$(fc) hyposub3.for

$(OUTPATH)/hyposub4.o: hyposub4.for $(INCS)
	$(fc) hyposub4.for

$(OUTPATH)/hyposub6.o: hyposub6.for $(INCS)
	$(fc) hyposub6.for

$(OUTPATH)/hypo71sub.o: hypo71sub.for
	$(fc) hypo71sub.for

$(OUTPATH)/iscloc_wrap.o: iscloc_wrap.for $(INCS)
	$(fc) iscloc_wrap.for

$(OUTPATH)/inc_id.o: inc_id.for $(INCS)
	$(fc) inc_id.for

$(OUTPATH)/indata.o: indata.for $(INCS)
	$(fc) indata.for

$(OUTPATH)/isf_isc.o: isf_isc.for $(INCS)
	$(fc) isf_isc.for

$(OUTPATH)/isfnor_lib.o: isfnor_lib.for $(INCS)
	$(fc) isfnor_lib.for

$(OUTPATH)/libsei.o: libsei.for $(INCS)
	$(fc) libsei.for

$(OUTPATH)/lgstr.o: lgstr.for $(INCS)
	$(fc) lgstr.for

$(OUTPATH)/lsqlin.o: lsqlin.for $(INCS)
	$(fc) lsqlin.for

$(OUTPATH)/make_hypoc_brief.o: make_hypoc_brief.for $(INCS)
	$(fc) make_hypoc_brief.for

$(OUTPATH)/maxlik.o: maxlik.for $(INCS)
	$(fc) maxlik.for

$(OUTPATH)/maglib.o: maglib.for $(INCS)
	$(fc) maglib.for

$(OUTPATH)/mb_att.o: mb_att.for $(INCS)
	$(fc) mb_att.for

$(OUTPATH)/merge_f.o: merge_f.for $(INCS)
	$(fc) merge_f.for

$(OUTPATH)/mfhead.o: mfhead.for $(INCS)
	$(fc) mfhead.for

$(OUTPATH)/mul_spec.o: mul_spec.for $(INCS)
	$(fc) mul_spec.for

$(OUTPATH)/nansei.o: nansei.for $(INCS)
	$(fc) nansei.for

$(OUTPATH)/nortype.o: nortype.for $(INCS)
	$(fc) nortype.for

$(OUTPATH)/picsub.o: picsub.for $(INCS)
	$(fc) picsub.for

$(OUTPATH)/plot_foc.o: plot_foc.for $(INCS)
	$(fc) plot_foc.for

$(OUTPATH)/polos.o: polos.for $(INCS)
	$(fc) polos.for

$(OUTPATH)/quarrycheck.o: quarrycheck.for
	$(fc) quarrycheck.for
	
$(OUTPATH)/random.o: random.for $(INCS)
	$(fc) random.for

$(OUTPATH)/rea.o: rea.for $(INCS)
	$(fc) rea.for

$(OUTPATH)/rea2.o: rea2.for $(INCS)
	$(fc) rea2.for

$(OUTPATH)/recfil.o: recfil.for $(INCS)
	$(fc) recfil.for

$(OUTPATH)/removedc.o: removedc.for $(INCS)
	$(fc) removedc.for

$(OUTPATH)/sacsei_lib.o: sacsei_lib.for $(INCS)
	$(fc) sacsei_lib.for

$(OUTPATH)/sacsubf.o: sacsubf.for $(INCS)
	$(fc) sacsubf.for

$(OUTPATH)/sei_mes.o: sei_mes.for $(INCS)
	$(fc) sei_mes.for

$(OUTPATH)/seisan_logging.o: seisan_logging.for $(INCS)
	$(fc) seisan_logging.for

$(OUTPATH)/sig_spec.o: sig_spec.for $(INCS)
	$(fc) sig_spec.for

$(OUTPATH)/seiplot.o: seiplot.for $(INCS)
	$(fc) seiplot.for

$(OUTPATH)/seisinc.o: seisinc.for $(INCS)
	$(fc) seisinc.for

$(OUTPATH)/sfilname.o: sfilname.for $(INCS)
	$(fc) sfilname.for

$(OUTPATH)/shead.o: shead.for $(INCS)
	$(fc) shead.for

$(OUTPATH)/sheads.o: sheads.for $(INCS)
	$(fc) sheads.for

$(OUTPATH)/spec_dist.o: spec_dist.for $(INCS)
	$(fc) spec_dist.for

$(OUTPATH)/stat_loc.o: stat_loc.for $(INCS)
	$(fc) stat_loc.for

$(OUTPATH)/svd.o: svd.for $(INCS)
	$(fc) svd.for

$(OUTPATH)/swap.o: swap.for $(INCS)
	$(fc) swap.for

$(OUTPATH)/syntsel.o: syntsel.for $(INCS)
	$(fc) syntsel.for

$(OUTPATH)/sys_resp.o: sys_resp.for $(INCS)
	$(fc) sys_resp.for

$(OUTPATH)/systime.o: systime.for $(INCS)
	$(fc) systime.for

$(OUTPATH)/tau.o: tau.for $(INCS)
	$(fc) tau.for

$(OUTPATH)/text_sort.o: text_sort.for $(INCS)
	$(fc) text_sort.for

$(OUTPATH)/timerout.o: timerout.for $(INCS)
	$(fc) timerout.for

$(OUTPATH)/vincenty_wgs84.o: vincenty_wgs84.for $(INCS)
	$(fc) vincenty_wgs84.for

$(OUTPATH)/volcano.o: volcano.for $(INCS)
	$(fc) volcano.for

$(OUTPATH)/wave.o: wave.for $(INCS)
	$(fc) wave.for

$(OUTPATH)/xy_plot.o: xy_plot.for $(INCS)
	$(fc) xy_plot.for

 
#
# c object files
#
#
$(OUTPATH)/network.o: network.c
	$(cc) -lwsock32 -c network.c

$(OUTPATH)/libmseed.o: libmseed.c
	$(cc) libmseed.c -O2 -Wall -fPIC

$(OUTPATH)/respfil.o: respfil.c
	$(cc) respfil.c

$(OUTPATH)/seis_sub.o: seis_sub.c
	$(cc) seis_sub.c

$(OUTPATH)/seisanarch.o: seisanarch.c
	$(cc) seisanarch.c 

$(OUTPATH)/sfil.o: sfil.c
	$(cc) sfil.c

$(OUTPATH)/xget_screen_size_pc.o: xget_screen_size_pc.c
	$(cc) xget_screen_size_pc.c

$(OUTPATH)/xlib.o: xlib.c
	$(cc) xlib.c 

$(OUTPATH)/seismseed.o:seismseed.c
	$(cc) seismseed.c  

$(OUTPATH)/iscloc.o: iscloc.c $(INCS) \
    ../INC/iscloc.h  ../INC/iscloc_jb_model.h  ../INC/iscloc_jb_tables.h 
	$(cc) iscloc.c

#
# make archive
#
$(OUTPATH)/$(archive): $(SUBS) $(INCS) $(PLATFORM_DEP_OBJ)
	ar cr $(OUTPATH)/$(archive) $(SUBS) $(PLATFORM_DEP_OBJ)
	ranlib $(OUTPATH)/$(archive)

# 
# delete object files with option clean
#
clean:
	rm -f $(OUTPATH)/*.o
	rm -f $(OUTPATH)/seisan.a 

