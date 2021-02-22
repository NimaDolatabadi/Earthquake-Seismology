CFLAGS = -O
#
SAC = col2sac gsact lsac0 lsac lsac1 lsac2 sac2col sac_ave sacdump sacdump_slice sac_e saclst sac_me sac_snr sacStack lsac_minmax distaz distbaz ndistbaz

all: $(SAC) $(SAC2)
	rm *.o

$(SAC): %:%.o sacio.o fft.o Complex.o
	${LINK.c} -o $@ $@.o sacio.o fft.o Complex.o -lm
