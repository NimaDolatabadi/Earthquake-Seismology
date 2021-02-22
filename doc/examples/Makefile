

FFLAGS=-Wall `sac-config --cflags`
CFLAGS=-Wall `sac-config --cflags`
SACLIB=`sac-config --libs sac`
SACIO=`sac-config --libs sacio`
LDLIBS+=$(SACIO)

PROGS_IO = rsac1c rsac1f \
	   rsac2c rsac2f \
	   gethvc gethvf \
	   wsac1c wsac1f \
	   wsac2c wsac2f \
	   wsac3c wsac3f \
	   wsac4c wsac4f \
	   wsac5c wsac5f 

PROGS_LIB = filterf filterc \
	    envelopef envelopec \
	    correlatec correlatef \
	    convolvef convolvec 


all: 
	( LDLIBS=''          $(MAKE) io   )
	( LDLIBS='$(SACLIB)' $(MAKE) lib  )
	( LDLIBS=''          $(MAKE) test )

io: $(PROGS_IO)
lib: $(PROGS_LIB)

test:
	./create_input.sh
	./filterf
	./filterc
	diff filterf_out_sac.sac filterf_out.sac 
	diff filterc_out_sac.sac filterc_out.sac 
	./envelopef
	./envelopec
	diff envelopef_out_sac.sac envelopef_out.sac
	diff envelopec_out_sac.sac envelopec_out.sac
	./convolvef
	./convolvec
	diff convolvef_out_sac1.sac convolvef_out1.sac
	diff convolvec_out_sac1.sac convolvec_out1.sac
	./correlatef
	./correlatec
	diff correlatef_out_sac1.sac correlatef_out1.sac
	diff correlatec_out_sac1.sac correlatec_out1.sac


clean:
	rm -f $(PROGS_IO) $(PROGS_LIB) *.o *~ core
	rm -f *.sac

