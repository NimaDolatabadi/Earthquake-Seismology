#!/bin/sh

# filter
sac <<EOF
fg seismo
write filterc_in.sac
write filterf_in.sac
bp co 0.1 1.00 p 2 n 4
write filterc_out_sac.sac
write filterf_out_sac.sac
quit
EOF

# envelope
sac <<EOF
fg seismo
write envelopec_in.sac
write envelopef_in.sac
envelope
write envelopec_out_sac.sac
write envelopef_out_sac.sac
quit
EOF

# correlate
sac <<EOF
fg seismo
write correlatec_in1.sac
ch b 0.0
write correlatec_in2.sac
cuterr fillz 
cut 0 20
read  correlatec_in1.sac correlatec_in2.sac
ch b 0.0
write correlatec_in1.sac correlatec_in2.sac
write correlatef_in1.sac correlatef_in2.sac
cut off
correlate
write correlatec_out_sac0.sac correlatec_out_sac1.sac
write correlatef_out_sac0.sac correlatef_out_sac1.sac
quit
EOF

# Convolution
sac <<EOF
fg triangle delta 1e-2 npts 100 
write convolvec_in2.sac
write convolvef_in2.sac
fg seismo
rmean
write convolvec_in1.sac
write convolvef_in1.sac
read convolvec_in1.sac convolvec_in2.sac
convolve
write convolvec_out_sac0.sac convolvec_out_sac1.sac
write convolvef_out_sac0.sac convolvef_out_sac1.sac
quit
EOF

