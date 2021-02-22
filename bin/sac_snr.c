/********************************************************
*	Calculate signal to noise ratio for SAC data 
*	within specified time window
*	modified from sac_e
*	Usage:
*		sac_e -A(0-9,-5(b),-3(o),-2(a)) -Ssingal_start_time -Nnoise_start_time
*		-Wtime_window sac_files ...
*		Note: negativ noise or signal start time means shifting
*		to the left relative to the alignment (b, o, a, t0, etc)
*	Modified from lsac2, by Zhigang Peng, Fri Aug  9 11:13:54 PDT 2002
********************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sac.h"
#include "Complex.h"

int main(int argc, char **argv) {
  SACHEAD	hd;
  int		i,j,nt1,nt2,st1,st2, error,nrdc, p_arr, s_arr;
  float		*trace, align;
  float		twin, sst, nst, ntwin, stwin, signal, signal_mean, noise, noise_mean, snr;

  error = 0;
  nrdc = -5; /* default, align from begining */
  /* input parameters */
  for (i=1; !error && i < argc; i++) {
    if (argv[i][0] == '-') {
       switch(argv[i][1]) {

       case 'S':
  	 sscanf(&argv[i][2],"%f",&sst);
         break;

       case 'N':
  	 sscanf(&argv[i][2],"%f",&nst);
         break;

       case 'W':
  	 sscanf(&argv[i][2],"%f",&twin);
         break;

       case 'A':
         sscanf(&argv[i][2],"%d",&nrdc);
         break;

       default:
         error = 1;
         break;

       }
    }
  }

  if (argc < 4 || error) {
     fprintf(stderr, "Usage: %s -A(0-9,-5(b),-3(o),-2(a)) -Ssingal_start_time \n\
  -Nnoise_start_time -Wtime_window sac_files \n\
  Calculate signal to noise ratio for SAC data \n\
  within specified time window \n",argv[0]);
     return -1;
  }

  for (i=1;i<argc;i++) {
     if (argv[i][0] == '-') continue;
     if ((trace = read_sac(argv[i],&hd)) == NULL ||
	hd.o < -12340. || hd.a < -12340. ) {
           fprintf(stderr,"error opening %s or missing head info\n",argv[i]);
	   continue;
     }

     align = *((float *)(&hd) + 10 + nrdc) - hd.b;
     p_arr = (int) ((*((float *)(&hd) + 10 -2) - hd.b)/hd.delta);
     s_arr = (int) ((*((float *)(&hd) + 10 +0) - hd.b)/hd.delta);
     nt1= (int) ((align+nst)/hd.delta);if(nt1<1) nt1=1;
     nt2= (int) ((align+nst+twin)/hd.delta);if(nt2>hd.npts-2) nt2=hd.npts-2;
     st1= (int) ((align+sst)/hd.delta);if(st1<1) st1=1;
     st2= (int) ((align+sst+twin)/hd.delta);if(st2>hd.npts-2) st2=hd.npts-2;
     if (nt1>nt2 || st1 > st2) {
        fprintf(stderr,"time window for the noise or signal is less than 1 for %s\n",argv[i]);
	continue;
     }else if ((nt2>st1 && nt2<st2) || (nt1>st1 && nt1<st2)) {
/*        fprintf(stderr,"Alert, signal and noise time window overlap for %s\n",argv[i]);
 */
     }else if ((nrdc == -2 && st2>s_arr) || (nrdc == 0 && st1<p_arr)) {
/*       fprintf(stderr,"Alert, signal window including both P and S arrivals %s\n",argv[i]); */
     }else if ((p_arr>nt1 && p_arr<nt2) || (s_arr>nt1 && s_arr<nt2)) {
/*        fprintf(stderr,"Alert, noise window including either P or S arrivals %s\n",argv[i]);
*/
     }
     stwin = st2-st1+1.;
     signal = 0;
     signal_mean = 0;
     for(j=st1;j<st2;j++) {
	signal +=trace[j]*trace[j];	
	signal_mean +=trace[j];	
     }
     signal_mean = signal_mean/stwin;
     signal = signal/stwin;
     signal -= signal_mean*signal_mean;
     signal = sqrt(signal);

     ntwin = nt2-nt1+1.;
     noise = 0;
     noise_mean = 0;
     for(j=nt1;j<nt2;j++) {
	noise +=trace[j]*trace[j];	
	noise_mean +=trace[j];	
     }
     noise_mean = noise_mean/ntwin;
     noise = noise/ntwin;
     noise -= noise_mean*noise_mean;
     noise = sqrt(noise);

/*     snr = signal/(noise*1.414); */
     snr = signal/noise;
     printf("%s %7.4f %e %e\n", argv[i],snr,signal,noise);
  }

  return 0;
}
