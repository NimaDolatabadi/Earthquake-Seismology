/********************************************************
*	Show SAC data max. and min amplitude withing specified time window
*	Usage:
*		lsac2 t1 t2 sac_files ...
*	Modified by Zhigang Peng, Tue Apr 17 17:29:23 PDT 2001
*	By adding abs to the max amplitude
********************************************************/

#include <stdio.h>
#include <math.h>
#include "sac.h"

int main(int argc, char **argv) {
  SACHEAD	hd;
  int		i,j,n,n1,n2;
  float		*ar;
  float		t1, t2, am, t, sigma;

  if (argc < 4) {
     fprintf(stderr, "Usage: lsac_minmax t1 t2 sac_files ...\n");
     return -1;
  }

  sscanf(argv[1],"%f",&t1);
  sscanf(argv[2],"%f",&t2);
  for (i=3;i<argc;i++) {
     if ((ar = read_sac(argv[i],&hd)) == NULL) continue;
     n1= (int) ((t1-hd.b)/hd.delta);if(n1<1) n1=1;
     n2= (int) ((t2-hd.b)/hd.delta);if(n2>hd.npts-2) n2=hd.npts-2;
     if (n1>n2) {
        fprintf(stderr,"no time window for %s\n",argv[i]);
	continue;
     }
     for(am=ar[n1],n=j=n1;j<n2;j++) {
        if (ar[j]<am) continue;
        am = ar[j];
        n = j;
     }
     sigma = 1./(ar[n-1]+ar[n+1]-2*ar[n]);
     t = 0.5*(ar[n+1]-ar[n-1])*sigma;
     am = am - 0.5*t*t/sigma;
     printf("%s %8.2f %8.2e %8.2f\n", argv[i],(n-t)*hd.delta+hd.b,am,sqrt(-sigma)*hd.delta);

     for(am=ar[n1],n=j=n1;j<n2;j++) {
        if (ar[j]>am) continue;
        am = ar[j];
        n = j;
     }
     sigma = 1./(ar[n-1]+ar[n+1]-2*ar[n]);
     t = 0.5*(ar[n+1]-ar[n-1])*sigma;
     am = am - 0.5*t*t/sigma;
     printf("%s %8.2f %8.2e %8.2f\n", argv[i],(n-t)*hd.delta+hd.b,am,sqrt(-sigma)*hd.delta);
  }

  return 0;
}
