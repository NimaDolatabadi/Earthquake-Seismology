/********************************************************
*	Calculate SAC data average value within specified time window
*	Usage:
*		sac_ave t1 t2 sac_files ...
*	Modified from lsac2, by Zhigang Peng, 
*	Initial coding: Sat May  1 13:13:51 PDT 2004
********************************************************/

#include <stdio.h>
#include <math.h>
#include "sac.h"

int main(int argc, char **argv) {
  SACHEAD	hd;
  int		i,j,n1,n2;
  float		*ar;
  float		t1, t2, t, std, mean;

  if (argc < 4) {
     fprintf(stderr, "Usage: %s t1 t2 sac_files ...\n\
  Calculate SAC data average value within specified time window\n",argv[0]);
     return -1;
  }

  sscanf(argv[1],"%f",&t1);
  sscanf(argv[2],"%f",&t2);
  for (i=3;i<argc;i++) {
     std = 0;
     mean = 0;

     if ((ar = read_sac(argv[i],&hd)) == NULL) continue;
     n1= (int) ((t1-hd.b)/hd.delta);if(n1<1) n1=1;
     n2= (int) ((t2-hd.b)/hd.delta);if(n2>hd.npts-2) n2=hd.npts-2;
     if (n1>n2) {
        fprintf(stderr,"no time window for %s\n",argv[i]);
	continue;
     }
     t = n2-n1+0.;
/*     printf("%d %d %f\n",n2,n1,t); */
     for(j=n1;j<n2;j++) {
	mean +=ar[j];	
/*	printf("# %d %11.6f %11.6f\n",j,ar[j],mean); */
     }
     mean = mean/t;
/*     printf("%f %11.6f\n",t,mean); */
     for(j=n1;j<n2;j++) {
	std +=(ar[j]-mean)*(ar[j]-mean);
     }
     std = sqrt(std/(t-1));
     printf("%s %11.6f %11.6f\n", argv[i],mean,std);
  }

  return 0;
}
