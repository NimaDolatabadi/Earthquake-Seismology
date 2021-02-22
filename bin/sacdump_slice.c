/***************************************************************
*	Dump SAC binary files to stdout
*	Usage:
*	sacdump_slice sac_files ...
*       Modified from Lupei's code sac2col
*	Update: this version of sacdump_slice
*	Can extract part of instead of whole data
*       Zhigang peng, Wed Mar  3 10:44:59 PST 2004
***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "sac.h"

int main(int argc, char **argv) {
  int		i,j,n1,n2;
  float		*data,t1,t2;
  SACHEAD	hd;

  if ( argc < 3 ) {
     fprintf(stderr, "Usage: %s t1 t2 sac_files ...\n",argv[0]);
     return -1;
  }

  sscanf(argv[1],"%f",&t1);
  sscanf(argv[2],"%f",&t2);

  for(i=3;i<argc;i++){

       if ( (data = read_sac(argv[i], &hd)) == NULL ) {
	  continue;
       }

     n1= (int) ((t1-hd.b)/hd.delta);if(n1<1) n1=1;
     n2= (int) ((t2-hd.b)/hd.delta);if(n2>hd.npts-2) n2=hd.npts-2;

       switch (hd.iftype) {

         case ITIME:
/*         		printf("DATA %s %f %f\n",argv[i], hd.delta, hd.b); */
	  	for(j=n1;j<n2;j++)
		   printf("%e %e\n",hd.b+hd.delta*j,data[j]);
		break;

	 case IXY:
/*		printf("DATA %s %f\n",argv[i]); */
		for(j=n1;j<n2;j++)
		   printf("%e %e\n",data[j],data[j+hd.npts]);
		break;

	 default:
	 	fprintf(stderr,"not ITIME/IXY type %s\n",argv[i]);
       }

       free(data);

  }

  return 0;

}
