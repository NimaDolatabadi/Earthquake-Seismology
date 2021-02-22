/***************************************************************
*	Dump SAC binary files to stdout
*	Usage:
*	sacdump sac_files ...
*       Modified from Lupei's code sac2col
*       Zhigang peng, 10/17/00 13:16:53
***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "sac.h"

int main(int argc, char **argv) {
  int		i,j;
  float		*data;
  SACHEAD	hd;

  if ( argc < 2 ) {
     fprintf(stderr, "Usage: %s sac_files ...\n",argv[0]);
     return -1;
  }

  for(i=1;i<argc;i++){

       if ( (data = read_sac(argv[i], &hd)) == NULL ) {
	  continue;
       }

       switch (hd.iftype) {

         case ITIME:
/*         		printf("DATA %s %f %f\n",argv[i], hd.delta, hd.b); */
	  	for(j=0;j<hd.npts;j++)
		   printf("%f %f\n",hd.b+hd.delta*j,data[j]);
		break;

	 case IXY:
		printf("DATA %s %f\n",argv[i]);
		for(j=0;j<hd.npts;j++)
		   printf("%e %e\n",data[j],data[j+hd.npts]);
		break;

	 default:
	 	fprintf(stderr,"not ITIME/IXY type %s\n",argv[i]);
       }

       free(data);

  }

  return 0;

}
