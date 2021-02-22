/***************************************************************
*	Dump SAC binary files to stdout
*	Usage:
*	 sac2col sac_files ...
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
       		printf("DATA %s %f %f\n",argv[i], hd.delta, hd.b);
	  	for(j=0;j<hd.npts;j++)
		   printf("%e\n",data[j]);
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
