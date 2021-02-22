/********************************************************
*	read 1-column data from stdin and write them as
*	evenly spaced SAC data.
*	usage:
*		col2sac sac_file_name delta_t t0
*********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "sac.h"

int main(int argc, char **argv) {
  int i,n,block=512;
  float *aa,dt,t0;
  SACHEAD hd;
  char	line[512];

  if (argc != 4) {
     fprintf(stderr,"Usage: col2sac sac_file_name delta_t t0\n");
     return 1;
  }

  sscanf(argv[2],"%f",&dt);
  sscanf(argv[3],"%f",&t0);
  n = block;
  aa = (float *) calloc(n,sizeof(float));
  i = 0;
  while (fgets(line,512,stdin)) {
    sscanf(line,"%f",aa+i);
    i++;
    if (i==n) {
       n += block; 
       aa = realloc(aa, n*sizeof(float));
    }
  }

  hd = sachdr(dt,i,t0);
  return write_sac(argv[1],hd,aa);

}
