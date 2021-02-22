/*********************************************************************
*	sacAlign.c:
*	  save sac traces aligned with a time mark
*
*	Author:  Lupei Zhu
*
*	Revision History
*		March 1999	Initial coding
*		June  2000	include other time marks option
*      New Author: Zhigang Peng
*      11/01/00 20:16:29
*********************************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sac.h"
#include "Complex.h"

int main(int argc, char **argv) {
  int 		i, j, nn, shift, error, start, end, ntrace, nrdc;
  char		outa[10], outf[64], rdc;
  float 	ar, dt, *src, *trace, reduce_vel, align;
  SACHEAD	hd, *hd0;
  
  error = 0;
  rdc = 't'; nrdc = -5;		/* default: align with begining */
  outf[0] = '\0';
  /* input parameters */
  for (i=1; !error && i < argc; i++) {
    if (argv[i][0] == '-') {
       switch(argv[i][1]) {
  
       case 'O':
	 strcpy(outa,&argv[i][2]);
         break;
  
       case 'E':
	 rdc=argv[i][2];
	 if (rdc == 't') sscanf(&argv[i][3],"%d",&nrdc);
	 else sscanf(&argv[i][2],"%f",&reduce_vel);
	 break;

       default:
	 error = 1;
	 break;

       }
    }
  }

  if (argc < 3 || error || outa[0] == '\0') {
     fprintf(stderr,"usage: %s [-E(t(0-9,-5(b),-3(o),-2(a))|vel)] -Ooutput_file_appendix sac_traces\n",argv[0]);
     return -1;
  }

  ntrace = 0;
  for(i=1;i<argc;i++) {

    if (argv[i][0] == '-') continue;
    if ( (trace=read_sac(argv[i],&hd)) == NULL 
	 || (nrdc == -2 && hd.a < -12340.) 
	 || (rdc != 't' && hd.dist < -12340.)  ) {
	   fprintf(stderr,"error opening %s or missing head info\n",argv[i]);
	   continue;
    }
    if (rdc == 't') 
       align = *((float *)(&hd) + 10 + nrdc) - hd.b;
    else
       align = hd.dist/reduce_vel - hd.b;

    ntrace++;
    /*      if (ntrace == 1) { */
    nn = hd.npts;
    dt = hd.delta;
    ar = align;
    src=(float *)malloc(nn*sizeof(float));
    hd0=(SACHEAD *)malloc(sizeof(SACHEAD));
    assert(src != NULL && hd0 != NULL);
    memcpy(src, trace, nn*sizeof(float));
    memcpy(hd0, &hd, sizeof(SACHEAD));
/*      } else { */
       /* stacking */
    shift = rint((ar-align)/dt);
    start = shift;		if (start<0) start = 0;
    end   = hd.npts+shift;	if (end>nn) end = nn;
    for (j=start; j<end; j++)
      src[j] = trace[j-shift];
    free(trace);
       /*      } */
    hd.npts = nn;
    sprintf(outf,"%s.%s",argv[i],outa);
    printf("output file name are %s\n",outf);
    write_sac(outf, *hd0, src);  
  }
  
  return 0;
  
}
