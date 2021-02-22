/*********************************************************************
*	sacStack.c:
*	 stack sac traces aligned with a time mark
*
*	Author:  Lupei Zhu
*
*	Revision History
*		March 1999	Initial coding
*		June  2000	include other time marks option
*		Oct 26, 2000	add normalization option
*********************************************************************/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sac.h"
#include "Complex.h"

int main(int argc, char **argv) {
  int 		i,j,nn,shift,error,start,end,ntrace,nrdc,norm,setbaz;
  char		outf[64], rdc;
  float 	ar,dt,*src,*trace,reduce_vel,align,maxA,baz,p;
  SACHEAD	hd,*hd0;
  
  error = 0;
  rdc = 't'; nrdc = -5;		/* default: align with begining */
  norm = 0;			/* no nomalization */
  outf[0] = '\0';
  setbaz = 0;
  /* input parameters */
  for (i=1; !error && i < argc; i++) {
    if (argv[i][0] == '-') {
       switch(argv[i][1]) {
  
       case 'O':
	 strcpy(outf,&argv[i][2]);
         break;
  
       case 'E':
	 rdc=argv[i][2];
	 if (rdc == 't') sscanf(&argv[i][3],"%d",&nrdc);
	 else sscanf(&argv[i][2],"%f",&reduce_vel);
	 break;

       case 'N':
	 norm = 1;
	 break;

       case 'S':
	 setbaz = 1;
	 sscanf(&argv[i][2],"%f/%f",&baz,&p);
	 break;

       default:
	 error = 1;
	 break;

       }
    }
  }

  if (argc < 3 || error || outf[0] == '\0') {
     fprintf(stderr,"usage: %s [-E(t(0-9,-5(b),-3(o),-2(a))|vel)] [-N] [-Sbaz/p] -Ooutput_file sac_traces\n\
		-E: align with a time mark or with an apparent velocity (b)\n\
		-N: normalize (off)\n\
		-S: set baz and user0 (p) in head\n",argv[0]);
     return -1;
  }

  ntrace = 0;
  for(i=1;i<argc;i++) {

    if (argv[i][0] == '-') continue;
    if ( (trace=read_sac(argv[i],&hd)) == NULL 
	|| (nrdc == -2 && hd.a < -12340.)
	|| (rdc != 't' && hd.dist < -12340.) ) {
	   fprintf(stderr,"error opening %s or missing head info\n",argv[i]);
	   continue;
    }
    if (rdc == 't') 
       align = *((float *)(&hd) + 10 + nrdc) - hd.b;
    else
       align = hd.dist/reduce_vel - hd.b;

    ntrace++;
    if (ntrace == 1) {
       nn = hd.npts;
       dt = hd.delta;
       ar = align;
       src=(float *)malloc(nn*sizeof(float));
       hd0=(SACHEAD *)malloc(sizeof(SACHEAD));
       assert(src != NULL && hd0 != NULL);
       memcpy(hd0, &hd, sizeof(SACHEAD));
       hd0->evla = -12345.;
       hd0->evlo = -12345.;
       hd0->dist = 0.;
       hd0->baz = 0.;
       hd0->user0 = 0.;
       for(j=0;j<nn;j++) src[j] = 0.;
    }

    /* stacking */
    shift = rint((ar-align)/dt);
    start = shift;		if (start<0) start = 0;
    end   = hd.npts+shift;	if (end>nn) end = nn;
    maxA = 1.;
    if (norm) {
       maxA = 0.;
       for(j=start;j<end;j++) {
	  if (trace[j-shift]>maxA) maxA = trace[j-shift];
       }
    }
    hd0->dist += hd.dist;
    hd0->baz += hd.baz;
    hd0->user0 += hd.user0;
    for (j=start; j<end; j++)
      src[j] += trace[j-shift]/maxA;
    free(trace);

  }
  
  if (ntrace<1) return 0;

  ar = 1./ntrace;
  hd0->dist *= ar;
  hd0->baz *= ar;
  hd0->user0 *= ar;
  if (setbaz) {
     hd0->baz = baz;
     hd0->user0 = p;
  }
  for(j=0;j<nn;j++) src[j] *= ar;
  hd.npts = nn;
  write_sac(outf, *hd0, src);
  
  return 0;

}
