
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "sac.h"
#include "sacio.h"

#define MAX 1000

int 
main(int argc, char *argv[]) {

    /* Local variables */
    int nlen, nerr, max;

    float beg, delta;
    char *kname;

    float yarray[1000];
    float xarray[1];
    float *yenv;

    max = MAX;

    /*     Define the Maximum size of the data Array 
     *     Define the Data Array of size MAX 
     *     Declare Variables used in the rsac1() subroutine 
     *     Define variables used in the envelope routine 
     */
        
    kname = strdup("envelopec_in.sac");
    rsac1(kname, yarray, &nlen, &beg, &delta, &max, &nerr, SAC_STRING_LENGTH);

    if (nerr != 0) {
      fprintf(stderr, "Error reading in file: %s\n", kname);
      exit(-1);
    }

    /* Allocate space for the envelope of yarray */
    yenv = (float *) malloc(sizeof(float) * nlen);
    if(yenv == NULL) {
      fprintf(stderr, "Error allocating memory for envelope\n");
      exit(-1);
    }

    /*     Call envelope ( Envelope Function )
     *        - nlen   - Number of points in yarray
     *        - yarray - Input array to take the envelope of
     *        - yenv   - Envelope of yarray
     */
    envelope(nlen, yarray, yenv);

    /*     Do more processing ....  */
    xarray[0] = 0;
    kname = strdup("envelopec_out.sac");    
    wsac0(kname, xarray, yenv, &nerr, SAC_STRING_LENGTH);
    if (nerr != 0) {
      fprintf(stderr, "Error writing out file: %s\n", kname);
      exit(-1);
    }
    
    free(yenv);

    return 0;
}
