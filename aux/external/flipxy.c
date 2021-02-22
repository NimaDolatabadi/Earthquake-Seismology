#include <stdlib.h>
#include "extfunc.h"

/* Function to flip the x and y data values. */
/* Parameter call_data is a pointer to a struct 
   called sac_files which is defined in extfunc.h. */

int flipxy( int argc, char **argv, sac_files *call_data, int *update)
{
    int i, j, numfiles, npts, error;
    int leven;
    float *ydata, *xdata, delta, begin, end;
    sac_header *hdr;

    /* Replace files in memory with output of this function. */
    *update = REPLACE;

    /* Get the number of input files. */
    numfiles = call_data->nfiles;

    for( i=0; i<numfiles; i++ ){
	/* Set a pointer to the y data values. */
	ydata = call_data->ext_yvalues[i];

	/* Set a pointer to the header. */
	hdr   = call_data->ext_hdrs[i];

	/* Get the number of points. */
	npts = getnhdr(hdr, "npts", &error);

	/* Check for evenly spaced file. */
	leven = getlhdr(hdr, "leven", &error);

	if( leven ){
	    /* For evenly spaced data generate the x values */
	    /* using begin, end and delta.                  */
	    begin = getfhdr(hdr, "b", &error);
	    delta = getfhdr(hdr, "delta", &error);    

	    /* Have to allocate memory for x values. */
	    if((xdata = malloc(npts*sizeof(float))) == NULL){
		printf("Error allocating memory for x data-flipxy\n");
		return 1;
	    }

	    /* Generate x data. */
	    xdata[0] = begin;
	    for( j=1; j<npts; j++ )
		xdata[j] = xdata[j-1] + delta;

	    /* The output file will be unevenly spaced. */    
	    setlhdr(hdr, "leven", FALSE, &error);
	}
	else{
	    /* If the input file is unevenly spaced */
	    /* just get the pointer to the x value. */
	    xdata = call_data->ext_xvalues[i];
	}

	/* SAC routine to return the min and max of and array */
	getlims(ydata, npts, &begin, &end);

	setfhdr(hdr, "b", begin, &error);
	setfhdr(hdr, "e", end, &error);

	/* Flip the pointers. */
	call_data->ext_yvalues[i] = xdata;
	call_data->ext_xvalues[i] = ydata;
    }

    return 0;
}
