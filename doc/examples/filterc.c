
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "sac.h"
#include "sacio.h"

#define MAX 1000

int 
main(int argc, char *argv[]) {

    /* Local variables */
    double low, high, attenuation, transition_bandwidth;;
    int nlen, nerr, max;

    float beg, delta;
    double delta_d;
    char *kname;
    int order;

    int passes;
    float yarray[1000];
    float xarray[1];

    max = MAX;

    /*     Define the Maximum size of the data Array 
     * Filter Prototypes 
     * Filter Types 
     *     Define the Data Array of size MAX 
     *     Declare Variables used in the rsac1() subroutine 
     *     Define variables used in the filtering routine 
     */
        
    kname = strdup("filterc_in.sac");
    rsac1(kname, yarray, &nlen, &beg, &delta, &max, &nerr, SAC_STRING_LENGTH);
    delta_d = delta;
    if (nerr != 0) {
      fprintf(stderr, "Error reading in file: %s\n", kname);
      exit(-1);
    }

    low    = 0.10;
    high   = 1.00;
    passes = 2;
    order  = 4;
    transition_bandwidth = 0.0;
    attenuation = 0.0;

    /*     Call xapiir ( Apply a IIR Filter ) 
     *        - yarray - Original Data 
     *        - nlen   - Number of points in yarray 
     *        - proto  - Prototype of Filter 
     *                 - SAC_FILTER_BUTTERWORK        - Butterworth 
     *                 - SAC_FILTER_BESSEL            - Bessel 
     *                 - SAC_FILTER_CHEBYSHEV_TYPE_I  - Chebyshev Type I 
     *                 - SAC_FILTER_CHEBYSHEV_TYPE_II - Chebyshev Type II 
     *        - transition_bandwidth (Only for Chebyshev Filter) 
     *                 - Bandwidth as a fraction of the lowpass prototype 
     *                   cutoff frequency 
     *        - attenuation (Only for Chebyshev Filter) 
     *                 - Attenuation factor, equals amplitude reached at 
     *                   stopband egde 
     *        - order  - Number of poles or order of the analog prototype 
     *                   4 - 5 should be ample 
     *                   Cannot exceed 10 
     *        - type   - Type of Filter 
     *                 - SAC_FILTER_BANDPASS 
     *                 - SAC_FILTER_BANDREJECT 
     *                 - SAC_FILTER_LOWPASS 
     *                 - SAC_FILTER_HIGHPASS 
     *        - low    - Low Frequency Cutoff [ Hertz ] 
     *                   Ignored on SAC_FILTER_LOWPASS 
     *        - high   - High Frequency Cutoff [ Hertz ] 
     *                   Ignored on SAC_FILTER_HIGHPASS 
     *        - delta  - Sampling Interval [ seconds ] 
     *        - passes - Number of passes 
     *                 - 1 Forward filter only 
     *                 - 2 Forward and reverse (i.e. zero-phase) filtering 
     */
    xapiir(yarray, nlen, SAC_BUTTERWORTH, 
	   transition_bandwidth, attenuation, 
	   order, 
	   SAC_BANDPASS, 
	   low, high, 
	   delta_d, passes);

    /*     Do more processing ....  */
    xarray[0] = 0;
    kname = strdup("filterc_out.sac");    
    wsac0(kname, xarray, yarray, &nerr, SAC_STRING_LENGTH);
    if (nerr != 0) {
      fprintf(stderr, "Error writing out file: %s\n", kname);
      exit(-1);
    }
    

    return 0;
}
