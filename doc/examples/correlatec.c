
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <sac.h>
#include <sacio.h>

#define MAX        4000
#define ERROR_MAX  256

int 
main(int argc, char *argv[]) {

    /* Local variables */
    int i;
    int nlen, nlen1, nlen2, nerr, max;

    float beg1, beg2, delta, end;
    char *kname;

    float yarray1[MAX], yarray2[MAX], ytmp[MAX*4], xarray[1];
    float *out;
    float max_value, max_time;

    int nwin, wlen, nfft, leven, when;

    char error[ERROR_MAX];

    max = MAX;

    /* Read in the first file  */        
    kname = strdup("correlatec_in1.sac");
    rsac1(kname, yarray1, &nlen1, &beg1, &delta, &max, &nerr, SAC_STRING_LENGTH);

    if (nerr != 0) {
      fprintf(stderr, "Error reading in file(%d): %s\n", nerr, kname);
      exit(-1);
    }

    /* Read in the second file  */
    kname = strdup("correlatec_in2.sac");
    rsac1(kname, yarray2, &nlen2, &beg2, &delta, &max, &nerr, SAC_STRING_LENGTH);

    if (nerr != 0) {
      fprintf(stderr, "Error reading in file: %s\n", kname);
      exit(-1);
    }

    nlen = nlen1;
    /** 
     *  If the signals are not the same length, then find the longest
     *  signal, make both signals that length by filling the remainder
     *  with zeros (pad at the end) and then run them through crscor
     *  This should be fixed in upcoming releases and the introduction
     *  of a "correlate" function so you do not need to handle the 
     *  signal length, padding, and window lengths, ...
     *
     */

    /* Allocate space for the correlation of yarray1 and yarray2 */
    max = next2((2 * nlen) - 1) * 2;
    out = (float *) malloc(sizeof(float) * max);
    if(out == NULL) {
      fprintf(stderr, "Error allocating memory for correlation\n");
      exit(-1);
    }

    /* Set up values for the cross correlation */
    nwin = 1;
    wlen = nlen;
    nfft = 0;
    
    /*     Call crscor ( Cross Correlation )
     *        - yarray1 - First  Input array to correlate
     *        - yarray2 - Second Input array to correlate
     *        - nlen    - Number of points in yarray and yarray2
     *        - nwin    - Windows to use in the correlation
     *        - wlen    - Length of the windows
     *        - type    - Type of Window (SAC_RECTANGLE)
     *        - out     - output sequence 
     *        - nfft    - Length of the output sequence
     *        - error   - Error Message
     *        - err_len - Length of Error Message (on input)
     */
    crscor(yarray1, yarray2, nlen, 
           nwin, wlen, SAC_RECTANGLE,
           ytmp, &nfft, error, ERROR_MAX);
    
    for(i = 0; i < max; i++) {
      out[i] = 0;
    }
    
    /*     
     *     out[0 : nlen1 - 2 ] <-- ytmp[ nfft - nlen1 + 1 : nfft -1 ]
     *     out[nlen1 - 1 : nlen1 + nlen2 - 2 ] <-- ytmp[ 0 : nlen2-1 ]
     */
    for(i = 0; i <= nlen1 - 2; i++) {
      out[i] = ytmp[nfft - nlen1 + i + 1];
    }
    for(i = 0; i <= nlen2 - 1; i++) {
      out[nlen1 + i - 1] = ytmp[i];
    }


    nfft = nlen1 + nlen2 - 1;
    xarray[0] = 0;
    leven = TRUE;
    beg1 = -delta * (nlen1 - 1) + (beg2 - beg1);
    end = beg1 + delta * (nfft - 1);

    setnhv ( "npts",   &nfft,   &nerr, SAC_STRING_LENGTH);
    setfhv ( "delta",  &delta,  &nerr, SAC_STRING_LENGTH);
    setlhv ( "leven",  &leven,  &nerr, SAC_STRING_LENGTH);
    setfhv ( "b",      &beg1,   &nerr, SAC_STRING_LENGTH);
    setfhv ( "e",      &end,    &nerr, SAC_STRING_LENGTH);
    setihv ( "iftype", "itime", &nerr, SAC_STRING_LENGTH, SAC_STRING_LENGTH);
    when = SAC_NUMBER_UNDEFINED;
    setnhv ( "nzyear", &when,   &nerr, SAC_STRING_LENGTH);
    setnhv ( "nzhour", &when,   &nerr, SAC_STRING_LENGTH);

    /* Find the maximum value and time of the correlation function */
    max_value = out[0];
    max_time  = 0;
    for(i = 1; i < nfft; i++) {
      if(out[i] > max_value) {
        max_value = out[i];
        /* Negative shifts are at the end of the correlation sequence */
        if(i > nfft/2) { 
          max_time  = (i - nfft) * delta;
        } else {
          max_time  = i * delta;
        }
      }
    }

    /*
      setfhv( "user0", &max_time,  &nerr, SAC_STRING_LENGTH);
      setfhv( "user1", &max_value, &nerr, SAC_STRING_LENGTH);
    */

    /*   Write out the correlation function   */
    kname = strdup("correlatec_out1.sac");    
    wsac0(kname, xarray, out, &nerr, SAC_STRING_LENGTH);
    if (nerr != 0) {
      fprintf(stderr, "Error writing out file: %s\n", kname);
      exit(-1);
    }
    
    free(out);

    return 0;
}
