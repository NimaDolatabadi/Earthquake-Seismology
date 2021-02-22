
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
    int i, j;
    int nlen, nlen1, nlen2, nerr, max;

    float beg, delta, end;
    char *kname;

    float yarray1[MAX], yarray2[MAX], ytmp[MAX], xarray[1];
    float *out;

    int nwin, wlen, nfft, leven;

    char error[ERROR_MAX];

    max = MAX;

    for(i = 0; i < MAX; i++) {
      yarray1[i] = 0.0;
      yarray2[i] = 0.0;
      ytmp[i] = 0.0;
    }
    /* Read in the first file  */        
    kname = strdup("convolvec_in1.sac");
    rsac1(kname, ytmp, &nlen1, &beg, &delta, &max, &nerr, SAC_STRING_LENGTH);

    if (nerr != 0) {
      fprintf(stderr, "Error reading in file(%d): %s\n", nerr, kname);
      exit(-1);
    }


    /* Read in the second file  */
    kname = strdup("convolvec_in2.sac");
    rsac1(kname, yarray2, &nlen2, &beg, &delta, &max, &nerr, SAC_STRING_LENGTH);

    if (nerr != 0) {
      fprintf(stderr, "Error reading in file: %s\n", kname);
      exit(-1);
    }

    /* Reverse the First Signal */
    j = 0;
    for(i = nlen1 - 1; i >= 0; i--) {
      yarray1[j] = ytmp[i];
      j++;
    }
    
    nlen = nlen1;
    if(nlen2 > nlen) {
      nlen = nlen2;
    }
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
    
    /*     Call crscor ( Cross Correlation, no, wait, uh Convolution )
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
           out, &nfft, error, ERROR_MAX);

    /* Zero out the tmp signal */
    for(i = 0; i < MAX; i++) {
      ytmp[i] = 0.0;
    }
    
    /* Reconstruct the signal from the "cross correlation" back to front
     *  
     *  ytmp[0         : nlen1 - 2         ] <- out[nfft-nlen1+1 : nfft  - 1 ] 
     *  ytmp[nlen1 - 1 : nlen1 + nlen2  -2 ] <- out[0            : nlen2 - 1 ]
     *
     *  nfft-1 is the last point of the output sequence
     */
    for(i = 0; i <= nlen1 - 2; i++) {  
      ytmp[i] = out[nfft - nlen1 + i + 1];
    }
    for(i = 0; i <= nlen2 - 1; i++) {
      ytmp[nlen1 + i - 1] = out[i];
    }

    nfft = nlen1 + nlen2 - 1;
    xarray[0] = 0;
    leven = TRUE;
    beg = 0;
    end = beg + delta * (nfft - 1);
    newhdr();
    j = 1;
    setnhv ( "npts",   &nfft,    &nerr, SAC_STRING_LENGTH);
    setfhv ( "delta",  &delta,   &nerr, SAC_STRING_LENGTH);
    setlhv ( "leven",  &leven,   &nerr, SAC_STRING_LENGTH);
    setfhv ( "b",      &beg,     &nerr, SAC_STRING_LENGTH);
    setfhv ( "e",      &end,     &nerr, SAC_STRING_LENGTH);
    setihv ( "iftype", "itime",  &nerr, SAC_STRING_LENGTH, SAC_STRING_LENGTH);
    setkhv ( "kcmpnm", "Q",      &nerr, SAC_STRING_LENGTH, SAC_STRING_LENGTH);
    setkhv ( "kstnm",  "sta",    &nerr, SAC_STRING_LENGTH, SAC_STRING_LENGTH);
    setnhv ( "nwfid",  &j,       &nerr, SAC_STRING_LENGTH);
    setkhv ( "kevnm",  "FUNCGEN: TRIANGLE", &nerr, SAC_STRING_LENGTH, SAC_STRING_LENGTH);

    /*   Write out the correlation function   */
    kname = strdup("convolvec_out1.sac");    
    wsac0(kname, xarray, ytmp, &nerr, SAC_STRING_LENGTH);
    if (nerr != 0) {
      fprintf(stderr, "Error writing out file: %s\n", kname);
      exit(-1);
    }
    
    free(out);

    return 0;
}
