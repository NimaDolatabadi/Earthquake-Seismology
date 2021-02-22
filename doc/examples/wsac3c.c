
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sacio.h>

/* Define the Maximum size of the data arrays  */
#define MAX 300

int
main(int argc, char *argv[])
{
  /* Define the varaibles used in the call to wsac2() */
  float xdata[MAX], ydata[MAX] ;
  int max, nerr;
  char kname[ 11 ];
  int j;
  int leven;
  float cona, conb;

  max = MAX;

  /* Set the name the file to be written and initial x value */
  strcpy ( kname , "expdata" ) ;
  xdata[0] = 0.1;
  leven    = 0;
  cona     = 12.3;
  conb     = -45.6;

  /*  Create the Amplitude and Time, an Exponential 
   *  Best viewed with axis as loglin
   */
  ydata[0] = exp(-xdata[0]);
  for(j = 1; j < max; j++) {
    xdata[j] = xdata[j-1] + xdata[j-1] * 1/(4 * M_PI);
    ydata[j] = exp(-xdata[j]);
  }

  /* Create a New Header to store more information
     Newly created header value are set to a default state      
  */
  newhdr();
  
  /* Store values in the newly created header
     You must define the following header variables
     - delta  Time Sampling
              Only if the file is evenly spaced
     - b      Beginning Time
     - e      Ending Time
     - npts   Number of Points in the File
     - iftype File Type
          - itime Time Series File
	  - irlim Spectral File Real/Imaginary 
	  - iamph Spectral File Amplitue/Phase
	  - ixy   X-Y File
	  - iunkn Unknown
     
     All other variables are up to the user
  */
  setnhv ( "npts",   &max,            &nerr, strlen("npts"));
  setlhv ( "leven",  &leven,          &nerr, strlen("leven"));
  setfhv ( "b",      &(xdata[0]),     &nerr, strlen("b"));
  setfhv ( "e",      &(xdata[max-1]), &nerr, strlen("e"));
  setihv ( "iftype", "ixy",           &nerr, strlen("iftype"), strlen("ixy"));
  setfhv ( "user0",  &cona,           &nerr, strlen("user0"));
  setfhv ( "user1",  &conb,           &nerr, strlen("user1"));
  setkhv ( "kuser0", "gendat",        &nerr, strlen("kuser0"), strlen("gendat"));
  
  /* Write the SAC file kname
     - kname holds the name of the file to be written
     - xdata Input Time Data      
     - yfunc Input Amplitude Data
     - nerr Error return Flag
     - strlen(kname) Length of character string kname
  */
  wsac0(kname, xdata, ydata, &nerr, strlen( kname )) ;

  /* Check the Error status
     - 0 on Success
     - Non-Zero on Error
  */
  if(nerr != 0) {
    fprintf(stderr, "Error writing SAC File: %s\n", kname);
    exit(-1);
  }

  exit(0);

}
