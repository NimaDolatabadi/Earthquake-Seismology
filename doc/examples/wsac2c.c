
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
  
  max = MAX;

  /* Set the name the file to be written and initial x value */
  strcpy ( kname , "expdata" ) ;
  xdata[0] = 0.1;

  /*  Create the Amplitude and Time, an Exponential 
   *  Best viewed with axis as loglin
   */
  ydata[0] = exp(-xdata[0]);
  for(j = 1; j < max; j++) {
    xdata[j] = xdata[j-1] + xdata[j-1] * 1/(4 * M_PI);
    ydata[j] = exp(-xdata[j]);
  }
  
  /* Write the SAC file kname
     - kname holds the name of the file to be written
     - yfunc Input Amplitude Data
     - max number of points to be written
     - xdata Input Time Data      
     - nerr Error return Flag
     - strlen(kname) Length of character string kname
  */
  wsac2(kname, ydata, &max, xdata, &nerr, strlen( kname )) ;


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
