
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sacio.h>

/* Define the Maximum size of data array */
#define MAX 200

int
main(int argc, char *argv[])
{
  
  /* Define variables to be passed to wsac1() */
  int max, j, nerr;
  float yfunc[ MAX ], x, beg, del;
  char kname[ 10 ];
  
  max = MAX;

  /* Define the file to be written, the beginning time 
     time sampling, and the initial value
  */
  strcpy ( kname , "expdata" ) ;
  beg = 0.00;
  del = 0.02;
  x = beg;

  /* Create the Amplitude data, an Exponential */
  for ( j = 0; j < MAX ; j++ ) {
    yfunc[ j ] = exp ( -x ) ;
    x = x + del;
  }

  /* Write the SAC file kname
     - kname holds the name of the file to be written
     - yfunc Input Amplitude data
     - max number of points to be writtne
     - beg Beginning Time of the data
     - del Time Sampling of the series
     - nerr Error return Flag
     - strlen(kname) Length of the character array kname
  */
  wsac1 (kname, yfunc, &max, &beg, &del, &nerr, strlen( kname )) ;

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

