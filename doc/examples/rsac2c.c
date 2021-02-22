#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <sacio.h>

/* Define the maximum length of the data and time array */
#define MAX 3000

int
main(int argc, char *argv[])
{
  /* Define variables to be used in the call to rsac2() */
  float xarray[MAX], yarray[MAX];
  int nlen, nerr, max;
  char kname[ 11 ] ;
  
  max = MAX;

  /* Copy the name of the file to be read into kname */
  strcpy(kname, "FILE2") ;

  /* Call rsac1 to read filename kname
     - Amplitude Data is loaded into yarray
     - Length of data is stored in nlen
     - Time Data is loaded into xarray
     - max is the maximum number of points to be read in 
     - nerr is the error return flag
     - strlen( kname ) is the length of character array kname
     All variables are passed as references either
         arrays like kname and yarray or
	 using &varible to pass reference to variable
  */
  rsac2(kname, yarray, &nlen, xarray, &max, &nerr, strlen( kname )) ;

  /* Check the error status, nerr
     - 0 on Success
     - Non-Zero on Failure  
  */
  if ( nerr > 0 ) {
    fprintf(stderr, "Error reading in SAC file: %s\n", kname);
    exit(nerr) ;
  }

  /* Do some processing ... */

  exit(0);
}
