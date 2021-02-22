
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sacio.h>

/* Define the maximum length of the data array */
#define MAX 1000

int
main(int argc, char *argv[])
{ 
  /* Define variables to be used in the call to rsac1() and getfhv() */
  int max = MAX, nlen, nerr, n1, n2;
  float yarray[ MAX ] , beg , del , delta , B , T1 , T2 ;
  char kname[ 11 ] ;

  /* Copy the name of the file to be read into kname */
  strcpy ( kname , "FILE1" ) ;

  /* Read in the SAC File */
  rsac1( kname, yarray, & nlen, & beg, & del, & max, & nerr, strlen( kname ) ) ;

  /* Check the Error status */
  if ( nerr != 0 ) {
    fprintf(stderr, "Error reading SAC file: %s\n", kname);
    exit(-1);
  }

  /* Get floating point header value: Delta
     "DELTA" - name of the header variable requested
      delta  - value of the header variable delta, returned
      nerr   - Error return flag
      strlen("DELTA") - Length of the character array "DELTA"
  */
  getfhv ( "DELTA" , & delta , & nerr , strlen("DELTA") ) ;
  /* Check the Return Value */
  if ( nerr != 0 ) {
    fprintf(stderr, "Error getting header variable: delta\n");
    exit(-1);
  }

  /* Get floating point header value: B */
  getfhv ( "B" , &B , & nerr , strlen("B") ) ;
  if ( nerr != 0 ) {
    fprintf(stderr, "Error getting header variable: b\n");
    exit(-1);
  }

  /* Get floating point header value: T1 */
  getfhv ( "T1" , & T1 , & nerr , strlen("T1") ) ;
  if ( nerr != 0 ) {
    fprintf(stderr, "Error getting header variable: t1\n");
    exit(-1);
  }

  /* Get floating point header value: T2 */
  getfhv ( "T2" , & T2 , & nerr , strlen("T2") ) ;
  if ( nerr != 0 ) {
    fprintf(stderr, "Error getting header variable: t2\n");
    exit(-1);
  }

  /* Compute the time sample at which t1 and t2 occur  */
  n1 = (int) ( ( ( T1 - B ) / delta ) + 0.5 ) ;
  n2 = (int) ( ( ( T2 - B ) / delta ) + 0.5 ) ;
  
  /* ... */

  exit(0);
  
}
