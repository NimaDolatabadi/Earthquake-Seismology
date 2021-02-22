#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <sacio.h>

/* Define the maximum length of the data array */
#define MAX 1000

int
main(int argc, char **argv)
{
  /* Define variables to be used in the call to rsac1() */
  float yarray[MAX], beg, del;
  int nlen, nerr, max = MAX;
  char kname[ 11 ] ;
  
  /* Copy the name of the file to be read into kname */
  strcpy( kname , "FILE1" ) ;
  

  /* Call rsac1 to read filename kname
     - Data is loaded into yarray
     - Length of data is stored in nlen
     - Begining time and time sampling are in beg and del
     - max is the maximum number of points to be read in 
     - nerr is the error return flag
     - strlen( kname ) is the length of character array kname
     All variables are passed as references either
         arrays like kname and yarray or
	 using &varible to pass reference to variable
  */
  rsac1( kname, yarray, &nlen, &beg, &del, &max, &nerr, strlen( kname ) ) ;

  /* Check the error status, nerr
     - 0 on Success
     - Non-Zero on Failure  
  */
  if ( nerr != 0 ) {
    fprintf(stderr, "Error reading in SAC file: %s\n", kname);
    exit ( nerr ) ;
  }
  
  /* Do some processing ... */
  
  exit(0);
}
