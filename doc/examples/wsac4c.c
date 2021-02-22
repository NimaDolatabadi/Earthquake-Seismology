

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sacio.h>

#define MAX 36

int
main(int argc, char *argv[]) {
      
  /*  Maximum Size of Array, in 2-D */
  int max;
  
  /* Size of arrays to store the data */
  float dummy[MAX], zdata[MAX];
      
  /*  Define variables to be passed into wsac0 */
  char kname[10];
  int i, j, k;
  int nerr;
  int nx, ny;
  int leven;
  float minimum, maximum;

  /*  Define the file to be written and the min and max of the 2-D Array */
  strcpy(kname, "xyzdata");
  max     = MAX;
  minimum = 1.0;
  maximum = 6.0;
  nx      = 6;
  ny      = 6;
  leven   = 1;

  /* Create the 2D Data */
  k = 0;
  for(i = minimum-1; i < maximum; i++) {
    for(j = minimum-1; j < maximum; j++) {
      zdata[k] = sqrt(i * i + j * j);
      k = k + 1;
    }
  }

  /* Create a new Header and fill it
     We are defining the data type, iftype to be 'ixyz', a 2-D Array
  */
  newhdr();
  setnhv("npts",     &max,     &nerr, strlen("npts"));
  setlhv("leven",    &leven,   &nerr, strlen("leven"));
  setihv("iftype",   "ixyz",   &nerr, strlen("iftype"), strlen("ixyz"));
  setnhv("nxsize",   &nx,      &nerr, strlen("nxsize"));
  setnhv("nysize",   &ny,      &nerr, strlen("nysize"));
  setfhv("xminimum", &minimum, &nerr, strlen("xminimum"));
  setfhv("xmaximum", &maximum, &nerr, strlen("xmaximum"));
  setfhv("yminimum", &minimum, &nerr, strlen("yminimum"));
  setfhv("ymaximum", &maximum, &nerr, strlen("ymaximum"));
  /* Write the SAC file kname
     - kname holds the name of the file to be written
     - dummy Input Amplitude Data
     - zdata Input Time Data      
     - nerr Error return Flag
  */

  wsac0(kname, dummy, zdata, &nerr,strlen(kname));

  /* Check the Error status
     - 0 on Success
     - Non-Zero on Error
  */
  if(nerr != 0) {
    fprintf(stderr, "Error writing SAC File: %s %d\n", kname,nerr);
    exit(-1);
  }

  exit(0);
    
}
