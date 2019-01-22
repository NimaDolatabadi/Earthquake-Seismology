
/*******************************************

  program: dirf.c
  author:  Lars Ottemoeller
  purpose: write list of files from list
           of arguments to file 'filenr.lis'
  date:    24 March 2000

  changes:

  07/2010 lo added -tail, and -head option
  feb 01 2011 jh: remove include to curses.h
  jan 15 2012 jh: increase to 99999 events
  dec 21 2016 on: fix bug with no argument
           
*******************************************/

#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>

main (int argc,char *argv[]) {
  int counter;                  /* counter */
  int start,stop,n;
  FILE *fil;

  if (argc == 1) {
     printf("No argument given. Valid syntax is: dirf filename/wildcard\n");
     return;
  }

/* open file */
  fil = fopen("filenr.lis","w");
  if (fil==NULL) {
    printf("Fail to open filenr.lis \n");
    return(0);    
  }

/* default take all */
  start = 1;
  stop = argc-1;

/* last 20 */
  if (strcmp(argv[1],"-tail") == 0 ) {
    n = 20;
    start = argc - n;
    if (start < 2 ) {
      start = 2;
    }
    stop  = argc-1;
  }

/* first 20 */
  if (strcmp(argv[1],"-head") == 0 ) {
    n = 20;
    start = 2;
    stop  = n+1;
    if (stop > argc ) {
      stop = argc-1;
    }
  }

/* write all arguments to filenr.lis */
  n=0;
  for (counter=start;counter<=stop;++counter) {
    n++;
/* maximum of 9999 files */
    if (counter > 99999) 
    {
      printf(" Maximum is 99999 files \n");
      fclose(fil);
      return(0);
    }
    if (counter <= 999) {
      printf(" #%3d  %-70s\n",n,argv[counter]);
      fprintf(fil," #%3d  %-70s\n",n,argv[counter]);
    } else {
      printf("%5d  %-70s\n",n,argv[counter]);
      fprintf(fil,"%5d  %-70s\n",n,argv[counter]);
    }
  }
/* print blank line */
  fprintf(fil,"\n");

/* close file */
  fclose(fil);
  return(0);
}


