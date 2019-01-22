/* first version by----------------------- ARNE-SJURSEN ---- */
/* get list of s-files in directory sdir */
/* updates */
/* feb 96 by jh: close directory */
/* dec 96      : sort after year and month also */
/* mar 25 97   : comment out closedir if no files */
/* apr 8       : fix to work on solaris with local directory */
/* sep 98 by jh: ----------- version 7.0 check --------------*/
/*               5 char base name, longer s-file name        */
/* jun 99        do not use s-file with a % tagged on end    */
/* aug 3       : fix sorting, local data base                */
/* sep 9    lo : only use s-files if 19 characters           */
/* apr 27 12 on: add quiet parameter to function             */
/* 2014.02.21 pv : changed qsort due to:
          warning: passing argument 4 of qsort from incompatible pointer type 
          fixed warning: comparison between pointer and integer */

#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>


void sfil_(sdir,dir_length,sfiles,max_sfiles,nfiles,quiet)

char *sdir;                  /* directory name */
int *dir_length;             /* length of sdir */
char  *sfiles;               /* files to get   */
int *max_sfiles;             /* maximum number of s-files */
int *nfiles;                 /* number of files found */
int *quiet;	             /* if 'quiet' is different fom zero, then no   */
                             /* messages will be written to standard output */
{
   int i;
   int length;                   /* length of string to use */
   char text13[14];
   char text6[7];
   DIR *dirp;                    /* directory structure */
   char *base;                   /* data base used */
   struct dirent *dp;            /* directory entry */
  
   if (*dir_length == 0) {        /* if local dir, put a . */
      *dir_length = 1;
      sdir[0] = '.';
   }

   sdir[*dir_length] = '\0';       /* assume no null char from fortran */
   length = 19;
   *nfiles = 0;
   base = getenv("TRANSFER_BASE");
   if (*quiet == 0) printf(" Reading events from base %c%c%c%c%c ",*base,*(base+1),*(base+2),*(base+3),*(base+4)); 
   dirp = opendir(sdir);
   if (!dirp) {
      nfiles = 0;
      if (*dir_length == 1) {       /* if local dir remove . */
         *dir_length = 0;
         sdir[0] = ' ';
      }
      return;                      /* return since no files */
   } 
   
   /*   get files  */
   for (dp=readdir(dirp);dp!=NULL;dp=readdir(dirp)) {
      if ((*nfiles) < (*max_sfiles)) { 
         if(*((dp->d_name)+2)=='-' && (*((dp->d_name)+19))=='\0' && (*((dp->d_name)+7))=='-' && (*((dp->d_name)+12))=='S')   
            strncpy((sfiles+((*nfiles)++)*(length)),dp->d_name,length);
      } else {
         if (*quiet == 0) printf("--%s-- ERROR: Out of variable space, \'%s\' will not be listed\n", "",dp->d_name);
      }
   }

   if (*quiet == 0) printf(" %d \n",*nfiles);
 
   /* if a local data base, put year and month in front for sorting */ 
   if (strncmp(",,",base,2) == 0) {
      for (i=0;i<*nfiles;i++) {
         strncpy(text13,sfiles+i*length,13);
         strncpy(text6,sfiles+i*length+13,6);
         strncpy(sfiles+i*length,text6,6);
         strncpy(sfiles+i*length+6,text13,13);
      }
   }

   /* pv: qsort (sfiles,*nfiles,length,strcmp); */
   qsort (sfiles,*nfiles,length,(int (*)(const void *, const void *))strcmp);


   /* now put back if local data base */

   if (strncmp(",,",base,2) == 0) {
      for (i=0;i<*nfiles;i++) {
         strncpy(text13,sfiles+i*length+6,13);
         strncpy(text6,sfiles+i*length,6);
         strncpy(sfiles+i*length+13,text6,6);
         strncpy(sfiles+i*length,text13,13);
      }
   }

   closedir(dirp); 

   if (*dir_length == 1) {
      *dir_length = 0;
      sdir[0] = ' ';
   }
}
