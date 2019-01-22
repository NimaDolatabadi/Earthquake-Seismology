/* updates */
/* 25 3 97 by jh : comment out closedir   */
/* nov 98 by bmt   -----------------    version 7.0 check ----------------*/
/* 10 nov 98 by bmt : specific search */
/* march 29  00 jh  : change file length to 29   */
/* nov 26 04 lo : add function to read seed filenames */
/* 2014.02.21 pv : changed qsort due to:
          warning: passing argument 4 of qsort from incompatible pointer type */
 
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>


    void respfil_(sdir,dir_length,sfiles,max_sfiles,nfiles)
/* -----------------------------------------------------------ARNE-SJURSEN-- */
/* get list of response-files in directory sdir */

  char *sdir;                  /* directory name */
  int *dir_length;             /*length of sdir */
  char  *sfiles;               /* files to get   */
  int *max_sfiles;             /* maximum number of files */
  int *nfiles;                 /* number of files found */


{


  int i;
  int length;                   /* length of string to use */
  DIR   *dirp;                  /* directory structure */
  char *base;                    /* data base used */
  char cad[200];
  struct dirent *dp;            /* directory entry */

  sdir[*dir_length]='\0';       /* assume no null char from fortran */
/*  length=32; */
  length=29;
  *nfiles=0;
  strcpy(cad,sdir + *dir_length-9);
  sdir[*dir_length-9]='\0';
  dirp = opendir(sdir);
  if (!dirp) {
/*    closedir(dirp);*/ 
    nfiles=0;
    return;                      /* return since no files */
  } 
/*   get files  */
  for (dp=readdir(dirp);dp!=NULL;dp=readdir(dirp)) {
    if ((*nfiles)<(*max_sfiles)){ 
      if(!strncmp(cad,dp->d_name,9)) {  
        strncpy((sfiles+((*nfiles)++)*(length)),dp->d_name,length);
      }
}
    else {
      printf("--%s-- ERROR: Out of variable space, \'%s\' will not be listed\n"
  	,"",dp->d_name);
    }
  }
/*    printf(" %d \n",*nfiles); */
 

  /* pv: qsort (sfiles,*nfiles,length,strcmp); */
  qsort (sfiles,*nfiles,length,(int (*)(const void *, const void *))strcmp);


  closedir(dirp); 

}




    void respfilseed_(sdir,dir_length,sfiles,max_sfiles,nfiles)
/* -----------------------------------------------------------ARNE-SJURSEN-- */
/* get list of response-files in directory sdir */

  char *sdir;                  /* directory name */
  int *dir_length;             /*length of sdir */
  char  *sfiles;               /* files to get   */
  int *max_sfiles;             /* maximum number of files */
  int *nfiles;                 /* number of files found */


{


  int i;
  int length;                   /* length of string to use */
  DIR   *dirp;                  /* directory structure */
  char *base;                    /* data base used */
  char cad[200];
  struct dirent *dp;            /* directory entry */

  sdir[*dir_length]='\0';       /* assume no null char from fortran */
/*  length=32; */
  length=29;
  *nfiles=0;
  strcpy(cad,sdir + *dir_length-5);
  sdir[*dir_length-5]='\0';
  dirp = opendir(sdir);
  if (!dirp) {
/*    closedir(dirp);*/ 
    nfiles=0;
    return;                      /* return since no files */
  } 
/*   get files  */
  for (dp=readdir(dirp);dp!=NULL;dp=readdir(dirp)) {
    if ((*nfiles)<(*max_sfiles)){ 
      if(!strncmp(cad,dp->d_name,5)) {  
        strncpy((sfiles+((*nfiles)++)*(length)),dp->d_name,length);
      }
}
    else {
      printf("--%s-- ERROR: Out of variable space, \'%s\' will not be listed\n"
  	,"",dp->d_name);
    }
  }
/*    printf(" %d \n",*nfiles); */
 

  /* pv: qsort (sfiles,*nfiles,length,strcmp); */
  qsort (sfiles,*nfiles,length,(int (*)(const void *, const void *))strcmp);


  closedir(dirp); 

}



