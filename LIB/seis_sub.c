/*        update                                                       */
/* sep 98 jh    : ---------- version 7.0 check ---------------------*/
/*                no changes                                        */
/* nov 5 98 jh  : change call so no '_' in name, does not work on linux */
/* nov 12 bmt   : add clock_ function     */                         
/* feb 19 lo    : include stdlib.h and define strcmp */
/* mat 16 jh    : undefine strcmp, does not work on sun os, rm getfil */ 
/*                not usded in any programs                           */
/* mar 18 lo    : putenvsun changed to run with gnu compilers       */
/* aug 3  jh    : put in qsort in get files again, where did it go  */
/* sep 28 bmt   : add _open,_read,_eof function*/
/* sep 29 bmt : time adjusted */
/* nov 8  bmt : change read and open name functions*/
/* dec 9  bmt : add functions for bgisei*/
/* jun 20 jh  : revised bgisei routines */
/* jan 27 2011 jh : remove curses.h, remove bgisei stuff */
/* 2014.02.21 pv : changed qsort due to:
          warning: passing argument 4 of qsort from incompatible pointer type */


/*#include <curses.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <libgen.h>

#define maxchar 100      /* Max number of characters in a string */

typedef char string[maxchar+1];   /* String type */
        char envtext[255];




/*  Put enviroment variable on sun                */
/* changed to run with gnu compilers lo mar 99    */
/* mar 30 99 by jh   ad sort_fi                   */

    void putenvsun_(text)

     char* text;
{
/*      strcpy(envtext,text);
      putenv(envtext); */
      putenv(text);
}
/******************************************************************/

/* routine to make system call in c from fortran, is faster */
/* than doing it from fortran ...                           */
/* modified to handle any length of command string bjb 2001/02/14 */
void systemc_(text,length)
int *length; 
char *text;
{
  char *c_buffer;
  c_buffer = (char *)malloc (*length +1);
  if (! c_buffer) return;
  strncpy (c_buffer, text, *length);
  *(c_buffer + *length) = '\0';
  system(c_buffer);
}

/* - - - - - - - - - - old routine - - - - - - - - - - */
/* routine to make system call in c from fortran, is faster */
/* than doing it from fortran ...                           */
/* If $SEISARCH != "SOLARIS", then add a null to string     */
/*
void systemc_(text,length)

int *length; 
char *text;
{
char arch[20];
char command_text[241];

get_arch_(arch);                Get environment variable 
if( strcmp(arch,"SOLARIS") != 0 ) text[*length]='\0'; 

strncpy(command_text,text,*length);
command_text[*length] = '\0';
printf("\n %s \n",command_text);
system(command_text);
}
*/

/*void systemcc_(text,length)
int *length; 
char *text;
{
char arch[20];
char command_text[240];
if( strcmp(arch,"SOLARIS") != 0 ) text[*length]='\0'; 
strncpy(command_text,text,*length);
command_text[*length] = '\0';
system(command_text);
}  */

void readn_(char_ptr,n)
char *char_ptr;
long *n;
{

  char input[100];
  int i;

  for (i=0;i<*n;i++) char_ptr[i]=getchar();
  fflush(stdin);
}



/* updates */
/* 25 3 97 jh : commanr out closedir */
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <time.h>
#include <unistd.h> 


    void getfiles_(sdir,dir_length,sfiles,max_sfiles,nfiles)
/* -----------------------------------------------------------ARNE-SJURSEN-- */
/* get list of all files in directory sdir */
/* sep 98 by jh ----- version 7.0 check -------------------------------------*/
/*          no change                                                        */
/* put in qsort again *  */

  char *sdir;                  /* directory name */
  int *dir_length;             /*length of sdir */
  char  *sfiles;               /* files to get   */
  int *max_sfiles;             /* maximum number of s-files */
  int *nfiles;                 /* number of files found */


{

/*  int (*strcmp)(); */
  int i;
  int length;                   /* length of string to use */
  DIR   *dirp;                  /* directory structure */
  char *base;                    /* data base used */
  struct dirent *dp;            /* directory entry */

  sdir[*dir_length]='\0';       /* assume no null char from fortran */
  length=80;
  *nfiles=0;
  dirp = opendir(sdir);
  if (!dirp) {
/*    closedir(dirp);  */
    nfiles=0;
    return;                      /* return since no files */
  } 
/*   get files  */
  for (dp=readdir(dirp);dp!=NULL;dp=readdir(dirp)) {
    if ((*nfiles)<(*max_sfiles)){ 
        strncpy((sfiles+((*nfiles)++)*(length)),dp->d_name,length);
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

clock_t clock_() {
 return clock();
}

/* updates */
/* sep 98 by jh -------------- version 7.0 check ----------------------*/
/*              no change                                              */
/* mar 19 99 lo change name to sortfi                                  */

#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <fcntl.h>      /* Needed only for _O_RDWR definition */
#include <unistd.h>
#define _O_BINARY 0

    void sortfi_(files,nfiles)
/* sort character strings 80 chars long */

  char  *files;               /* file names to sort   */
  int *nfiles;                 /* number of names */


{



  /* pv: qsort (files,*nfiles,80,strcmp); */
  qsort (files,*nfiles,80,(int (*)(const void *, const void *))strcmp);



}

/* only used in linux */

int  new_open(name,attr)
char *name;    /*file name*/
int attr;     /* attribute of the file*/
{
 return open(name,attr);
}

int new_read(f, p, c) 
int f;   /* file descriptor */
void *p;  /*buffer for receiver*/
int c;   /* # of byte to read */
{
  return read(f,p,c);
}

