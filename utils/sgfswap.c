	/* ===========================================================================
	 * PURPOSE:  To swap the endian of a SAC (binary) .sgf file
	 * ==========================================================================	
	Input is a SAC Graphics Format (SGF) file. Output is a SGF file
	  with the opposite endian.
	If input file is named XXXX.sgf, output file is XXXX_swap.sgf

	SGF files are direct access (512 byte blocks) binary files.  Data 
	  are stored in buffers of 2*nbuf I*2 words. Preceding each buffer
	  is an I*4 word = nbuf.  The first buffer has less than
	  2**16 words, so if nbuf is greater then 2**16, the machine and 
	  the format of the file have the opposite endian.  In that case, 
	  one needs to do a swap4 on that word to find nbuf.
	  Once the number, nbuf, is known one swaps that word and the next
	  2*nbuf I*2 words.  This is repeated for the rest of the file
	  (although one does not need to test after the first).
=========================================================================
	 * MODIFICATION HISTORY: jas/vt May 2009
	 *
========================================================================= 
	 */
#include <fcntl.h>
#include <sys/types.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

#define TRUE 1
#define FALSE 0

static int same = TRUE;
int32_t int_swap32(char cbuf[]);
int32_t int_join32(char cbuf[]);
int16_t int_swap16(char cbuf[]);
int16_t int_join16(char cbuf[]);

int main(int argc, char *argv[]) {
    char outputfile[80], cbuf2[2], cbuf4[4];
    int16_t nowi2;
    int j, buflen, done = 0, ninputfile, nowi4;
    FILE *file_in, *file_out;
    
    /* If no arguments, print usage */
    if (argc < 2) {
        fprintf(stderr, "usage: %s XXX.sgf, where XXX.sgf", argv[0]);
        fprintf(stderr, " is a SAC Graphics Format file.\n");
        fprintf(stderr, "Program swiches the endian.\n");
        fprintf(stderr, "Output is XXX_swap.sgf\n");
        exit(-1);
    }

/* does argv[1] end in ".sgf" ? */
    if (strcmp(&argv[1][strlen(argv[1]) - 4], ".sgf") != 0) {
        fprintf(stderr, "%s: %s not a SAC GRAPHICS FILE.\n", *argv, argv[1]);
        exit(1);
    }
    /* open the input .sgf file */
    ninputfile = strlen(argv[1]);
    if ((file_in = fopen(argv[1], "rb")) == NULL) {
        fprintf(stderr, "%s: Can't open %s for reading.\n",
	     *argv, argv[1]);
        exit(-3);
    }
/*    fprintf(stderr,"%s is the input file\n",argv[1]); */
    strncpy(outputfile,argv[1],ninputfile - 4);
    outputfile[ninputfile-4] = '\0';
    strcat(outputfile,"_swap.sgf");
/* open the output .sgf file with opposite endian */
    if ((file_out = fopen(outputfile,"wb")) == NULL) {
        fprintf(stderr, "cannot create output file to write: '%s'\n",
                outputfile);
        exit(-2);
    }
/*    fprintf(stderr,"%s is the output file\n",outputfile); */
    
/* First buffer.  Check on endian of file relative to computer */
    if (fread(cbuf4,4,1,file_in) != 1) {
        fprintf(stderr, "Failure on read of first buffer\n");
        exit(-3);
    } 
    buflen = int_join32(cbuf4);
    nowi4 = int_swap32(cbuf4);
/*    fprintf(stderr,"%d is the input first nowi4\n",buflen); */
    if (buflen >= 65536) {
      same = FALSE;
      buflen = nowi4;
      }
/*    fprintf(stderr,"%d = swapped nowi4\n",nowi4);
    fprintf(stderr,"%d = number in buffer\n",buflen); */
    fwrite(&nowi4,4,1,file_out);
	
    for (j = 0; j < 2*buflen; j++) {
      if(fread(cbuf2,2,1,file_in) != 1) {
	fprintf(stderr, "%s: Problem with buffer read for j = %d\n", 
	*argv, j);
	exit(-1);
      }
      nowi2 = int_swap16(cbuf2);
      fwrite(&nowi2,2,1,file_out);
      }
    
/* execute this loop once for each additional buffer */
    while (done == 0) {
        if (fread(cbuf4,4,1,file_in) != 1)
            done=1;
        else  {
	nowi4 = int_swap32(cbuf4);
	if (same == TRUE) buflen = int_join32(cbuf4);
	else buflen = nowi4;
/*        fprintf(stderr,"%d = number in buffer\n",buflen); */
        fwrite(&nowi4,4,1,file_out);
	for (j = 0; j < 2*buflen; j++) {
          if(fread(cbuf2,2,1,file_in) != 1) {
	    fprintf(stderr, "%s: Problem with buffer read for j = %d\n", 
	      *argv, j);
	  exit(-1);
          }
          nowi2 = int_swap16(cbuf2);
          fwrite(&nowi2,2,1,file_out);
          }
        }
      }
    return 0;
    }
    

int16_t int_join16(char cbuf[]) {
  union {
    char cval[2];
    int16_t lval;
  } l_union;
  
  l_union.cval[0] = cbuf[0];
  l_union.cval[1] = cbuf[1];
  return(l_union.lval);
}

int16_t int_swap16(char cbuf[]) {
  union {
    char cval[2];
    int16_t lval;
  } l_union;
  
  l_union.cval[1] = cbuf[0];
  l_union.cval[0] = cbuf[1];
  return(l_union.lval);
}
int32_t int_swap32(char cbuf[]) {
  union {
    char cval[4];
    unsigned int lval;
  } l_union;
  
  l_union.cval[3] = cbuf[0];
  l_union.cval[2] = cbuf[1];
  l_union.cval[1] = cbuf[2];
  l_union.cval[0] = cbuf[3];
  return(l_union.lval);
}

int32_t int_join32(char cbuf[]) {
  union {
    char cval[4];
    unsigned int lval;
  } l_union;
  
  l_union.cval[3] = cbuf[3];
  l_union.cval[2] = cbuf[2];
  l_union.cval[1] = cbuf[1];
  l_union.cval[0] = cbuf[0];
  return(l_union.lval);
}




