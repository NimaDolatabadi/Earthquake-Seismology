/** 
 * Copyright (c) 2008, Brian Savage < savage _AT_ uri.edu >
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright 
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright 
 *   notice, this list of conditions and the following disclaimer in 
 *   the documentation and/or other materials provided with the distribution.
 * * Neither the name of the author nor the names of its contributors may be 
 *   used to endorse or promote products derived from this software without 
 *   specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 *
 * The views and conclusions contained in the software and documentation are 
 * those of the authors and should not be interpreted as representing official
 * policies, either expressed or implied.
 *
 * BBF Swap 
 * BlackBoard File Swap
 *   The program was written to efficiently swap Blackboard varaible files
 *   produced using SAC (Seismic Analysis Code)
 *
 * To compile
 *   % cc -o bbfswap bbfswap.c
 *
 * To run
 *   % ./bbfswap input_bbf_file
 *   %    The output will be input_bbf_file.swp
 *   % ./bbfswap -o input_bbf_file
 *   %    The bbf_file will be overwritten
 *   % ./bbfswap -v input_bbf_file
 *   %    Detail about the bbf_file will displayed 
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "../inc/vars.h"
#include "../inc/bbf.h"


void bbf_message(int type, char *fmt, ...);

static int verbose = 0;

#define BBFSWAP              "bbfswap"

void
usage() {
  fprintf(stderr, 
	  "Usage: " BBFSWAP " [-vo] [-x entension] inputfiles\n"
	  "    Convert Blackboard variable files (BBF) between byte orders\n"
	  "    -v Verbose Mode\n"
	  "    -x extension Added to the end of the files\n"
	  "       Default .swp\n"
	  "    -o Overwrite the original file\n"
	  );
}

int
main(int argc, char *argv[]) {

  int i;
  int doswap;
  char *input;
  char *output;
  char *ext;
  int options;
  bbf *b;

  ext = NULL;
  i = 1;
  options = TRUE;
  if(argc <= 1) 
    usage();

  while(i < argc) {
    if(argv[i][0] == '-' && options) {
      switch(argv[i][1]) {
      case 'v': 
        verbose++;
        bbf_verbose(verbose);
        bbf_message(MESSAGE_DEBUG, "Verbose Mode On\n");
        break;
      case 'x':
        i++;
        ext = strdup(argv[i]);
        break;
      case 'o':
        ext = strdup("");
        break;
      }
    } else {
      /* Turn off Options, Create Extension, run through files */
      if(options == TRUE) {
        if(ext == NULL) {
          ext = strdup(".swp");
        }
        options = FALSE;
      }

      /* Create Output File */
      input = argv[i];
      output = (char *)malloc(sizeof(char) * (strlen(input) + strlen(ext) + 1));
      sprintf(output, "%s%s", input,ext);

      b = bbf_read(input, &doswap);

      bbf_write(b, output, !doswap);

      free(output);
      bbf_free(b);
      b = NULL;
    }
    i++;
  }
  exit(0);

}

