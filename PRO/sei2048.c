/*******************************************

  program: sei2048.c
  author:  Peter Voss
  purpose: Used to solve problem on Sun where 
           the last record of a wav file cannot 
           be read if it contain a EOF. 
           The program is used to add 2048 spaces
           at the end of a temporary wav file.

           Problem is only seen with the 
           Sun Studio 11  Fortran 95 compiler, 
           version 8.2

  date:    17 June 2011

*******************************************/

#include <stdio.h>

int main() {
int j=0;
while (j < 2048) {
        putchar(32);
        j++; };
}



