/************************************************************
*	List the SAC header fields
*	Usage
*		saclst header_variable_names ... f sac_files ...
*       Got from Lupei Zhu
*       Modified by Zhigang Peng, 11/29/00 23:41:10
*************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sac.h"

int	sac_head(const char *aa);

int main(int argc, char **argv)
{
  int		i,j,ls[40],nl;
  float		*pt;
  SACHEAD	hd;

  if(argc<2) {
      fprintf(stderr,"Usage: saclst header_lists f file_lists\n");
      return -1;
  }

  nl=0;argv++;argc--;
  while ( *argv[0] != 'f' ) {
     ls[nl] = sac_head(argv[0]);
     if (ls[nl]==-999) {
	fprintf(stderr, "error in header_list  %s\n",argv[0]);
	return -1;
     }
     nl++; argv++; argc--;
  }

  for (i=1;i<argc;i++) {

      printf("%s ",argv[i]);

      if ( read_sachead(argv[i], &hd) != -1) {
	pt = (float *) &hd;
        for (j=0;j<nl;j++) {
	  if ((ls[j]>39 && ls[j]<50) || (ls[j]>0 && ls[j]<3) || ls[j]==56)
	     printf("%10.2e",pt[ls[j]]);
	  else if (ls[j]==60)
	     printf("%16s",hd.kevnm);
	  else if (ls[j]==61)
	     printf("%8s",hd.kstnm);
	  else if (ls[j]==62)
	     printf("%8s",hd.kcmpnm);
	  else if (ls[j]==63)
	     printf("%8s",hd.knetwk);
	  else if (ls[j]<70)
	     printf("%14.4f",pt[ls[j]]);
	  else if (ls[j]<76)
/*  	     printf("%5d %3d %2d %2d %5.2f",hd.nzyear,hd.nzjday,hd.nzhour,hd.nzmin,hd.nzsec+0.001*hd.nzmsec); */
  	     printf("%5d %3d %2d %2d %2d %3d",hd.nzyear,hd.nzjday,hd.nzhour,hd.nzmin,hd.nzsec,hd.nzmsec);
	  else if (ls[j] == 79) 
	     printf("%8d",hd.npts);
          else if (ls[j] == 118 )
	     printf("%8s",hd.ko);
          else if (ls[j] == 120 )
	     printf("%8s",hd.ka);
          else if (ls[j] == 122 )
	     printf("%8s",hd.t0);
          else
             printf("wrong flag"); 
        }
      }

      printf("\n");

  }

  return 0;

}


int	sac_head(const char *aa) {
  if(strcmp(aa,"delta")==0) return(0);
  else if (strcmp(aa,"depmin")==0) return(1);
  else if (strcmp(aa,"depmax")==0) return(2);
  else if (strcmp(aa,"b")==0) return(5);
  else if (strcmp(aa,"e")==0) return(6);
  else if (strcmp(aa,"o")==0) return(7);
  else if (strcmp(aa,"a")==0) return(8);
  else if (strcmp(aa,"t0")==0) return(10);
  else if (strcmp(aa,"t1")==0) return(11);
  else if (strcmp(aa,"t2")==0) return(12);
  else if (strcmp(aa,"t3")==0) return(13);
  else if (strcmp(aa,"t4")==0) return(14);
  else if (strcmp(aa,"t5")==0) return(15);
  else if (strcmp(aa,"t6")==0) return(16);
  else if (strcmp(aa,"t7")==0) return(17);
  else if (strcmp(aa,"t8")==0) return(18);
  else if (strcmp(aa,"t9")==0) return(19);
  else if (strcmp(aa,"stla")==0) return(31);
  else if (strcmp(aa,"stlo")==0) return(32);
  else if (strcmp(aa,"stel")==0) return(33);
  else if (strcmp(aa,"stdp")==0) return(34);
  else if (strcmp(aa,"evla")==0) return(35);
  else if (strcmp(aa,"evlo")==0) return(36);
  else if (strcmp(aa,"evel")==0) return(37);
  else if (strcmp(aa,"evdp")==0) return(38);
  else if (strcmp(aa,"mag")==0) return(39);
  else if (strcmp(aa,"user0")==0) return(40);
  else if (strcmp(aa,"user1")==0) return(41);
  else if (strcmp(aa,"user2")==0) return(42);
  else if (strcmp(aa,"user3")==0) return(43);
  else if (strcmp(aa,"user4")==0) return(44);
  else if (strcmp(aa,"user5")==0) return(45);
  else if (strcmp(aa,"user6")==0) return(46);
  else if (strcmp(aa,"user7")==0) return(47);
  else if (strcmp(aa,"user8")==0) return(48);
  else if (strcmp(aa,"user9")==0) return(49);
  else if (strcmp(aa,"dist")==0) return(50);
  else if (strcmp(aa,"az")==0) return(51);
  else if (strcmp(aa,"baz")==0) return(52);
  else if (strcmp(aa,"gcarc")==0) return(53);
  else if (strcmp(aa,"depmen")==0) return(56);
  else if (strcmp(aa,"cmpaz")==0) return(57);
  else if (strcmp(aa,"cmpinc")==0) return(58);
  else if (strcmp(aa,"kevnm")==0) return(60);
  else if (strcmp(aa,"kstnm")==0) return(61);
  else if (strcmp(aa,"kcmpnm")==0) return(62);
  else if (strcmp(aa,"knetwk")==0) return(63);
  else if (strcmp(aa,"kztime")==0) return(70);
  else if (strcmp(aa,"npts")==0) return(79);
  else if (strcmp(aa,"ko")==0) return(118);
  else if (strcmp(aa,"ka")==0) return(120);
  else if (strcmp(aa,"kt0")==0) return(122);
  else if (strcmp(aa,"kt1")==0) return(124);
  else if (strcmp(aa,"kt2")==0) return(126);
  else if (strcmp(aa,"kt3")==0) return(128);
  else if (strcmp(aa,"kt4")==0) return(130);
  else if (strcmp(aa,"kt5")==0) return(132);
  else if (strcmp(aa,"kt6")==0) return(134);
  else if (strcmp(aa,"kt7")==0) return(136);
  else if (strcmp(aa,"kt8")==0) return(138);
  else if (strcmp(aa,"kt9")==0) return(140);
  else if (strcmp(aa,"kf")==0) return(142);
  else return(-999);
}
