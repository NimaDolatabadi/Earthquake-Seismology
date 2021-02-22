/************************************************************
*	Calculate the SAC orig. time based on kztime and
*	Catalog Value	
*	Usage
*		gsact year month day hour min sec minsec f sac_files ...
*       Got from Lupei Zhu
*       Modified by Zhigang Peng, Mon Aug 13 11:34:37 PDT 2001
*       Modified Hisory
*	Change the filename from sacgeto to gsact
*	which can be used both on the arrival and origin time
*************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sac.h"
#include <math.h>

#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)

int julian(int year,int month, int day);

int main(int argc, char **argv)
{
  int		i,j,cat[10],nl,jday;
  float		*pt;
  SACHEAD	hd;
  double		catday, kzday,tdiff,origtime;

  if(argc<9) {
      fprintf(stderr,"Usage: %s year month day hour min sec minsec f sac_files ...\n",argv[0]);
      fprintf(stderr,"     : Calculate the SAC origin and arrival time relative to kztime\n\tbased on catalog and arrivals\n");
      return -1;
  }

  nl=0;argv++;argc--;
  while ( *argv[0] != 'f' ) {
     cat[nl] = atoi(argv[0]);
     if (cat[nl]==-999) {
	fprintf(stderr, "error in header_list  %s\n",argv[0]);
	return -1;
     }
     nl++; argv++; argc--;
  }
/* for (i = 0; i<nl; i++) {
*	printf("orgin[%d] = %d\n",i,cat[i]);
*}
*/

/* convert the time into date.*/
 if (nl == 7) {
 jday = julian(cat[0],cat[1],cat[2]);
 catday = jday + cat[3]/24. + cat[4]/24./60. + cat[5]/24./60./60. + cat[6]/24./60./60./1000.;
 } else if (nl == 6) {
 catday = cat[1] + cat[2]/24. + cat[3]/24./60. + cat[4]/24./60./60. + cat[5]/24./60./60./1000.;
 }
  for (i=1;i<argc;i++) {
/*      printf("%s ",argv[i]);
*/
      if ( read_sachead(argv[i], &hd) != -1) {
      pt = (float *) &hd;
      kzday = hd.nzjday + hd.nzhour/24. + hd.nzmin/24./60. + hd.nzsec/24./60./60. + hd.nzmsec/24./60./60./1000.;
      tdiff = catday - kzday;
      if (cat[0] == hd.nzyear && abs(tdiff) <1) {
      origtime = tdiff*24*60*60;
/*      printf("Orgin Time referenced to kztime is %6.3f\n",origtime);
*     printf("%5d %3d %2d %2d %2d %3d",hd.nzyear,hd.nzjday,hd.nzhour,hd.nzmin,hd.nzsec,hd.nzmsec);
*	printf("r %s ; ch o %7.4f ; wh\n", argv[i], origtime);
*/
	printf("%s\t%7.4f\n", argv[i], origtime);
      }else {
	fprintf(stderr, "error in the catalog event orgin time !!\n");
      }
      }
  }

  return 0;

}


int julian(int year, int month, int day){
 int julianday = 0;
 int dom[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

 if(month>=13 || month <1) {
    fprintf(stderr,"Input Month Error!! Usage: julian -[j|d] day month year\n");
    return -1;
 }
 if(day>31 || day <1) {
    fprintf(stderr,"Input Day Error !! Usage: julian -[j|d] day month year\n");
    return -1;
  }
 if(year<0) {
    fprintf(stderr,"Input Year Error !! Usage: julian -[j|d] day month year\n");
    return -1;
 }
 if(isleap(year)) {
    dom[2] = 29;
  } else {
    dom[2] = 28;
  }
  for (month--;month; month--) {
    julianday +=dom[month];
   }
   julianday += day;

 /* printf("julian day is %d\n",julianday);*/
  return julianday;
} 

