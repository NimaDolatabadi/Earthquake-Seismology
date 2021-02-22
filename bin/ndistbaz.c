#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* cc distbaz.c -o distbaz -lm */

/*
c
c Subroutine to calculate the Great Circle Arc distance
c    between two sets of geographic coordinates
c
c Given:  stalat => Latitude of first point (+N, -S) in degrees
c         stalon => Longitude of first point (+E, -W) in degrees
c         evtlat => Latitude of second point
c         evtlon => Longitude of second point
c
c Returns:  delta => Great Circle Arc distance in degrees
c           az    => Azimuth from pt. 1 to pt. 2 in degrees
c           baz   => Back Azimuth from pt. 2 to pt. 1 in degrees
c
c If you are calculating station-epicenter pairs, pt. 1 is the station
c
c Equations take from Bullen, pages 154, 155
c
c T. Owens, September 19, 1991
c           Sept. 25 -- fixed az and baz calculations
c
  P. Crotwell, Setember 27, 1994
            Converted to c to fix annoying problem of fortran giving wrong
               answers if the input doesn't contain a decimal point.

 modified by Zhigang Peng, Sun Jan 20 13:50:42 PST 2002
 Changes: 1. orders of inputs are changed to stalon stalat evtlon evtlat
	  2. output are in the format of distance (km) and baz (degrees)	
	  3. this version ndistbaz, take the station coordinate from argv
	     and event coordinate from stdin
*/

main(int argc, char *argv[]) 
{
   char          line[128];
   double stalat, stalon, evtlat, evtlon;
   double delta, az, baz;
   double scolat, slon, ecolat, elon;
   double a,b,c,d,e,aa,bb,cc,dd,ee,g,gg,h,hh,k,kk;
   double rhs1,rhs2,sph,rad,del,daz,dbaz,pi,piby2;
   double degree2km;

   degree2km = 111.195;
   if (argc != 3) {
      printf("Usage: cat eve_lon eve_lat | ndistbaz sta_lon sta_lat\n");
      printf("       Returns:  Distance Baz\n");
      exit(1);
   } else {
      stalon = atof(argv[1]);
      stalat = atof(argv[2]);
   }


   pi=3.141592654;
   piby2=pi/2.0;
   rad=2.*pi/360.0;

   while(gets(line)) {
      sscanf(line,"%lf %lf",&evtlon,&evtlat);
      if ((stalat == evtlat)&&(stalon == evtlon)) {
   	 printf("%9.4f %7.3f\n", 0.0,0.0);
      } else {
/*
c
c scolat and ecolat are the geocentric colatitudes
c as defined by Richter (pg. 318)
c
c Earth Flattening of 1/298.257 take from Bott (pg. 3)
c
*/
   sph=1.0/298.257;

   scolat=piby2 - atan((1.-sph)*(1.-sph)*tan(stalat*rad));
   ecolat=piby2 - atan((1.-sph)*(1.-sph)*tan(evtlat*rad));
   slon=stalon*rad;
   elon=evtlon*rad;
/*
c
c  a - e are as defined by Bullen (pg. 154, Sec 10.2)
c     These are defined for the pt. 1
c
*/
   a=sin(scolat)*cos(slon);
   b=sin(scolat)*sin(slon);
   c=cos(scolat);
   d=sin(slon);
   e=-cos(slon);
   g=-c*e;
   h=c*d;
   k=-sin(scolat);
/*
c
c  aa - ee are the same as a - e, except for pt. 2
c
*/
   aa=sin(ecolat)*cos(elon);
   bb=sin(ecolat)*sin(elon);
   cc=cos(ecolat);
   dd=sin(elon);
   ee=-cos(elon);
   gg=-cc*ee;
   hh=cc*dd;
   kk=-sin(ecolat);
/*
c
c  Bullen, Sec 10.2, eqn. 4
c
*/
   del=acos(a*aa + b*bb + c*cc);
   delta=del/rad;
/*
c
c  Bullen, Sec 10.2, eqn 7 / eqn 8
c
c    pt. 1 is unprimed, so this is technically the baz
c
c  Calculate baz this way to avoid quadrant problems
c
*/
   rhs1=(aa-d)*(aa-d)+(bb-e)*(bb-e)+cc*cc - 2.;
   rhs2=(aa-g)*(aa-g)+(bb-h)*(bb-h)+(cc-k)*(cc-k) - 2.;
   dbaz=atan2(rhs1,rhs2);
   if (dbaz<0.0) {
      dbaz=dbaz+2*pi;
   } 
   baz=dbaz/rad;
/*
c
c  Bullen, Sec 10.2, eqn 7 / eqn 8
c
c    pt. 2 is unprimed, so this is technically the az
c
*/
   rhs1=(a-dd)*(a-dd)+(b-ee)*(b-ee)+c*c - 2.;
   rhs2=(a-gg)*(a-gg)+(b-hh)*(b-hh)+(c-kk)*(c-kk) - 2.;
   daz=atan2(rhs1,rhs2);
   if(daz<0.0) {
      daz=daz+2*pi;
   }
   az=daz/rad;
/*
c
c   Make sure 0.0 is always 0.0, not 360.
c
*/
   if(abs(baz-360.) < .00001) baz=0.0;
   if(abs(az-360.) < .00001) az=0.0;

   printf("%9.4f %7.3f\n", delta*degree2km, baz);
   }
 }
   exit(0);
}
