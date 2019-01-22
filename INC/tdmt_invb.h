/* apr 27 2012 jh  comment out malloc.h, not found on mac, not needed */
/* jul 26 2013 jh  put in gaute hope's (eg@gaute.vetsj.com) suggested changes *//*                 see july 13 */ 


#include <math.h>
/* #include <malloc.h> */
#include <stdlib.h>  /* new july 13 */
#include <stdio.h>
#include <sys/file.h>
#include <string.h>
#define ABS(x) ((x) < 0.0 ? -1.0*(x) : (x))
#define PI 3.1415927
#define SWAP(a,b) {double temp=(a);(a)=(b);(b)=temp;} /* new july 13*/

struct MOMENT
 {
 float mxx;
 float myy;
 float mxy;
 float mxz;
 float myz;
 float mzz;
 int   isoflag;
 };

struct GREEN
 {
 int zz,nn;
 int np;
 float dt;
 float *u1; /* Tangential Strikeslip */
 float *u2; /* Tangential Dipslip    */
 float *u3; /* Radial     Strikeslip */
 float *u4; /* Radial     Dipslip    */
 float *u5; /* Radial     45_slip    */
 float *u6; /* Vertical   Strikeslip */
 float *u7; /* Vertical   Dipslip    */
 float *u8; /* Vertical   45_slip    */
 float *u9; /* Radial     Explosion  */
 float *u10; /* Vertical   Explosion    */
 };

struct DATA
 {
 int zz,nn;
 int np;
 float dt;
 float *t;  /* Tangential */
 float *r;  /* Radial     */
 float *z;  /* Vertical   */
 float dist;
 float azi;
 float vr;
 char name[50];
 };
