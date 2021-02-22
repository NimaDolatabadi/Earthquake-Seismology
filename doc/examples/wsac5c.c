
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sacio.h>

#define NCOMP   11
#define NDATA   4000
#define NSTA    11
#define FALSE   0
#define TRUE    1

int
main(int argc, char *argv[])
{
  float sdata[NCOMP][NDATA], xdummy[NDATA];
  float evla, evlo, stla, stlo;
  char kevnm[NSTA] , *kstnm ;
  int nerr, ndata, j, i;
  float b, delta;
  float cmpaz, cmpinc;

  char kname[NCOMP][NSTA] = { "STAZ" , "STBZ" , "STCZ" , "STDZ" , "STEZ" ,
			      "STFZ" , "STGZ" , "STHZ" , "STHN" , "STHE", "STHN" } ;

  int true  = TRUE;
  
  b      = 0.0;
  delta  = 0.25;
  cmpaz  = 0.0;
  cmpinc = 0.0;
  ndata  = NDATA;
  evla   = -23.56;
  evlo   = 123.56;

  newhdr () ;
  setihv("IFTYPE", "ITIME", &nerr , strlen("IFTYPE"), strlen("ITIME"));
  setihv("IZTYPE", "IB",    &nerr , strlen("IZTYPE"), strlen("IB"));
  setfhv("B",      &b,      &nerr , strlen("B"));
  setlhv("LEVEN",  &true,   &nerr , strlen("LEVEN"));
  setfhv("DELTA",  &delta,  &nerr , strlen("DELTA")) ;
  
  strcpy(kevnm, "Event Name");
  
  setnhv("NPTS",   &ndata,    &nerr, strlen("NPTS"));
  setfhv("EVLA",   &evla,     &nerr, strlen("EVLA"));
  setfhv("EVLO",   &evlo,     &nerr, strlen("EVLO"));
  setkhv("KEVNM",  &kevnm[0], &nerr, strlen("KEVNM"), SAC_STRING_LENGTH);
  setfhv("CMPAZ",  &cmpaz,    &nerr, strlen("CMPAZ"));
  setfhv("CMPINC", &cmpinc,   &nerr, strlen("CMPINC"));

  for ( j = 0 ; j < NCOMP - 2 ; j++ )
    {
      kstnm = kname[j]  ;
      setkhv ( "KSTNM", kstnm, &nerr, strlen("KSTNM"), strlen(kstnm));
      stla = j * 10;
      stlo = j * 20;
      for(i = 0; i < NDATA; i++) {
	sdata[j][i] = 1.0 * rand()/INT32_MAX;
      }
      setfhv ( "STLA" , &stla , &nerr , strlen("STLA"));
      setfhv ( "STLO" , &stlo , &nerr , strlen("STLO"));
      wsac0 ( kstnm, xdummy, sdata[j], &nerr, strlen(kstnm));
    }
  
  cmpinc = 90.0;
  setfhv("CMPINC", &cmpinc, &nerr, strlen("CMPINC")) ;
  j = 9;
  for(i = 0; i < NDATA; i++) {
    sdata[j][i] = 1.0 * rand()/INT32_MAX;
  }
  wsac0(kname[9], xdummy, sdata[9], &nerr, strlen(kname[9]));
  
  cmpaz = 90.0;
  setfhv("CMPAZ", &cmpaz, &nerr, strlen("CMPAZ")) ;
  j = 10;
  for(i = 0; i < NDATA; i++) {
    sdata[j][i] = 1.0 * rand()/INT32_MAX;
  }
  wsac0(kname[10], xdummy, sdata[10], &nerr, strlen(kname[10]));

  return 0;
}
