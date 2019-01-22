// Program to search in seisan nordic file the calculated stress drop,
// user must be call it with command line argumens. The fist argument 
// is obligatory and the rest are optionals. The program can be call
// the following way:
//
// getstressdrop <nordic.inp> [-c <stress_cut>] [-s <stations.lis>] [-p <points_polygon.inp>]
//
// the <stess_cut> is the maximum stress drop to consider in the searching process.
// the <stations.lis> is a file with the station-channels list, with the same format 
// that is written in the line SPEC of the S-FILES nordic format.
// the <points_poligon.inp> is the file with the longitude latitude. The last points
// of the polygon must be equal to the first (close polygon)
//
// Output will be a file <getstressdrop_histo.out>, with the number of stressdrop
// by step of 10 bars. 
// and a file <getstressdrop_mw.out> with the list of earthquake magnitude vs stressdrop
// Also is printed out the average stress drop. 
//
// Written by Bladimir Moreno, August 30, 2014
//-----------------------------------------------------------------------
#include <stdio.h>
#include <string.h>

char stalist[100][20];    // to load the stations-channels

// Subroutine to search in a polygon. Return 1 if found it, else return 0

int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy)
{
  int i, j, c = 0;
  if(!nvert) return 1;
  for (i = 0, j = nvert-1; i < nvert; j = i++) {
    if ( ((verty[i]>testy) != (verty[j]>testy)) &&
	 (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
       c = !c;
  }
  return c;
}

// Subroutine to search if station-chanel is the user list. Return 1 if found it,else return 0

int stainclude(int nsta,char* staname) {
 int i=0;
 int found = 0;
 if(!nsta) return 1;
 for(i=0; i<nsta; ++i)
 if(strstr(stalist[i],staname)) { found=1; break; }
 return found;
}

//  main program

int main(int argc, char *argv[]) {
 FILE *f;
 FILE *ft,*fout;
 int i,nsta,npoints,first,j;
 float latitude[100],longitude[100];  // to load polygon points
 float xlon,ylat,stressdrop,maxstressdrop;
 char line[100],cad[20],station[20];
 float stress[90000];                 // to load the stress from nordic input
 float fval;
 float mw[90000];                     // to load moment magnitude
 float stresscut = 900;               // defauls stessdrop cut
 int k;
 int class[1000];                     // count for the stress range
 float avg_mw[9000];                  // average for Mw for individual earthquake
 float avg_stress[9000];               // average for stressdrop for individual earthquake
 int nquake[9000];                    // to control individual earthquakes
 int numquakes;                       // total of earthquakes
int poly_flag = 0;
int station_flag = 0;
char  stationlist_file[80];
char  poly_file[80];

 if(argc < 2) {
  printf("use getstressdrop <nordic.inp> [ -c <stress_cut> ] [ -s  <stationslist_file.inp> ] [ -p <points_polygon.inp> ]\n");
 return;
 }


 if(argc>2) {
  for(i=2; i<argc;++i) {
   if(!strcmp(argv[i],"-c")) sscanf(argv[i+1],"%f",&stresscut);   // set stress cut
   if(!strcmp(argv[i],"-s")) { station_flag = 1; strcpy(stationlist_file,argv[i+1]); }  // set stationlist file 
   if(!strcmp(argv[i],"-p")) { poly_flag = 1; strcpy(poly_file,argv[i+1]); } // set polygon file 
  }
 }
 nsta=0;                              
 npoints=0;                           
 if(station_flag) {                         // load station-channels list from the input file
  ft = fopen(stationlist_file,"r");
   if(!ft) {
    printf("file %s no found\n",stationlist_file);
    return;
  }
  i=0;
  while(fgets(line,15,ft)) {
   strcpy(stalist[i++],line);
  }
  nsta=i;                             // number of station-channels
  fclose(ft);
 }
 if(poly_flag) {                         // load polygon from the input file
  ft = fopen(poly_file,"r");
  if(!ft) {
   printf("file %s no found\n",poly_file);
   return;
  }
  i=0;
  while(fgets(line,40,ft)) {
   sscanf(line,"%f %f",&latitude[i],&longitude[i]);
   ++i;
  }
  npoints = i;                        // number of polygon points
  fclose(ft);
 }
 f = fopen(argv[1],"r");
 if(!f) {
  printf("file %s no found\n",argv[1]);
  return;
 }
 first = 1;                           // to control individual earthquake
 j=0;                                 // count stressdrop and Mw selected
 i=0;                                 // count number of earthquakes
 int nj=0;                            // count total number of channels used
 while(fgets(line,90,f)) {
  if((line[79]=='1') && first ) {     // get latitude and longitude of eathquake
   first = 0;
   strncpy(cad,line+23,7);
   cad[7]=0;
   sscanf(cad,"%f",&ylat);
   strncpy(cad,line+30,8);
   cad[8]=0;
   sscanf(cad,"%f",&xlon);
   line[80]=0;                      // remove return
   if(i>0) printf("                                     \n"); // blank line, new earthquake
   printf("%s\n",line);
   ++i;
  }
  if(strstr(line,"SPEC") && strstr(line,"MO") && strstr(line,"ST") && !strstr(line,"SPEC AVERAGE") && !strstr(line,"SPEC SD")) { // get stressdrop and Mw
   strncpy(station,line+6,8);
   station[8]=0;
   strncpy(cad,line+75,3);
   cad[3]=0;
   sscanf(cad,"%f",&fval);               // get Mw
   if(fval>0) {                          // ignore stressdrop when Mw=0
    strncpy(cad,line+25,5);
    cad[5]=0;
    sscanf(cad,"%f",&stressdrop);
    if((stressdrop < stresscut) && (stressdrop > 1) && stainclude(nsta,station) && pnpoly(npoints,longitude,latitude,xlon,ylat) ){
     line[79]=0;                    // remove return
     printf("%.79s\n",line);             // stressdrop selected 
     stress[j]=stressdrop;
     mw[j]=fval;
     nquake[j]=i;                   // to control same earthquake
     ++j;
     ++nj;
    }
   }
  }
  if(!strncmp(line,"   ",3))  first = 1;      // new earthquake
 }
 fclose(f);
 if(j==0) {                       // no selection found
  printf("%s\n","No earthquake was selected by the criteria");
  return;
 }
 
// Calculate average of stressdrop and Mw for individual earthquakes

 float summw=mw[0];
 float sumstress=stress[0];
 int l=1;                // count earthquakes selected by criteria
 int n=1;                // count stressdrop for individual earthqauke
 for(k=0; k<(stresscut/10);++k) class[k]=0;
 for(i=1; i<j; ++i) {
  if(nquake[i]==nquake[i-1]) {   // same earthquake
   sumstress+=stress[i];
   summw+=mw[i];
   ++n;
  }
  else {                  // new earthquake
   avg_stress[l-1] = sumstress/n;
   avg_mw[l-1] = summw/n;
   sumstress=stress[i];
   summw=mw[i];
   n=1;
   ++l;
  }
 }
 avg_stress[l-1] = sumstress/n;     // average for the last earthquake
 avg_mw[l-1] = summw/n;
 numquakes=l;                     // total of earthquakes selected

// Compute histogram and average stress and create output file for Mw vs stressdrop
 
 sumstress = 0;
 fout = fopen("getstressdrop_mw.out","w");
 for(i=0; i<numquakes; ++i) {
   sumstress+=avg_stress[i];
   for(k=0; k<(stresscut/10);++k) 
    if((avg_stress[i]>(k*10)) && (avg_stress[i]<=((k+1)*10))) class[k]++;
   fprintf(fout,"%5.2f   %6.1f\n",avg_mw[i],avg_stress[i]);
 }
 fclose(fout);

// Calculate max average stressdrop
 
 maxstressdrop = 0;
 for(i=0; i<numquakes;++i)
     if(maxstressdrop<avg_stress[i]) maxstressdrop=avg_stress[i];   

 stresscut = maxstressdrop;   // set stresscut to the max available

// Create output file for the histogram

 fout=fopen("getstressdrop_histo.out","w");
 for(k=0; k<(stresscut/10);++k) 
   fprintf(fout,"%4d  %4d\n",k*10+5,class[k]);
 fclose(fout);
 
 printf("\nAverage stress drop for %d earthquakes selected: %6.1f\n",numquakes,sumstress/numquakes);
 printf("Total number of channels used: %d\n",nj);
 printf("File with stress drop histogram is getstressdrop_hist.out \n");
 printf("File with stress drop as a function of magnitude mw is getstressdrop_mw.out\n");
 
}

