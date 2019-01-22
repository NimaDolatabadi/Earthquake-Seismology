

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

static int les_linje2();
static int parse_line();
static int exists();
static void S_REC();
static void NOWTIME();
static int read_parameter(int argcount, char **argvec);
static void help (void);

char  *topdir      = 0;                       // top directory SEISAN
char  command[500];
int   ret;
int   cmd=0;
char  ID[25];
char  TIME[25];
char  LAT[25];
float LATF;
char  LON[25];
float LONF;
char  DEP[25];
float DEPF;
char  AG[25];
char  AGENCY[25];
char  MTYP[25];
char  MT[10];
char  MAG[25];
float MAGF;
char  WHERE[250];
char  snam[200];
char  s_fullpath[200];
int   FORMAT;
int   loop_forever=0;
int   ival;
int   tstart;
int   tstop;
int   diff;
char  fmt[30];
char  starttime[40];
char  endtime[40];
char  minmag[30];
char  mindep[30];
char  maxdep[30];
char  dbase[10];
float val;
int   scounter=0;
DIR   *dirp; 
struct dirent *direntp; 


time_t current_time;
time_t time_start;
time_t time_end;


int main(int argc, char **argv)
{
  FILE *sfile;
  FILE *inp;
  int  br1;
  int  i,j,tel;
  char record[600];
  int  cnt=0; 
  char dummy[200];
  char du[30];
  char year[5];
  char month[5];
  char day[5];
  char hour[5];
  char minute[5];
  char sec[5];
  char seconds[10]; 
  char events_p[200];
  char database[20];
  char spath[200];  
  char idsfile[20];
  char idnumber[20]; 
  char oldid[20];
  int  yr;
  int  mn;
  int  dy;
  int  ho;
  int  mi;
  int  sc;
  int  retur;
  int  ut; 
  
  char lin[200];
  
  char * line = NULL;
  size_t len = 0;
  ssize_t read;  


  
  char buffer[26];
  struct tm* tm_info;  
  char* c_time_string;

  FORMAT=5;

//------------------------------------------------------------------------
// set path to SEISAN_TOP as default
//------------------------------------------------------------------------
  topdir = (char*)getenv("SEISAN_TOP");
  if(topdir)
  {
    printf("SEISAN_TOP..................................: %s\n", topdir);    
  }else{
    printf("SEISAN_TOP not defined.\n");
    printf("Install SEISAN !\n");
    exit(0);    
  }
  sprintf(starttime,"                              ");
  sprintf(endtime,"                             ");

//-------------------------------------------------------------------------
// Process command line parameters
//-------------------------------------------------------------------------
  if (read_parameter(argc, argv) < 0)
  {
    printf("GET_WEB_LOCATIONS: Parameter processing failed\n\n");
    printf("                   Try '-h' for detailed help\n");
    exit(0); 
  }
  
  if(FORMAT == 5)
  {
    sprintf(fmt,"text");
  }else{
    printf("Format not supported !\n");
    exit(0);
  }


for(;;)
{
 
  if(loop_forever == 1)
  {
    current_time = time(NULL);
    time_start = current_time - tstart*60;//the time 10 minutes ago is 10*60
    time_end = current_time - tstop*60;
  
    c_time_string = ctime(&time_start);//convert the time tenMinutesAgo into a string format in the local time format
    printf("\n\n\n");

//  printf("The time 10 minutes ago in seconds from the epoch is: %i\n", (int)time_start);
//  printf("The time 10 minutes ago from the epoch in the local time format is: %s\n", c_time_string);

//    time(&time_start);
//    tm_info = localtime(&time_start);
    tm_info = gmtime(&time_start);    
    strftime(buffer, 26, "%Y-%m-%dT%H:%M:%S", tm_info);
    printf("start_time: %s\n",buffer);
    sprintf(starttime,"%s",buffer);
    
//    tm_info = localtime(&time_end);
    tm_info = gmtime(&time_end);    
    strftime(buffer, 26, "%Y-%m-%dT%H:%M:%S", tm_info);
    printf("end_time  : %s\n",buffer);
    sprintf(endtime,"%s",buffer);    
  }
  
  
  sprintf(command,"wget -T 1 -O get_web_locations.txt %chttps://earthquake.usgs.gov/fdsnws/event/1/query?format=%s&starttime=%s&endtime=%s&minmagnitude=%s&mindepth=%s&maxdepth=%s%c 2>request.log ",0x22,fmt,starttime,endtime,minmag,mindep,maxdep,0x22);  
  printf("%s\n",command);
  
  ret = system(command);    
  
  if ((sfile = fopen ("get_web_locations.txt", "r")) == NULL)
  {
    printf("GET_WEB_LOCATIONS: Can't open get_web_locations.txt file");
    return(-1);
  }
  tel=0;   
    
  while(fgets(lin,200,sfile)!=NULL)
  {
    printf("%s", lin);
    sprintf(record,"%s",lin);
    tel++;
    if(tel > 1)
    {
      parse_line(record);
      for(i=0;i<200;i++)
      {
        snam[i]='\0';
        s_fullpath[i]='\0';
      } 
      sscanf(TIME,"%4d",&yr);    
      sprintf(year,"%4d",yr);    
      sscanf(TIME,"%5s%2s",dummy,month);
      sscanf(TIME,"%8s%2s",dummy,day);
      sscanf(TIME,"%11s%2s",dummy,hour);
      sscanf(TIME,"%14s%2s",dummy,minute);
      sscanf(TIME,"%17s%2s",dummy,sec);
      sscanf(TIME,"%20s%3s",dummy,seconds);    
      sscanf(year,"%d",&yr);
      sscanf(month,"%d",&mn);
      sscanf(day,"%d",&dy);
      sscanf(hour,"%d",&ho);
      sscanf(minute,"%d",&mi);
      sscanf(sec,"%d",&sc);
      sprintf(snam,"%2s-%2s%2s-%2sR.S%4s%2s",day,hour,minute,sec,year,month);  // s-file name
      for(i=0;i<19;i++)                 // replace ' ' with'0'
        if(snam[i]==' ')snam[i]='0';

#ifdef __WIN32__

      sprintf(database,"%s",dbase);
      sprintf(spath,"\%s\\REA",topdir);
      sprintf(events_p,"%s\\%s",spath,database);
      sprintf(dummy,"%s\\%s\\%s",events_p,year,month);
      printf("Create_Sfile_P DUMMY................: %s\n",dummy);
      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      } 
      closedir(dirp);      
//--------------------------------------------
// make full path for S-file
//--------------------------------------------
      sprintf(s_fullpath,"%s\\%s",dummy,snam);
//      printf("%s\n",s_fullpath);
      retur=exists(s_fullpath);   // new
      printf("retur: %d\n",retur);
#else
//      printf("Linux\n");
      sprintf(database,"%s",dbase);
      sprintf(spath,"%s/REA",topdir);

      dirp=opendir(spath);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",spath);
        sprintf(command,"mkdir %s",spath);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }   
      closedir(dirp);
      sprintf(events_p,"%s/%s",spath,database);

      dirp=opendir(events_p);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",events_p);
        sprintf(command,"mkdir %s",events_p);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }   
      closedir(dirp);
      sprintf(dummy,"%s/%s",events_p,year);

      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",dummy);
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      }
      closedir(dirp);
      sprintf(spath,"%s/%s",dummy,month);
      sprintf(dummy,"%s",spath);
      dirp=opendir(dummy);
      if(dirp == NULL)
      {
        printf("%s does not exist, create it\n",dummy);
        sprintf(command,"mkdir %s",dummy);
        ret=system(command);
      }else{
//        printf("Exist....goon\n");
      } 
      closedir(dirp);
//--------------------------------------------
// make full path for S-file
//--------------------------------------------
      sprintf(s_fullpath,"%s/%s",dummy,snam);
      retur=exists(s_fullpath);   // new
//      printf("retur: %d\n",retur);  
#endif

      for(;;)
      {
        ut = 0;

        retur=exists(s_fullpath);  // s-file with same name ?
//        printf("HERE: %s  retur: %d\n",s_fullpath,retur);        
        if(retur == 1)
        {
          if ((inp = fopen (s_fullpath, "r")) == NULL)
          {
            printf("GET_WEB_LOCATIONS: Can't open %s\n",s_fullpath);
            return(-1);
          }
          fgets(lin,200,inp);
          fgets(lin,200,inp);
          fclose(inp);
          sscanf(lin,"%s %s %s",du,du,oldid);
          ret=strcmp(oldid,ID);
        
          if(ret != 0)
          {
      
            sc=sc+1;
            if(sc > 59)
            {
              sc = 0;
              mi = mi +1;
              if(mi > 59)
              {
	        mi = 0;
	        ho = ho+1;
              }
            }
            sprintf(hour,"%2d",ho);
            sprintf(minute,"%2d",mi);
            sprintf(sec,"%2d",sc);
            sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2sd",year,month,day,hour,minute,sec);
            sprintf(snam,"%2s-%2s%2s-%2sR.S%4s%2s",day,hour,minute,sec,year,month);
            for(i=0;i<19;i++)
              if(snam[i]==' ')snam[i]='0';

            sprintf(s_fullpath,"%s/%s",dummy,snam);

            ut = 1;
            break;
          }else{
            sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2s",year,month,day,hour,minute,sec);
            sprintf(idnumber,"%s",ID);              
            ut=1;
            break;
          }
        }else{
          sprintf(idsfile,"ID:%4s%2s%2s%2s%2s%2s",year,month,day,hour,minute,sec);
          sprintf(idnumber,"%s",ID);
          ut = 1;
        }
        if(ut == 1)
          break;
      }

      S_REC(year,month,day,hour,minute, sec, dummy,s_fullpath,snam,idsfile,idnumber);  

    }
  }  // eof while
  fclose(sfile); 
  if(loop_forever == 1)
  {
     diff=(tstart-tstop)*60;
     printf("sleep %d  %d\n",diff,diff/(60));
     sleep(diff);   
  }else{
    exit(0);
  }
  
  }    
    
    
}


void S_REC(year,month,day,hour,minute,seconds,dummy,fullpath,filename,idsfile,idnumber)
char year[];
char month[];
char day[];
char hour[];
char minute[];
char seconds[];
char dummy[];
char fullpath[];
char filename[];
char idsfile[];
char idnumber[];

{

  FILE    *sf;
  char buf[256];
  char comp[200];
  char dm1[200];
  char dm2[200];
  int hr1;
  int mn1;
  int sc1;
  int ms1;

  int i,l;
  int ch; 
  int cnt;
  int ret;
  int YR,MTH,DAY,HR,MIN;
  float SEC;
  int AR;
  int cyr;
  int cmon;
  int cday;
  int chrn;
  int cmin;
  int cisec;
//printf("SECONDS: %s\n",seconds);
  sscanf(year,"%d",&YR);
//printf("%s %d\n",year,YR);
  sscanf(month,"%d",&MTH);
//printf("%s %d\n",month,MTH);
  sscanf(day,"%d",&DAY);
//printf("%s %d\n",day,DAY);
  sscanf(hour,"%d",&HR);
//printf("%s %d\n",hour,HR);
  sscanf(minute,"%d",&MIN);
//printf("%s %d\n",minute,MIN);
  sscanf(seconds,"%f",&SEC);
//printf("%s %f\n",seconds,SEC);

//fclose(sf);
    sf=fopen(fullpath,"w");
    if(sf == NULL)
    {

      printf("GET_WEB_LOCATIONS: S_REC Can't open S-file: %s  SCOUNTER: %d\n",fullpath,scounter);
      perror(fullpath);
      exit(EXIT_FAILURE);      
//    for(i=0;i<52;i++)
//      printf("%2d %2x %c\n",i,fullpath[i],fullpath[i]);
      exit(0);
  }else{
    for(i=0;i<100;i++)
    buf[i]=' ';
//  printf("RTDET: S_REC.............................: %4d %2d%2d %2d%2d %4.1f\n",YR,MTH,DAY,HR,MIN,SEC);
    sscanf(LAT,"%f",&LATF);
    sscanf(LON,"%f",&LONF);
    sscanf(DEP,"%f",&DEPF);
    sscanf(MAG,"%f",&MAGF);
    sscanf(TIME,"%17s%6s",dummy,dm1);
    sscanf(dm1,"%f",&SEC);

    cnt=0;
    if(MTYP[0] == 'm' && MTYP[1] == 'b')
      cnt=1;
    if(MTYP[0] == 'M' && MTYP[1] == 'L')
      cnt=2;      
    if(MTYP[0] == 'm' && MTYP[1] == 'B')
      cnt=3;
    if(MTYP[0] == 'M' && MTYP[1] == 's')
      cnt=4;
    if(MTYP[0] == 'M' && MTYP[1] == 'S')
      cnt=5;  
    if(MTYP[0] == 'M' && MTYP[1] == 'W')
      cnt=6;  
    if(MTYP[0] == 'M' && MTYP[1] == 'c')
      cnt=7;  
    if(MTYP[0] == 'm' && MTYP[1] == 'w')
      cnt=8;
    if(MTYP[0] == 'w' && MTYP[1] == 'w')
      cnt=9; 
    if(MTYP[0] == 'w' && MTYP[1] == 'r')
      cnt=10;   
    if(MTYP[0] == 'm' && MTYP[1] == 'l')
      cnt=11;   
    if(MTYP[0] == 'm' && MTYP[1] == 'd')
      cnt=12;    
  
    switch(cnt)
    {
      case 0:
      MT[0]=' ';
      MT[1]='\0';
      break;      
      case 1:
      MT[0]='b';
      MT[1]='\0';
      break;
      case 2:
      MT[0]='L';
      MT[1]='\0';
      break;
      case 3:
      MT[0]='B';
      MT[1]='\0';
      break;
      case 4:
      MT[0]='s';
      MT[1]='\0';
      break;
      case 5:
      MT[0]='S';
      MT[1]='\0';
      break;
      case 6:
      MT[0]='W';
      MT[1]='\0';
      break;
      case 7:
      MT[0]='C';
      MT[1]='\0';
      break;
      case 8:
      MT[0]='W';
      MT[1]='\0';
      break;  
      case 9:
      MT[0]='W';
      MT[1]='\0';
      break;   
      case 10:
      MT[0]='W';
      MT[1]='\0';
      break;   
      case 11:
      MT[0]='L';
      MT[1]='\0';
      break; 
      case 12:
      MT[0]='C';
      MT[1]='\0';
      break;       
    }
  
    sprintf(buf," %4d %2d%2d %2d%2d %4.1f R %7.3f%8.3f%5.1f  %s                       %4.1f%s%s",YR,MTH,DAY,HR,MIN,SEC,LATF,LONF,DEPF,AGENCY,MAGF,MT,AGENCY); 
//    for(i=48;i<73;i++)
//      buf[i]=' ';
    buf[79]='1';
    buf[80]='\0';
    printf("%s\n",buf);
    fprintf(sf,"%s\n",buf);
  
// provider id line
  
    for(i=0;i<100;i++)
      buf[i]=' ';
    sprintf(buf," WEB event-id: %s",idnumber);
    for(i=0;i<40;i++)
    {
      if(buf[i] == '\0')
        buf[i] = ' ';
    }
    buf[79]='3';
    buf[80]='\0';  
    fprintf(sf,"%s\n",buf);   
  
    for(i=0;i<100;i++)
      buf[i]=' ';
    l=11;
    sprintf(buf," LOCALITY: ");
    for(i=0;i<90;i++)
    {
      if(WHERE[i] != '\0')
      {
        buf[l]=WHERE[i];
        l++;
      }else{
        break;
      }
      buf[79]='3';
      buf[80]='\0';
    }
    fprintf(sf,"%s\n",buf);  
  
    for(i=0;i<100;i++)
      buf[i]=' ';
//printf("In S_REC: FILENAME: %s\n",filename);
    sprintf(buf," %s",filename);

//  sprintf(buf," 2017-11-01-2329-16.USGS");

    for(i=0;i<100;i++)
      if(buf[i]=='\0')buf[i]=' ';
    buf[79]='6';
    buf[80]='\0';
//  fprintf(sf,"%s\n",buf);

    sprintf(dm1,"%4d",YR);
    sscanf(dm1,"%2c%d",dm2,&AR);

    for(i=0;i<100;i++)
      buf[i]=' ';
  
    NOWTIME(&cyr,&cmon,&cday,&chrn,&cmin,&cisec);  
  
    sprintf(buf," ACTION:NEW %2d-%2d-%2d %2d:%2d OP:SEIS STATUS:               %s",cyr,cmon,cday,chrn,cmin,idsfile);  
//  sprintf(buf," ACTION:NEW %2d-%2d-%2d %2d:%2d OP:SEIS STATUS:               %s",AR,MTH,DAY,HR,MIN,idsfile);  

    if(buf[12]==' ')buf[12]='0';
    if(buf[15]==' ')buf[15]='0';
    if(buf[18]==' ')buf[18]='0';
    if(buf[21]==' ')buf[21]='0';
    if(buf[24]==' ')buf[24]='0';
  
  

    for(i=0;i<100;i++)
      if(buf[i]=='\0')buf[i]=' ';
    for(i=59;i<73;i++)
      if(buf[i]==' ')buf[i]='0';
    buf[79]='I';
    buf[80]='\0';
    fprintf(sf,"%s\n",buf);                 
    sprintf(buf," STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO AIN AR TRES W  DIS CAZ7");
    fprintf(sf,"%s\n",buf);

// STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO AIN AR TRES W
// BER  BZ IP       2223  6.00    4
    scounter++;

    ret=fclose(sf);
//    printf("scounter: %d  fclose: %d\n",scounter,ret); 

  }
}

void NOWTIME(nyrn,nmon,nday,nhrn,nmin,nsek)
int *nyrn;
int *nmon;
int *nday;
int *nhrn;
int *nmin;
int *nsek;
{
  char dummy[256];

  int   iyrn;
  int   imon;
  int   iday;
  int   ihrn;
  int   imin;
  int   isec;
  int   i,j;
  int   BUFSECS;
  float  sek;
  char dum[50];

  time_t now = time(NULL);
  struct tm *now_s = localtime(&now);
  sprintf(dummy,"%d-%02d-%02d_%02d:%02d:%02d", 1900+now_s->tm_year, ++now_s ->tm_mon,now_s->tm_mday, now_s->tm_hour, now_s->tm_min,now_s->tm_sec);
    sscanf(dummy,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%2d",&iyrn,dum,&imon,dum,&iday,dum,&ihrn,dum,&imin,dum,&isec);
  iyrn=iyrn-2000;
  *nyrn=iyrn;
  *nmon=imon;
  *nday=iday;
  *nhrn=ihrn;
  *nmin=imin;
  *nmon=imon;
  *nsek=isec;
}

int exists(const char *fname)
{
  FILE *file;
  if (file = fopen(fname, "r"))
  {
    fclose(file);
    return 1;
  }
  return 0;
}


int parse_line(char line[])
{
  int k,j,n;
  int tel;
    
  for(k=0;k<25;k++)
  {
    ID[k]   = '\0'; 
    TIME[k] = '\0';
    LAT[k]  = '\0';
    LON[k]  = '\0';
    MTYP[k] = '\0';
    MAG[k]  = '\0';
    DEP[k]  = '\0';
    AG[k]   = '\0';
    AGENCY[k]  = '\0';
  }
  for(k=0;k<250;k++)
    WHERE[k] = '\0';
  
  for(k=0;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
      ID[k]=line[k];
    }else{
//      printf("ID.......: %s\n",ID);
      break;
    }
  }
//----------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 1)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      TIME[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("TIME.....: %s\n",TIME);  
//------------------------------------------  

  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 2)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      LAT[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("LAT......: %s\n",LAT);  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 3)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      LON[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("LON......: %s\n",LON);  
//------------------------------------------  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 4)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      DEP[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("DEP......: %s\n",DEP);  
//------------------------------------------ 
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 6)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      AG[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("AG.......: %s  AG[0] = %c  AG[1] = %c AG[2] : %x\n",AG,AG[0],AG[1],AG[2]);
  if(n==2)
  {
    AGENCY[0] = ' ';
    AGENCY[1] = AG[0];
    AGENCY[2] = AG[1];
    AGENCY[3] = '\0';
  }
  if(AGENCY[0] == ' ' && AGENCY[1] == 'u' && AGENCY[2] == 's')
  {
    AGENCY[0]='U';
    AGENCY[1]='S';
    AGENCY[2]='G';
  }
//  printf("AGENCY: %s\n",AGENCY);
//------------------------------------------      
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 9)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      MTYP[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("MTYP.....: %s\n",MTYP);  
//------------------------------------------  
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 10)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
  for(k=j;k<600;k++)
  {
    if(line[k] != 0x7c)
    {
//      printf("%c\n",line[k]);
      MAG[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("MAG......: %s\n",MAG);  
//------------------------------------------   
  tel=0;
  n=0;
  for(k=0;k<600;k++)
  {
    if(line[k] == 0x7c)
        tel++;
    if(tel == 12)
    {
       j=k;
       break;
    }
  }
//  printf("J=%d\n",j);
  j++;
//  sprintf(WHERE," LOCALITY: ");
  for(k=j;k<600;k++)
  {
    if(line[k] != 0xa)
    {
//      printf("%c\n",line[k]);
      WHERE[n]=line[k];
      n++;
    }else{
      break;
    }
  }
//  printf("WHERE....: %s\n",WHERE);  
//------------------------------------------   
}
int les_linje2(FILE *streamfp,char linje[])
{
  int l;
  int br;
  br=0;
  for(l=0;l<600;l++)
  {
    linje[l]=fgetc(streamfp);
//    printf("%2d %2x %c\n",l,linje[l],linje[l]);
    if(linje[l] == 0xa)
    {
      br=2;
      break;
    }

    if(linje[l] == EOF)
    {
      printf("End of file.\n");

      br=1;
      return(br);
    }
  }
  return(br);
}

/***************************************************************************
 * read_parameter:
 *
 * Process the command line parameters.
 *
 * Returns 0 on success, and -1 on failure
 ***************************************************************************/
static int
read_parameter(int argcount, char **argvec)
{
  int optind;
  int error = 0;
  int len;
  int j;

  if (argcount <= 1)
  {
    help();
    return(-1);
  }
  sprintf(mindep,"0");                                  // default minimum depth
  sprintf(maxdep,"800");                                // default maximum depth
//printf("argcount: %d error: %d\n",argcount,error);
  /* Process all command line arguments */
  for (optind = 1; optind < argcount; optind++)
  {
    if (strcmp (argvec[optind], "-h") == 0)
    {
      help();
      exit (0);
    }
//    else if (strcmp (argvec[optind], "-fmt") == 0)         // input format
//    {
//      strcpy(command, argvec[++optind]);
//      sscanf(command,"%f",&val);
//      FORMAT = (int)val;
//    }
    else if (strcmp (argvec[optind], "-loop") == 0)         // input loop or one query
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&ival);
      loop_forever = ival;
    }      
    else if (strcmp (argvec[optind], "-tsrt") == 0)         // input tstart
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&ival);
      tstart = ival;
    }
    else if (strcmp (argvec[optind], "-tstp") == 0)         // input tstop
    {
      strcpy(command, argvec[++optind]);
      sscanf(command,"%d",&ival);
      tstop = ival;
    }       
    else if (strcmp (argvec[optind], "-mag") == 0)         // input longitude
    {
      strcpy(command, argvec[++optind]);
      sprintf(minmag,"%s",command);
//      printf("%s\n",minmag);
    }
    else if (strcmp (argvec[optind], "-mind") == 0)         // input minimum depth
    {
      strcpy(command, argvec[++optind]);
      sprintf(mindep,"%s",command);
//      printf("%s\n",mindep);
    }    
    else if (strcmp (argvec[optind], "-maxd") == 0)         // input maximum depth
    {
      strcpy(command, argvec[++optind]);
      sprintf(maxdep,"%s",command);
//      printf("%s\n",maxdep);
    }       
    else if (strcmp (argvec[optind], "-srt") == 0)         // start-time
    {
      strcpy(command, argvec[++optind]);
      sprintf(starttime,"%s",command);
    }
    else if (strcmp (argvec[optind], "-end") == 0)         // end-time
    {
      strcpy(command, argvec[++optind]); 
      sprintf(endtime,"%s",command);
    }    
    else if (strcmp (argvec[optind], "-db") == 0)          // data base
    {
      strcpy(command, argvec[++optind]);
      len=strlen(command);
      sprintf(dbase,"%s",command);
      if(len > 5)
      {
        printf("Data-base name can not be more than 5 characters !\n");  
        exit(0);
      }else{
        for(j=0;j<5;j++)
          dbase[j]='_';
        for(j=0;j<len;j++)
          dbase[j]=command[j];
        dbase[5]='\0';
//        printf("Dbase: %s\n",dbase);
      }
    }    
 

      else
      {
          return -1;
	  fprintf(stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
      }
  }


  
  return 0;
}
//------------------------------------------------------------------------
// help
//------------------------------------------------------------------------
static void
help (void)
{
  fprintf(stderr,
    "Valid program options:\n"
    "-h                 show this help info\n"
    "-loop int          1-loop forever, 0-one query (0=default)\n"
    "-tsrt int          minutes back, start-time\n"
    "-tstp int          minutes back, stop-time\n"
    "-mag  float        minimum magnitude\n"
    "-mind text         minimum depth\n"
    "-maxd text         maximum depth\n"    
    "-db   text         SEISAN database name, max 5 characters\n"
    "-srt  text         start-time, format: 2018-02-20T03:45:00\n"
    "-end  text         end-time, format  : 2018-02-22T08:23:00\n"
     "\n");
}