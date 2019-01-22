//
// Version 17/01/17
//
//------------------------------------------------------------
// gcc -O -I../INC -o rtpick rdtrigL.c -lm ../LIB/seisan.a
//------------------------------------------------------------
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <dirent.h>
#include <libmseed.h>
#include "ew_bridge.h"
#include "PickData.h"
#include "FilterPicker5_Memory.h"
#include "FilterPicker5.h"

//Only included if RTQUAKE
#ifdef RTQUAKE
#include "gdfontl.h"                             // graphics for web browser
#include "gdfontg.h"                             // graphics for web browser
#include "gdfonts.h"                             // graphics for web browser
#include "gd.h"                                  // graphics for web browser
#include <pthread.h>
#endif


#define MAXCHA 300                               // max number of components
#define MAXSMP 500000                            // max number of samples per component


//----------------------------------------------------------------------------------------------
// Default FilterPicker parameters
//----------------------------------------------------------------------------------------------
#define Picker_FW 300.0
#define Picker_TH1 10.0
#define Picker_TH2 10.0
#define Picker_LTW 500.0
#define Picker_TUP 20.0



enum ResultType resultType = PICKS;

//----------------------------------------------------------------------------
// default values
//----------------------------------------------------------------------------
int      prt             = 0;                    // debug printing
int      wavefiledef     = 0;                    // switch to indicate if wavefilename is given as input
int      iterations      = 200;                  // number of iterations removing phases giving bad residuals, input
int      locate          = 0;                    // 1 = autolocate, 0 = no location, input
int      automag         = 0;                    // 1 = automag, 0 = no automag
int      geolocation     = 0;                    // parameter to request reverse geolocation from Mapquest
int      geodetail       = 10;                   // parameter to set the geolocation detail
int      no_stations_trg = 5;                    // min number of stations with phase reading needed to locate
int      sphases         = 1;                    // switch phases: 0 = p phases only, 1 = p+s phases
int      keep            = 1;                    // parameter to save start of existing s-file, 0: make new
int      allpng          = 0;                    // draw wave(1) or not(0)
int      dbg             = 0;  
int      mail1           = 0;                    // 0-no mail, 1-mail
int      mail2           = 0;                    // 0-no mail, 1-mail
int      mail3           = 0;                    // 0-no mail, 1-mail1
int      mail4           = 0;                    // 0-no mail, 1-mail
int      mail5           = 0;                    // 0-no mail, 1-mail
float    maxres          = 3.0;                  // stop iterations when this value is reached, input
float    MINDIFF         = 1.5;                  // minimum number of seconds s-p
int      SETCODA         = 1;                    // compute coda
//----------------------------------------------------------------------------
// globals
//----------------------------------------------------------------------------
char     DistInd[100];                           // first line old s-file, used to decide distance indicator
char     sfilename[256];                         // s-filename, given as input, complete path
char     wfilename[256];                         // waveform filename
char     mailaddress1[256];                      // mailaddress 
char     mailaddress2[256];                      // mailaddress 
char     mailaddress3[256];                      // mailaddress 
char     mailaddress4[256];                      // mailaddress 
char     mailaddress5[256];                      // mailaddress
char     mail_message[1024];                     // mail content
char     mail_storage1[1000];
char     mail_storage2[1000];
char     mail_storage3[1000];
char     mail_storage4[1000];
char     mail_storage5[1000];

char     *topdir      = 0;                       // top directory SEISAN
char     *topdir_rt   = 0;                       // top directory RTQUAKE
char     *topdir_html = 0;                       // top directory HTML pages

char     COMP_HDR[MAXCHA][256];                  // component header including sr, ns, start time etc, read from miniseed file
char     PICKLINES[MAXCHA][1024];                // result lines from filterpicker
char     ALLPICKS[MAXCHA][256];                  // all picklines from FP
char     PICKTIMES[MAXCHA][500];                 // contains number of picks + picktimes extracted from PICKLINES
char     STATION_NAMES[MAXCHA][80];              // station name: XX_OSL___00_EHZ, where XX is missing network code
char     TIME_START[MAXCHA][80];                 // start time each component
char     trig_components[MAXCHA][256];           // station component code (HHZ) is extracted from STATION_NAMES
char     trig_stations[MAXCHA][256];             // station name code (KONO) is extracted from STATION_NAMES
char     TRIGGER_TIMES_P[MAXCHA][256];           // p-phase pick from filterpicker
char     TRIGGER_TIMES_S[MAXCHA][256];           // s-phase pick from filterpicker
char     SFCODALOC[200];                         // full path s-file
char     HYPFILE[MAXCHA][500];                   // temporary storage for lines from hyp.out
char     STASJONSCOMP[MAXCHA][20];               // Component names found in wavefile mseed
char     POLARIZATION[MAXCHA];                   // polarization
char     ALLCOMP[MAXCHA][20];                    // all station components in wavefile


int      verdier[MAXCHA][MAXSMP];                // samples all components read from miniseed file
int      srates[500];                            // samplerate each component, samples per second
int      nusmp[MAXCHA];                          // number of samples each component
float    vraw[MAXCHA][MAXSMP];                   // raw data components dc removed 
float    vflt[MAXCHA][MAXSMP];                   // filtered data components
float    fseismo[MAXSMP];                        // filtered signal
float    ratios[MAXSMP];                         // r=STA/LTA ratios of filtered comp. (fseismo)
float    ltas[MAXSMP];                           // LTA of filtered comp. (fseismo)
float    picks[2][MAXSMP];                       // picks returned from MAKEPICKS
float    maxsmA;
float    maxsmB;
float    smratios[MAXSMP];
float    pk[2][MAXSMP];
float    env[MAXSMP];
float    xtap[MAXSMP];
float    h[MAXSMP];
double   ppsstid;
char     trg_tim_p[MAXCHA][256];                 // p-phase pick from envelope
char     trg_tim_s[MAXCHA][256];                 // s-phase pick from envelope
double   p_tim[MAXCHA];                          // MSECS for pick from envelope
double   s_tim[MAXCHA];                          // MSECS for pick from envelope
int      varighet[MAXCHA];
int      pa;
int      allpick_ix = 0;                         // total number of picks from FP
int      klon;
int      sendmail=0;
int      no_lines;
int      debug=0;                                // debug switch
int      COMPWGAPS[MAXCHA];                      // components with gaps marked with 0
//----------------------------------------------------------------------------
// structures
//----------------------------------------------------------------------------

struct listnode {                                // structure mseed2askii
  char *key;
  char *data;
  struct listnode *next;
};

//----------------------------------------------------------------------------
// functions
//----------------------------------------------------------------------------
static int param_cmdlin(int argcount, char **argvec);
static void howto(void);
static void ZERO_START();
static void STALTAPHFL(int ne,int ws,int wl);
static void TIMSEC();
static int buroots();
static int phase_picks_encomp();
static int Check_SS();
static void PNG_PLOTS_NEW();
static int Check_S(float *resid,int *nst,float res_lim,int lp);
//static int S_REC2();
//static int Create_Sfile();
//static int PHASE2MSEC();
static int PHASE_INSIDE();
static int WHICH_S();
static void CHK_ALL_PICS();
static void SECTIM();
static void station_comp();
static void STARTMSECS();
static void CNVMSECS();
static int phase_picks_0();
static int phase_picks_1();
static int phase_picks_2();
static int DC();
static void process_component();
static void readS();
static void Read_Parameters();
static int read_trigger();
static int les_linje();
//---------------------------------------------
// mseed2askii
//---------------------------------------------
static int writeascii (MSTrace *mst,int comp_cnt);
static struct listnode *addnode (struct listnode **listroot, void *key, int keylen,
				 void *data, int datalen);

//---------------------------------------------
// mseed2askii
//---------------------------------------------
static int    verbose      = 0;      /* Verbosity level */
static int    reclen       = -1;     /* Record length, -1 = autodetected */
static int    indifile     = 1;      /* Individual file processing flag */
static char  *outputfile   = 0;      /* Output file name for single file output */
static FILE  *ofp          = 0;      /* Output file pointer */
static int    outformat    = 1;      /* Output file format */
static double timetol      = -1.0;   /* Time tolerance for continuous traces */
static double sampratetol  = -1.0;   /* Sample rate tolerance for continuous traces */

struct listnode *filelist = 0;                   // input file
//****************  FILTER  **********************************
/* Type-independant absolute value for C standard types */
#ifndef abs
#define ABS(x) ((x)<0?-(x):(x))
#endif
#define TRUE 1
static  int nsects;
static  float sn[5*3+1],sd[5*3+1];

float   snc[MAXCHA][16];                     // filter coefficients
float   sdc[MAXCHA][16];                     // filter coefficients
int     cset[MAXCHA];
int     cset_pick[MAXCHA];

static  float save_y2[MAXCHA][16];           // overlap between 2 filter buffers
static  float save_y1[MAXCHA][16];
static  float save_x2[MAXCHA][16];
static  float save_x1[MAXCHA][16];
static  float save_y2_pick[MAXCHA][16];      // overlap between 2 filter buffers
static  float save_y1_pick[MAXCHA][16];
static  float save_x2_pick[MAXCHA][16];
static  float save_x1_pick[MAXCHA][16];

float   fl;
float   fh;
float   eqavst;
int     orden;

int     srold=0;

typedef struct {float r, i;} complex;

/* Table of constant values */
static  float c_b12 = 2.;
static  complex c_b43 = {1.,0.};


/* Local function prototypes */
static  double warp();
static  int chebparm(), lptbp(), lptbr(), lpthp(), bilin2(), lp();
static  int c1roots(), c2roots();
static  int cutoffs(), beroots();
static  complex cmul(), cpowi(), csqrt1(), conjg(), cdiv();
static void design();
static void appl(int fnd,int nsamp);
static int les_linje2();
static void S_REC2();
static void Create_Sfile();
static int Update_map_file(char snam[],int ant,float resid,int nst,int klon,int subnet,char TRG_TID[],char MINTRGTID[]);
static int createHTML2(float ml,float mc,float mw,float latt,float lonn,char tid[],char where[],char TRG_TID[]);
static void PHASE2MSEC();

//****************  END FILTER  **********************************

void *print_message_function( void *ptr );

void *print_message_function( void *ptr )

{
  int ret;
  char *message;

  message = (char *) ptr;
  printf("%s \n", message);
  ret = system(message);
}
void S_REC2(year,month,day,hour,minute,seconds,dummy,fullpath,filename,nchannels,snam)
char year[];
char month[];
char day[];
char hour[];
char minute[];
char seconds[];
char dummy[];
char fullpath[];
char filename[];
int nchannels;
char snam[];
{
  FILE    *sf;
  FILE    *so;
  char buf[100];
  char fname[80];
  char def[80];
  char comp[20];
  char record[200];
  char sfilecopy[500][100];
  int sfix;
  int hr1;
  int mn1;
  int sc1;
  int ms1;
  char dm1[50];
  char dm2[50];
  int i;
  int ch; 
  int cnt;
  int br;
  int YR,MTH,DAY,HR,MIN;
  float fSEC;
  int AR;
  


  

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
  sscanf(seconds,"%f",&fSEC);
//printf("%s %f\n",seconds,SEC);

//  printf("AUTOPHASE: S_REC: fullpath..................: %s\n",fullpath);

   

  if(keep == 1)
  {

    sf=fopen(fullpath,"r");    
    if(sf == NULL)
    {
      printf("AUTOPHASE: S_REC: Can't open S-file: %s\n",fullpath);
      for(i=0;i<52;i++)
        printf("%2d %2x %c\n",i,fullpath[i],fullpath[i]);
      exit(0);
    }    
  }


  sfix=0;
  while ( fgets ( buf, sizeof buf, sf ) != NULL ) /* read a line from file */
  {
//       printf("%s", buf);
       sprintf(sfilecopy[sfix],"%s",buf);
       sfix++;
    if (buf[79] == '7')
    {
      break;

    }
  }


  for(ch=0;ch<nchannels;ch++)
  {
//printf("%s\n",trig_components[ch]);
    comp[0]=trig_components[ch][0];
    comp[1]=trig_components[ch][2];
    comp[2]='\0';

    cnt=0;
    for(i=0;i<6;i++)
    {
      if(trig_stations[ch][i] != '\0')
      {
        cnt++;
      }else{
        break;
      }
    }
//printf("%s  CNT: %d\n",trig_stations[ch],cnt);
//      printf("%3d TRIGGER_TIMES: %s %s\n",ch,TRIGGER_TIMES_P[ch],TRIGGER_TIMES_S[ch]);
    hr1=0;
    mn1=0;
    sc1=0;
    ms1=0;
    sscanf(TRIGGER_TIMES_P[ch],"%9c%2d",dm1,&hr1);
    sscanf(TRIGGER_TIMES_P[ch],"%12c%2d",dm1,&mn1);
    sscanf(TRIGGER_TIMES_P[ch],"%15c%2d",dm1,&sc1);
    sscanf(TRIGGER_TIMES_P[ch],"%18c%2d",dm1,&ms1);
    if(TRIGGER_TIMES_P[ch][3] != ' ')
    {

      switch(cnt)
      {
        case 2:
        if(POLARIZATION[ch] != ' ')
	{
	  if(varighet[ch] != 0)
	  {
            sprintf(sfilecopy[sfix]," %s   %s IP    A%c %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1,varighet[ch]);
	  }else{
            sprintf(sfilecopy[sfix]," %s   %s IP    A%c %2d%2d %2d.%2d     \n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1);    
	  }
	}else{
          if(varighet[ch] != 0)
	  {
            sprintf(sfilecopy[sfix]," %s   %s IP    A  %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1,varighet[ch]);
	  }else{
            sprintf(sfilecopy[sfix]," %s   %s IP    A  %2d%2d %2d.%2d     \n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);    
	  }
	}
        sfix++;	
	break;
        case 3:
        if(POLARIZATION[ch] != ' ')
	{
	  if(varighet[ch] != 0)
	  {
            sprintf(sfilecopy[sfix]," %s  %s IP    A%c %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1,varighet[ch]);
	  }else{
            sprintf(sfilecopy[sfix]," %s  %s IP    A%c %2d%2d %2d.%2d     \n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1);    
	  }
	}else{
	  if(varighet[ch] != 0)
	  {
            sprintf(sfilecopy[sfix]," %s  %s IP    A  %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1,varighet[ch]);  
	  }else{
            sprintf(sfilecopy[sfix]," %s  %s IP    A  %2d%2d %2d.%2d     \n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);    
	  }
	}
	sfix++;
        break;
        case 4:
        if(POLARIZATION[ch] != ' ')
	{
	  if(varighet[ch] != 0)
	  {
            sprintf(sfilecopy[sfix]," %s %s IP    A%c %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1,varighet[ch]);
	  }else{
            sprintf(sfilecopy[sfix]," %s %s IP    A%c %2d%2d %2d.%2d     \n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1);    
	  }
	}else{
	  if(varighet[ch] != 0)
	  {  
            sprintf(sfilecopy[sfix]," %s %s IP    A  %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1,varighet[ch]);
	  }else{
            sprintf(sfilecopy[sfix]," %s %s IP    A  %2d%2d %2d.%2d     \n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);    
	  }
	}
	sfix++;
        break;
        case 5:
        if(POLARIZATION[ch] != ' ')
	{
          if(varighet[ch] != 0)
	  {
            sprintf(sfilecopy[sfix]," %s%s IP    A%c %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1,varighet[ch]);
	  }else{
            sprintf(sfilecopy[sfix]," %s%s IP    A%c %2d%2d %2d.%2d     \n",trig_stations[ch],comp,POLARIZATION[ch],hr1,mn1,sc1,ms1);    
	  }
	}else{
	  if(varighet[ch] != 0)
	  {
            sprintf(sfilecopy[sfix]," %s%s IP    A  %2d%2d %2d.%2d %4d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1,varighet[ch]);  
	  }else{
            sprintf(sfilecopy[sfix]," %s%s IP    A  %2d%2d %2d.%2d     \n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);     
	  }
	}
	sfix++;
        break;
      }
    }
  }

  
if(sphases == 1)  
{  
  for(ch=0;ch<nchannels;ch++)
  {
    comp[0]=trig_components[ch][0];
    comp[1]=trig_components[ch][2];
    comp[2]='\0';

    cnt=0;
    for(i=0;i<6;i++)
    {
      if(trig_stations[ch][i] != '\0')
      {
        cnt++;
      }else{
        break;
      }
    }
    hr1=0;
    mn1=0;
    sc1=0;
    ms1=0;
//      printf("TRIGGER_TIMES_S: %s\n",TRIGGER_TIMES_S[ch]);
    sscanf(TRIGGER_TIMES_S[ch],"%9c%2d",dm1,&hr1);
    sscanf(TRIGGER_TIMES_S[ch],"%12c%2d",dm1,&mn1);
    sscanf(TRIGGER_TIMES_S[ch],"%15c%2d",dm1,&sc1);
    sscanf(TRIGGER_TIMES_S[ch],"%18c%2d",dm1,&ms1);
    if(TRIGGER_TIMES_S[ch][3] != ' ')
    {
//printf("%3d %3d %3d %3d %3d\n",ch,hr1,mn1,sc1,ms1);
      switch(cnt)
      {
        case 2:
//        fprintf(sf," %s   %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        sprintf(sfilecopy[sfix]," %s   %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
	sfix++;
//        printf(" %s   %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        break;
        case 3:
//        fprintf(sf," %s  %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        sprintf(sfilecopy[sfix]," %s  %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
	sfix++;
//        printf(" %s  %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        break;
        case 4:
//        fprintf(sf," %s %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        sprintf(sfilecopy[sfix]," %s %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
	sfix++;
//        printf(" %s %s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        break;
        case 5:
//        fprintf(sf," %s%s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        sprintf(sfilecopy[sfix]," %s%s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
	sfix++;
//        printf(" %s%s IS   3A  %2d%2d %2d.%2d\n",trig_stations[ch],comp,hr1,mn1,sc1,ms1);
        break;

        break;
      }
    }
  }
}
fclose(sf);
  
sf=fopen(fullpath,"w");
if(sf == NULL)
{
  printf("AUTOPHASE: S_REC: Can't open S-file: %s\n",fullpath);
  for(i=0;i<52;i++)
    printf("%2d %2x %c\n",i,fullpath[i],fullpath[i]);
  exit(0);
}      
  
  

for(i=0;i<sfix;i++)
{
  printf("%2d %s",i,sfilecopy[i]);
  fprintf(sf,"%s",sfilecopy[i]);
}
fclose(sf);
  
  
// exit(0); 
  
  
  for(ch=0;ch < nchannels;ch++)
  {
    for(i=0;i<256;i++)
    {
      TRIGGER_TIMES_P[ch][i]='\0';
      TRIGGER_TIMES_S[ch][i]='\0';
    }
  }
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
    if(linje[l] == 0xa && l == 0)
    {
      br=2;
      break;
    }
    if(linje[l] == 0xa && l > 0)
    {
      linje[l]='\0';
      break;        
    }
    if(linje[l] == EOF)
    {
//      printf("End of file. File contains: %2d items\n",l);

      br=1;
      return(br);
    }
  }
  return(br);
}


void MaxAmpFlt(int no_of_smp,int cmpkl,float *MxAmp)                    // Find max amplitude channel
{
  float max;
  int k;

  max=(float)0.0;
  for(k=0;k<no_of_smp;k++)
  {
    if(fabsf(vflt[cmpkl][k])>max)
      max=fabsf(vflt[cmpkl][k]);
  }
  *MxAmp=max;
}
void Scale_to_100_Raw(int no_of_smp,int cmpkl,float MxAmp)                 // Scale channel to max 1
{
  int k;

  for(k=0;k<no_of_smp;k++)
  {
    vraw[cmpkl][k]=(vraw[cmpkl][k]/MxAmp)*85.0;
  }
}
void Scale_to_100_Flt(int no_of_smp,int cmpkl,float MxAmp)                 // Scale channel to max 1
{
  int k;

  for(k=0;k<no_of_smp;k++)
  {
    vflt[cmpkl][k]=(vflt[cmpkl][k]/MxAmp)*85.0;
  }
}
void MaxAmpRaw(int no_of_smp,int cmpkl,float *MxAmp)                    // Find max amplitude channel
{
  float max;
  int k;

  max=(float)0.0;
  for(k=0;k<no_of_smp;k++)
  {
      if(fabsf(vraw[cmpkl][k])>max)
      max=fabsf(vraw[cmpkl][k]);
  }
  *MxAmp=max;
}
int Check_S(float *resid,int *nst,float res_lim,int lp)
{
  FILE *sf;
  FILE *out;
  int no_lines;
  int no,i,br,l,n,ret;
  int totlines;
  char linjer[200];
  char params1[MAXCHA][50];
  int index[200];
  char navn[25];
  char seq[2];
  char rs[10];
  char cmd1[200];
  int goon;
  int cr;
  int readings_left;
  float residual;
  int ant=0;
  int idx;
  int bytes_read;
  char dummy[50];
  char record[200];
  float res[200];
  float res_save[200];
  float max_res;
  
  for(i=0;i<200;i++)
  {
    res[i]=0.0; 
    index[i]=0;
  }

  sf=fopen("hyp.out","r");
  if(sf == NULL)
  {
    printf("AUTOPHASE :CHECK_S:Can't open S-file: hyp.out\n");
    exit(0);
  }else{


    no = 200;
    totlines=0;
    ant =0;
    for(i=0;i<no;i++)                      // find type 7 line before phase readings
    {
      br = les_linje(sf,record);
      if(record[78]=='Z' && record[79] == '7')
	break;
    }

    for(i=0;i<no;i++)
    {
      br = les_linje(sf,record);
      if(record[1] != ' ')
      {
        totlines++;
      }
      if(br == 1)
      {
	fclose(sf);
	break;
      }
      for(n=0;n<20;n++)
        navn[n]='\0';
      for(n=0;n<6;n++)
        navn[n]=record[n];
      sprintf(HYPFILE[i],"%s",record);
      if(record[65] != ' ')
      {
        sprintf(rs,"         ");
        sscanf(record,"%63c%5c",dummy,rs);
        sscanf(rs,"%f",&res[i]);
        ant++;
      }
    }
    fclose(sf);
    no_lines=ant;

    residual=0;
  
    for(l=0;l<totlines;l++)
      residual=residual+fabs(res[l]);
    if(ant > 0)
    {
      residual=residual/ant;
    }else{
      residual = 0.0;
    }
    printf("AUTOPHASE: readings left....................: %2d Avg.res: %8.3f\n",ant,residual);
    *resid = residual;
    *nst   = ant;

    if(residual <= res_lim)
    {            
      if(ant > 0)
      {
        *nst=ant;
        goon=0;  // Stop      
        sprintf(cmd1,"cp hyp.out hyp_new.out");
        ret = system(cmd1);
        return(goon);
      }
    }
        
    for(l=0;l<200;l++)
      index[l]=0;

    max_res = fabs(res[0]);
    idx=0;
    for(i=1;i<totlines;i++)
    {
      if(fabs(res[i]) > max_res)
      {
        max_res = fabs(res[i]);
        idx = i;
      }
    }
    res[idx]=0.0;
    index[idx]=1;

    sf=fopen("hyp.out","r");
    if(sf == NULL)
    {
      printf("AUTOPHASE: CHECK_S: Can't open hyp.out\n");
      exit(0);
    }

    out=fopen("hyp_new.out","w");
    if(out == NULL)
    {
      printf("AUTOPHASE: CHECK_S: Can't open hyp_new.out\n");
      exit(0);
    }else{
      for(i=0;i<no;i++)
      {
        br = les_linje(sf,record);
        fprintf(out,"%s\n",record);
        if(record[78]=='Z' && record[79] == '7')
	  break;
      }
      for(i=0;i<totlines;i++)
      {
	br = les_linje(sf,record);
	if(i != idx)
	{
	  fprintf(out,"%s\n",record);
	}
      }
      fclose(sf);
      fclose(out);
      ant=ant-1;
    }

    if(ant > 0)
    {
      *nst=ant;

      if( (residual) > res_lim)
      {
        goon=1;  // Continue
      }else{
        printf("AUTOPHASE: STOP iterations. Residual below..: %4.2f\n",res_lim);
        goon=0;  // Stop
      }
    }else{
      printf("\n");
      goon=0; 
    }
    return(goon);
  }
}




void TRIGGER_TIME_S(char timestamp_trg[],int ws,int ss,int kanal,float dt)
{
  int DDAY,MMTH;
  int DY;
  int YR;
  long   yrn;
  long   mon;
  long   day;
  long   hrn;
  long   min;
  long   doy;

  int   iyrn;
  int   imon;
  int   iday;
  int   ihrn;
  int   imin;

  float  sek;
  double MSECS;

  int seconds_cpu;
  char tiden_cpu[80];
  char millis_cpu[4];
  long day_cpu;
  long mon_cpu;
  long yrn_cpu;
  long hrn_cpu;
  long min_cpu;
  float sek_cpu;
  char dm[10];
  double MSECS_CPU;

  char   timestamp[80];
  char dum[50];
  int i,j;
  int klon=0;
  sscanf(timestamp_trg,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f6.3",&YR,dum,&imon,dum,&iday,dum,&ihrn,dum,  &imin,dum,&sek);
  hrn=ihrn;
  min=imin;
  mon=imon;
  day=iday;
  yrn=YR;
  yrn=yrn-2000;

  iday=day;
  imon=mon;
  iyrn=yrn;
  ihrn=hrn;
  imin=min;

  TIMSEC(yrn,mon,day,hrn,min,sek,&MSECS);
  MSECS=MSECS+(dt * ss)-(dt * ws);
  SECTIM(MSECS,&yrn,&doy,&mon,&day,&hrn,&min,&sek);

  iday=day;
  imon=mon;
  iyrn=yrn;
  ihrn=hrn;
  imin=min;
  iyrn=iyrn-2000;

  sprintf(timestamp,"%2d/%2d/%2d %2d:%2d:%6.3f",iday,imon,iyrn,ihrn,imin,sek);
  if(timestamp[0] == ' ') timestamp[0] = '0';
  if(timestamp[3] == ' ') timestamp[3] = '0';
  if(timestamp[6] == ' ') timestamp[6] = '0';
  if(timestamp[9] == ' ') timestamp[9] = '0';
  if(timestamp[12] == ' ') timestamp[12] = '0';
  if(timestamp[15] == ' ') timestamp[15] = '0';
  if(timestamp[18] == ' ') timestamp[18] = '0';
  if(timestamp[19] == ' ') timestamp[19] = '0';
  if(timestamp[20] == ' ') timestamp[20] = '0';

  sprintf(trg_tim_s[kanal],"%s",timestamp);  

  s_tim[kanal]=MSECS;                      // save trigger time

}
void TRIGGER_TIME_P(char timestamp_trg[],int ws,int pindex,int kanal,float dt)
{
  int DDAY,MMTH;
  int DY;
  int YR;
  long   yrn;
  long   mon;
  long   day;
  long   hrn;
  long   min;
  long   doy;

  int   iyrn;
  int   imon;
  int   iday;
  int   ihrn;
  int   imin;

  float  sek;
  double MSECS;

  int seconds_cpu;
  char tiden_cpu[80];
  char millis_cpu[4];
  long day_cpu;
  long mon_cpu;
  long yrn_cpu;
  long hrn_cpu;
  long min_cpu;
  float sek_cpu;
  char dm[10];
  double MSECS_CPU;

  char   timestamp[80];
  char dum[50];
  int i,j;
  int klon=0;

  sscanf(timestamp_trg,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f6.3",&YR,dum,&imon,dum,&iday,dum,&ihrn,dum,  &imin,dum,&sek);
  hrn=ihrn;
  min=imin;
  day=iday;
  mon=imon;
  yrn=YR;
  yrn=yrn-2000;

  iday=day;
  imon=mon;
  iyrn=yrn;
  ihrn=hrn;
  imin=min;

  TIMSEC(yrn,mon,day,hrn,min,sek,&MSECS);
  MSECS=MSECS+(dt * pindex)-(dt * ws);
  SECTIM(MSECS,&yrn,&doy,&mon,&day,&hrn,&min,&sek);

  iday=day;
  imon=mon;
  iyrn=yrn;
  ihrn=hrn;
  imin=min;
  iyrn=iyrn-2000;

  sprintf(timestamp,"%2d/%2d/%2d %2d:%2d:%6.3f",iday,imon,iyrn,ihrn,imin,sek);
  if(timestamp[0] == ' ') timestamp[0] = '0';
  if(timestamp[3] == ' ') timestamp[3] = '0';
  if(timestamp[6] == ' ') timestamp[6] = '0';
  if(timestamp[9] == ' ') timestamp[9] = '0';
  if(timestamp[12] == ' ') timestamp[12] = '0';
  if(timestamp[15] == ' ') timestamp[15] = '0';
  if(timestamp[18] == ' ') timestamp[18] = '0';
  if(timestamp[19] == ' ') timestamp[19] = '0';
  if(timestamp[20] == ' ') timestamp[20] = '0';

  sprintf(trg_tim_p[kanal],"%s",timestamp);  

  p_tim[kanal]=MSECS;                      // save trigger time

}


int CODA_N(int pindex,float ltas[],int nsamp)
{
  int i;
  int j;
  int cod;
  float lta_normal = 0.0;
  float lta_after = 0.0;
  for(i=(pindex-220);i<(pindex-20);i++)
    lta_normal = lta_normal + ltas[i];
  lta_normal = lta_normal/200;             // take average of 200 ltas BEFORE P

  for(j=0;j<(nsamp-200);j++)
  {
    lta_after=0.0;
    for(i=(pindex+j);i<(pindex+j+200);i++)
      lta_after = lta_after + ltas[i];
    lta_after = lta_after/200;
    if(lta_after <= (lta_normal*10.0))
    {
      cod=i;
      return(cod);
    }
  }
  return(0);
}




void main(int argc,char *argv[])
{
  FILE *sorg;                                    // handler to read original s-file  
//---------------------------------------------
// mseed2askii
//---------------------------------------------
  MSTraceGroup *mstg = 0;
  MSTrace *mst;
  MSRecord *msr = 0;
  
  struct listnode *flp; 
  int retcode;                                   // return code ms_readmsr
  int totalsamps = 0;                            // total number of samples msr->samplecnt
  int nsamples;                                  // return from writeascii
  
  
  char   cmd01[256];                             // string for system commands 
  char   timecheck[100];                         // timestring to be checked
  char   dbname[256];                            // database name, extracted from s-filename path  
  char   MINTRGTID[80];                          // trg time from s-file format: 03/04/2015 18:20:16.2
  char   TRG_TID[80];                            // trg time from s-file format: 2015-04-03-18:20:16.2 
  char   fnam[256];                              // filename extracted from line 6 s-file
  char   yr_trg[20];                             // year from s-file
  char   mo_trg[20];                             // month from s-file
  char   da_trg[200];
  char   hr_trg[200];
  char   mi_trg[200];
  char   se_trg[200];  
  char   sfilepath[300];  
  char   mvcmd[500];
  char   ddum[100];
  int    tel;
  int    len;
  int    ret;                                    // returns from system command
  int    subnet;                                 // extracted from s-file wavefile name  
  int    comp_cnt=0;                             // number of components in wavefile
  int    nchannels;                              // number of channels returned from read_trigger
  double win1;
  double win2;
  int    i,j;
  char   FIRSTCOMP[30];
  int    lik = 1;
  int    ii = 0;
  int    iist = 0;
  int    no =0; 
  int    kanaler[MAXCHA]; 
  char   longname1[20];
  char   longname2[20];
  char   navn1[10];
  char   navn2[10];
  char   komp1[10];
  char   komp2[10];
  char   stime1[10];
  char   stime2[10];
  char   date1[20];
  char   date2[20];
  char   tid1[20];
  char   tid2[20];
  char   tchk1[50];
  char   tchk2[50];
  int    sx1;
  int    sx2;
  char   NEXTCOMP[20];
  int    sx; 
#ifdef RTQUAKE
  pthread_t thread1;  
#endif
  int  iret1;    
  
//-------------------------------------------------------------------------
// Process command line parameters
//-------------------------------------------------------------------------
  if(param_cmdlin(argc, argv) < 0)
  {
    printf("Error command line parameters\n\n");
    printf("Type '-h' for options\n");
    exit(0);
  }
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
#ifdef RTQUAKE  
//------------------------------------------------------------------------
// set path to RTQUAKE_TOP as default
//------------------------------------------------------------------------
  topdir_rt = (char*)getenv("RTQUAKE_TOP");
  if (topdir_rt)
  {
    printf("AUTOPHASE: RTQUAKE_TOP......................: %s\n", topdir_rt);
  }else{
    printf("AUTOPHASE: RTQUAKE_TOP not defined\n");
    printf("Include setup_rt.bash or setup_rt.csh in your .bashrc or .csh\n");
    exit(0);      
  }
//------------------------------------------------------------------------
// set path to HTML_TOP as default
//------------------------------------------------------------------------
  topdir_html = (char*)getenv("HTML_TOP");
  if (topdir_html)
  {
    printf("AUTOPHASE: HTML_TOP.........................: %s\n", topdir_html);
  }else{
    printf("AUTOPHASE: HTML_TOP not defined\n");
    printf("Include setup_rt.bash or setup_rt.csh in your .bashrc or .csh\n");
    exit(0);      
  }  
//------------------------------------------------------------------------
// read rtquake.par if exist, otherwise use defaults
//------------------------------------------------------------------------
  Read_Parameters();
#endif  
  sprintf(SFCODALOC,"%s",sfilename);
//-------------------------------------------------------------------
// sfilename given as input. Complete path and filename
//--------------------------------------------------------------------
  sprintf(sfilepath,"%s",sfilename);
  if(prt >= 1)  
    printf("rtpick.........................: %s\n",sfilepath);  
//-------------------------------------------------------------------
// save original s-file to current directory as s_original.sav
//--------------------------------------------------------------------
#ifdef RTQUAKE  
  sprintf(cmd01,"cp %s s_orginal.sav",sfilename);                  // save original s-file to current directory
  if(prt >= 1)
    printf("Save original s-file to s_original.sav to current directory\n");
  ret=system(cmd01);
#endif
//#ifdef LINUX  
#ifdef __linux__
  sprintf(cmd01,"cp %s s_orginal.sav",sfilename);                  // save original s-file to current directory
  ret=system(cmd01);
  #endif
//#ifdef WIN32  
#ifdef __MINGW32__
  sprintf(cmd01,"copy %s s_orginal.sav",sfilename);                  // save original s-file to current directory
  ret=system(cmd01);
#endif
  if(prt >= 1)
    printf("Save original s-file to s_original.sav to current directory\n");
//-------------------------------------------------------------------
// read copy of original s-file to find L(ocal), R(egional) or D(istant)
//--------------------------------------------------------------------  
  if((sorg = fopen("s_orginal.sav","r")) == NULL)    
  {
    printf("Can't read s_orginal.sav.\n");
    exit(0);
  }
  ret = les_linje(sorg,DistInd);
  fclose(sorg);
//-------------------------------------------------------------------
// extract database name from s-filepath
//-------------------------------------------------------------------
  for(i=0;i<256;i++)
  {
    if(sfilename[i] == 'R' && sfilename[i+1] == 'E' & sfilename[i+2] == 'A')
    {
      dbname[0] = sfilename[i+4];
      dbname[1] = sfilename[i+5];
      dbname[2] = sfilename[i+6];
      dbname[3] = sfilename[i+7];
      dbname[4] = sfilename[i+8];
      dbname[5] = '\0';
    }
  }
  if(prt >= 1)
    printf("AUTOPHASE main: dbname......................: %s\n",dbname);
//-------------------------------------------------------------------
// open s-file and read the '1' line to get the time for trigger
// continue to read line '6' to get the wavefile name
//-------------------------------------------------------------------
  readS(sfilename,MINTRGTID,TRG_TID,fnam,&subnet,yr_trg,mo_trg);
//  printf("TRG_TID: %s\n",TRG_TID);
  if(prt >= 1)
    printf("AUTOPHASE main: subnet......................: %d\n",subnet);
//---------------------------------------------------------------------------
// wavefile is in miniseed format and is converted to askii for reading
// depending on the wavefile switch (see documentation rtquake.par) the
// file is added to the file list for conversion to askii
//---------------------------------------------------------------------------  
  if(wavefiledef == 1)
  {
    // Add the file name to the intput file list
    printf("wfilename: %s\n",wfilename);
    if ( ! addnode (&filelist, NULL, 0, wfilename, strlen(wfilename)+1) )
    {
      printf ("Error adding file name: %s\n",wfilename);
      exit(0);
    }
  }else{
    sprintf(cmd01,"%s/WAV/%s/%s/%s/%s",topdir,dbname,yr_trg,mo_trg,fnam);
    printf("CMD01: %s\n",cmd01);
    // Add the file name to the intput file list */
    if ( ! addnode (&filelist, NULL, 0, cmd01, strlen(cmd01)+1) )
    {
      printf ("Error adding file name: %s\n",cmd01);
      exit(0);
    }
  }

  if(prt >= 1)
    printf(" CH      STATION                 TIMESTAMP   SR     DT  NSAMP PINDEX     PP    PN    SS    SN  CODA   DIFF-P   DIFF-S\n");
//---------------------------------------------------------------------------------------
// Read wavefile that was converted from miniseed to askii: trigger.ask
// Each component is read and processed for phases
//---------------------------------------------------------------------------------------
  comp_cnt=0;
  // Init MSTraceGroup
  mstg = mst_initgroup (mstg);
  
  // Read input miniSEED files into MSTraceGroup
  flp = filelist;

  if ( verbose )
    printf ("Reading %s\n", flp->data);
      
  while ( (retcode = ms_readmsr(&msr, flp->data, reclen, NULL, NULL,1, 1, verbose-1)) == MS_NOERROR )
  {
    if ( verbose > 1)
      msr_print (msr, verbose - 2);
  
    mst_addmsrtogroup (mstg, msr, 1, timetol, sampratetol);

    totalsamps += msr->samplecnt;
  }
      
  if ( retcode != MS_ENDOFFILE )
    printf("Error reading %s: %s\n", flp->data, ms_errorstr(retcode));
      
  // Make sure everything is cleaned up
  ms_readmsr (&msr, NULL, 0, NULL, NULL, 0, 0, 0);
      
  // If processing each file individually, write ASCII and reset
  if ( indifile )
  {
    mst = mstg->traces;
    while ( mst )
    {
//      printf("comp_cnt: %2d\n",comp_cnt);
      nsamples = writeascii (mst,comp_cnt);
      if(nsamples != 0)
        comp_cnt++;
      mst = mst->next;
    }
    mstg = mst_initgroup (mstg);
  }
      
  
  // Make sure everything is cleaned up
  mst_freegroup (&mstg);
  
  if ( ofp )
    fclose (ofp);
  if(prt >= 1)
    printf("Components found reading miniseed file. COMP_CNT: %d\n",comp_cnt);
  for(i=0;i<comp_cnt;i++)
    COMPWGAPS[i]=1;
  for(i=0;i<comp_cnt;i++)
  {
    tel=0;

    for(j=i;j<comp_cnt;j++)
    {
      ret=strcmp(STASJONSCOMP[i],STASJONSCOMP[j]) ;
      if(ret == 0)
      {
	tel++;
	if(tel > 1)
	{
	  printf("GAP in %s index %d\n",STASJONSCOMP[j],j);
	  COMPWGAPS[j] = 0; 
	}
      }
    }
  }

//----------------------------------------------------------------
// read_trigger
//
// Read wavefile converted from miniseed
// construct unique station name based on network name,station,location,component
// call routine to process each component for phases
//----------------------------------------------------------------


  nchannels=read_trigger(comp_cnt,1);
//printf("here 1  iist: %d  nchannels: %d\n",iist,nchannels);                        //debtu 
//  for(i=0;i<nchannels;i++)
//    printf("%2d %s\n",i,ALLCOMP[i]);
  no=nchannels;

  for(;;)
  {
//printf("here 1\n");                        //debtu 
    ii = iist; 
    lik=1;
    sprintf(FIRSTCOMP,"%s",ALLCOMP[ii]);
    for(i=ii+1;i<nchannels;i++)
    {
      ret = strncmp(FIRSTCOMP,ALLCOMP[i],14);
      if(ret == 0)
      {
//      printf("%s  %s\n",FIRSTCOMP,ALLCOMP[i]);
        lik++;
      }
    }
//  printf("%d\n",lik);
    for(i=0;i<lik;i++)
    {
      ALLCOMP[ii][15] = ' ';
      switch(lik)
      {
        case 1:
        ALLCOMP[ii][16] = '1';
        break;
        case 2:
        ALLCOMP[ii][16] = '2';
        break;
        case 3:
        ALLCOMP[ii][16] = '3';
        break;
      }
      ALLCOMP[ii][17] = '\0';
      ii++;
    }
    iist=iist+lik;
    no=no-lik;
    if(no <=0)
      break;
  } 

 
//  if(prt >= 1)
    printf("read_trigger main: nchannels.............: %d\n",nchannels);
//printf("call read 2    comp_cnt: %d\n",comp_cnt);
  nchannels=read_trigger(comp_cnt,2);
//printf("nchannels: %d\n",nchannels);


  printf("-----------------------------------------------------------------------\n");
  printf("SELECTED PICKS:\n");
  printf("-----------------------------------------------------------------------\n");


  tel=0;
//  char c;                             // debtu
  for(i=0;i<nchannels;i++)
  {



    len = strlen(trig_stations[i]);
    
    if(TRIGGER_TIMES_P[i][0] != ' ' || TRIGGER_TIMES_S[i][0] != ' ')
    {
      switch(len)
      {
        case 2:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s    %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
          tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ')
        {
          printf("%3d %s    %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s    %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        break;
        case 3:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s   %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ')
        {
          printf("%3d %s   %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s   %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        break;
        case 4:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s  %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ')
        {
          printf("%3d %s  %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ') 
        {
          printf("%3d %s  %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);      
          kanaler[tel]=i;
	  tel++;
        }
        break;
        case 5:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]); 
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ') 
        {
          printf("%3d %s %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ') 
        {
          printf("%3d %s %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        break;
      }
    }
  }

//printf("tel: %d\n",tel);              // debtu
tel=tel-1;
for(i=0;i<tel;i++)
{
//  c=getchar();                       // debtu
  sx1=kanaler[i];
  sx2=kanaler[i+1];
//  printf("%2d  %2d  %c  %c %s %s\n", sx1,sx2,TRIGGER_TIMES_S[sx1][0], TRIGGER_TIMES_S[sx2][0],TRIGGER_TIMES_S[sx1], TRIGGER_TIMES_S[sx2]);  // debtu
  if(TRIGGER_TIMES_S[sx1][0] != ' ' && TRIGGER_TIMES_S[sx2][0] != ' ')
  {
//printf("ALL %s %s %s %s\n",stime1,stime2,komp1,komp2);    
    len = strlen(trig_stations[sx1]);
    switch(len)
    {
      case 2:
      sprintf(longname1,"%s____%s",trig_stations[sx1],trig_components[sx1]);
      break;
      case 3:
      sprintf(longname1,"%s___%s",trig_stations[sx1],trig_components[sx1]);
      break;
      case 4:
      sprintf(longname1,"%s__%s",trig_stations[sx1],trig_components[sx1]);
      break;
      case 5:
      sprintf(longname1,"%s_%s",trig_stations[sx1],trig_components[sx1]);
      break;
    } 

    len = strlen(trig_stations[sx2]);
    switch(len)
    {
      case 2:
      sprintf(longname2,"%s____%s",trig_stations[sx2],trig_components[sx2]);
      break;
      case 3:
      sprintf(longname2,"%s___%s",trig_stations[sx2],trig_components[sx2]);
      break;
      case 4:
      sprintf(longname2,"%s__%s",trig_stations[sx2],trig_components[sx2]);
      break;
      case 5:
      sprintf(longname2,"%s_%s",trig_stations[sx2],trig_components[sx2]);
      break;
    }
    ret=strcmp(trig_stations[sx1],trig_stations[sx2]);
    if(ret==0)
    {
      if(trig_components[sx1][2] != 'Z' && trig_components[sx2][2] != 'Z')
      {
        sscanf(TRIGGER_TIMES_S[sx1],"%21c",tchk1);
        sscanf(TRIGGER_TIMES_S[sx2],"%21c",tchk2);
        ret=WHICH_S(sx1,sx2,tchk1,tchk2);
//        printf("%3d %s  %s  %s %s remove: %3d\n",i,longname1,longname2,TRIGGER_TIMES_S[sx1],TRIGGER_TIMES_S[sx2],ret);
        sprintf(TRIGGER_TIMES_S[ret],"                         ");
      }
    }       
  }
}
  printf("----------------------------------------------------------------------------------------\n");

  ii = 0;
  iist = 0;
  no =0;
  
  no=nchannels;

  for(;;)
  {
    ii = iist; 
    for(sx=0;sx<8;sx++)
      FIRSTCOMP[sx]=ALLCOMP[ii][sx];
    FIRSTCOMP[sx]='\0';
//  sscanf(ALLCOMP[ii],"%8c",FIRSTCOMP);  
    for(i=ii+1;i<nchannels;i++)
    {
      sscanf(ALLCOMP[i],"%8c",NEXTCOMP);
//    printf("%s %s\n",FIRSTCOMP,NEXTCOMP);
      ret = strcmp(FIRSTCOMP,NEXTCOMP);
      if(ret == 0)
      {
        if(ALLCOMP[ii][14] == ALLCOMP[i][14])
        {
//        printf("remove %d\n",i);
          sprintf(TRIGGER_TIMES_P[i],"                         ");
          sprintf(TRIGGER_TIMES_S[i],"                         ");	
        }
      }
    }
    iist=iist+1;
    no=no-1;
    if(no <=0)
      break;
  } 

//for(i=0;i<nchannels;i++)
//  printf("%s  %s  %s\n",ALLCOMP[i],TRIGGER_TIMES_P[i],TRIGGER_TIMES_S[i]);

  for(i=0;i<nchannels;i++)
  {
    len = strlen(trig_stations[i]);
    
    if(TRIGGER_TIMES_P[i][0] != ' ' || TRIGGER_TIMES_S[i][0] != ' ')
    {
      switch(len)
      {
        case 2:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s    %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
          tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ')
        {
          printf("%3d %s    %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s    %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        break;
        case 3:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s   %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ')
        {
          printf("%3d %s   %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s   %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        break;
        case 4:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s  %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ')
        {
          printf("%3d %s  %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ') 
        {
          printf("%3d %s  %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);      
          kanaler[tel]=i;
	  tel++;
        }
        break;
        case 5:
        if(TRIGGER_TIMES_P[i][0] == ' ' && TRIGGER_TIMES_S[i][0] != ' ')
        {
          printf("%3d %s %s   %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]); 
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] == ' ') 
        {
          printf("%3d %s %s   P %s %c   %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        if(TRIGGER_TIMES_P[i][0] != ' ' && TRIGGER_TIMES_S[i][0] != ' ') 
        {
          printf("%3d %s %s   P %s %c S %s\n",i,trig_stations[i],trig_components[i],TRIGGER_TIMES_P[i],POLARIZATION[i],TRIGGER_TIMES_S[i]);
          kanaler[tel]=i;
	  tel++;
        }
        break;
      }
    }
  }

//  for(i=0;i<comp_cnt;i++)
//    printf("%2d %s   %s\n",i,TRIGGER_TIMES_P[i],TRIGGER_TIMES_S[i]);
/*  
  CHK_ALL_PICS(&win1,&win2);
  for(i=0;i<comp_cnt;i++)
  {
    sscanf(TRIGGER_TIMES_P[i],"%21c",timecheck);
    timecheck[21]='\0';
    if(timecheck[2] != ' ')
      PHASE_INSIDE(win1,win2,timecheck);
  }
*/ 
  Create_Sfile(fnam,sfilepath,dbname,nchannels,klon,locate,yr_trg,mo_trg,da_trg,hr_trg,mi_trg,se_trg,sfilename,subnet,TRG_TID,MINTRGTID);
#ifdef RTQUAKE
  sprintf(cmd01,"rtpurge");
  ret = system(cmd01); 
#endif  
  
    
#ifdef RTQUAKE
/*------------------------------------------------------------------------*/
/*                Create png plots for web-page                           */
/*------------------------------------------------------------------------*/
  if(allpng == 1)
  {
    printf("-----------------------------------------------------------------------\n");
    printf("Create plots for web-page.\n");
    printf("-----------------------------------------------------------------------\n");
    PNG_PLOTS_NEW(nchannels);
  }
  
/*------------------------------------------------------------------------*/
/*                Send mail to specified users                            */
/*------------------------------------------------------------------------*/  
  if(sendmail >=0)
  {
    printf("-----------------------------------------------------------------------\n");
    printf("Send mails to specified addresses.\n");
    printf("-----------------------------------------------------------------------\n");
    if(mail1>=1)
    {
      printf("Sent mail to recipient 1\n");
      printf("%s\n",mail_storage1);
      const char *message1=mail_storage1;
      iret1 = pthread_create( &thread1, NULL, print_message_function, (void*) message1);
      if(iret1)
      {
        fprintf(stderr,"Error - pthread_create() return code: %d\n",iret1);
        exit(EXIT_FAILURE);
      }    
    }
    if(mail2>=1)
    {
      printf("Sent mail to recipient 2\n");
      printf("%s\n",mail_storage2);    
      const char *message1=mail_storage2;
      iret1 = pthread_create( &thread1, NULL, print_message_function, (void*) message1);
      if(iret1)
      {
        fprintf(stderr,"Error - pthread_create() return code: %d\n",iret1);
        exit(EXIT_FAILURE);
      }    
    }
    if(mail3>=1)
    {
      printf("Sent mail to recipient 3\n");
//      printf("%s\n",mail_storage3);    
      const char *message1=mail_storage3;
      iret1 = pthread_create( &thread1, NULL, print_message_function, (void*) message1);
      if(iret1)
      {
        fprintf(stderr,"Error - pthread_create() return code: %d\n",iret1);
        exit(EXIT_FAILURE);
      }    
    }
    if(mail4>=1)
    {
      printf("Sent mail to recipient 4\n");
//      printf("%s\n",mail_storage4);    
      const char *message1=mail_storage4;
      iret1 = pthread_create( &thread1, NULL, print_message_function, (void*) message1);
      if(iret1)
      {
        fprintf(stderr,"Error - pthread_create() return code: %d\n",iret1);
        exit(EXIT_FAILURE);
      }    
    }
    if(mail5>=1)
    {
      printf("Sent mail to recipient 5\n");
//      printf("%s\n",mail_storage5);    
      const char *message1=mail_storage5;
      iret1 = pthread_create( &thread1, NULL, print_message_function, (void*) message1);
      if(iret1)
      {
        fprintf(stderr,"Error - pthread_create() return code: %d\n",iret1);
        exit(EXIT_FAILURE);
      }    
    }  
  }
#endif    

#ifdef RTQUAKE
  sprintf(cmd01,"rm seed.out");
  ret = system(cmd01);
#endif

#ifdef __linux__
  sprintf(cmd01,"rm seed.out");
  ret = system(cmd01);
#endif

#ifdef __MINGW32__
  sprintf(cmd01,"del seed.out");
  ret = system(cmd01);
#endif
  
}


#ifdef RTQUAKE

void PNG_PLOTS_NEW(int nchannels)
{
  FILE   *out;
  int    i;
  int    ix;
  int    kar;
  int    kar2;
  int    displ;
  int    ret2;
  int    kan;
  int    kl;
  int    nemin;
  int    nl;
  int    koda;
  int    rate;
  int    yincr;  
  int    avstand;
  int    bg,sg1,sg2,sg3,sg4,tl,tt,tk,t5,dg;
  int    ix_old,ix_new,iy_old,iy_new;
  int    cmpix;
  int    cmpix2;
  int    ret;
  int    stasjon;
  int    ncomp[MAXCHA];
  int    xsiz,ysiz;


  int    YR;
  int    DY;
  int    DDAY;
  int    MMTH;
  int   iyrn;
  int   imon;
  int   iday;
  int   ihrn;
  int   imin;
  int   mark10;
  
  double PPH[MAXCHA];
  double SPH[MAXCHA];
  double COD[MAXCHA];  
  double MSECS;
  double MINI;
  double MSECS_START[MAXCHA];  
  
  char   last_station[200];
  char   fil[200];
  char   fil2[200];
  char   timestring[256];
  char   dum[256];
  char   NAME_ONLY[MAXCHA][100];
  char   COMP_NAMES[MAXCHA][100];
  char   ph[100];
  char   navnihyp[200];
  char   comp[100];
  char   sec10[100];
  char   pngfile[200];
  char  nam[256];
  
  long   yrn;
  long   mon;
  long   day;
  long   hrn;
  long   min;
  long   doy;

  float  sek;
  float  diff;
  float  idx;
  float  incr;
  float  MAXAMP_RAW;
  float  MAXAMP_FLT;

  
  for(i=0;i<nchannels;i++)                       // initialize p-phase and s-phase table
  {
    PPH[i]=-1.0;
    SPH[i]=-1.0;
  }
  gdImagePtr im;                                 // Declare the image 

  kar=0;
  yincr=86;                                      // Set distance increment in pixels between traces
  avstand=yincr;

  for(i=0;i<200;i++)                             // initialize last station name
    last_station[i]='\0';  

//---------------------------------------
// FIND NUMBER OF COMPONENTS EACH STATION
//---------------------------------------
  stasjon=0;

  for(cmpix = 0; cmpix < nchannels; cmpix++)
  {
    for(i=0;i<20;i++)
      fil[i]='\0';
    for(i=0;i<5;i++)
    {
      if(STATION_NAMES[cmpix][i+3] != '_')
      {
        fil[i]=STATION_NAMES[cmpix][i+3];
        kar++;
      }
    }
    for(cmpix2 = 0; cmpix2 < nchannels; cmpix2++)
    {
      for(i=0;i<20;i++)
        fil2[i]='\0';
      for(i=0;i<5;i++)
      {
        if(STATION_NAMES[cmpix2][i+3] != '_')
        {
          fil2[i]=STATION_NAMES[cmpix2][i+3];
          kar2++;
        }
      }
      ret=strncmp(fil,fil2,kar);                 // compare current station with previous station
      if(ret == 0)                               // New station ?
        stasjon++;
    }
    ncomp[cmpix]=stasjon;
    stasjon=0;
    kar=0;
    kar2=0;
  }
//for(i=0;i<nchannels;i++)
//  printf("%2d %2d\n",i,ncomp[i]);
//------------------------------------------
// convert all start times to seconds
//------------------------------------------
  for(cmpix = 0; cmpix < nchannels; cmpix++)
  {
    sprintf(timestring,"%s",TIME_START[cmpix]);

    sscanf(timestring,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f6.3",&YR,dum,&imon,dum,&iday,dum,&ihrn,dum,  &imin,dum,&sek);
    hrn=ihrn;
    min=imin;
    mon=imon;
    day=iday;
    yrn=YR;
    yrn=yrn-2000;
    iday=day;
    imon=mon;
    iyrn=yrn;
    ihrn=hrn;
    imin=min;
    TIMSEC(yrn,mon,day,hrn,min,sek,&MSECS);
    MSECS_START[cmpix]=MSECS;
  }

//---------------------------------------
// component name all channels
//---------------------------------------
  for(cmpix = 0; cmpix < nchannels; cmpix++)
  {
    dum[0]=STATION_NAMES[cmpix][12];
    dum[1]=STATION_NAMES[cmpix][14];
    dum[2]='\0';
    sprintf(COMP_NAMES[cmpix],"%s",dum);
  }
  sprintf(dum,"hyp.out"); 

//--------------------------------------
// read s-file
//--------------------------------------
  if(prt >= 4)
    printf("CALLING CHECK_SS %s\n",dum);
  ret=Check_SS(dum);
  if(prt >= 4)
    printf("RET: %d\n",ret);

//---------------------------------------
// extract the station name only  
//---------------------------------------
  for(cmpix = 0; cmpix < nchannels; cmpix++)
  {
    for(i=0;i<20;i++)
      fil[i]='\0';
    for(i=0;i<5;i++)
    {
      if(STATION_NAMES[cmpix][i+3] != '_')
      {
        fil[i]=STATION_NAMES[cmpix][i+3];
      }
    }
    sprintf(NAME_ONLY[cmpix],"%s",fil);
  } 
  
//-------------------------------------------------------------------
// insert all readings of phases from s-file into corresponding
// place for each station component. (for plotting of phases)
//-----------------------------------------------------------------
  for(nl = 0; nl < no_lines; nl++)
  {
    sprintf(timestring,"%s",HYPFILE[nl]);
    for(i=0;i<20;i++)
      navnihyp[i]='\0';
    strncpy(navnihyp,timestring,6);

    for(i=0;i<5;i++)
      navnihyp[i]=navnihyp[i+1];
    navnihyp[5]='\0';

    for(i=0;i<5;i++)
    {
      if(navnihyp[i]==' ')
        navnihyp[i]='\0';
    }
    sscanf(timestring,"%6c%s",dum,comp);
    sscanf(timestring,"%8c%s",dum,ph);
    dum[0]=timestring[18];
    dum[1]=timestring[19];
    dum[2]='\0';
    sscanf(dum,"%d",&ihrn);
    dum[0]=timestring[20];
    dum[1]=timestring[21];
    dum[2]='\0';
    sscanf(dum,"%d",&imin);
    sscanf(timestring,"%23c%f5.2",dum,&sek);
    koda=0;
    if(timestring[32] != ' ')
      sscanf(timestring,"%28c%d",dum,&koda);

    hrn=ihrn;
    min=imin;
    TIMSEC(yrn,mon,day,hrn,min,sek,&MSECS);
      
    for(cmpix = 0; cmpix < nchannels; cmpix++)
    {

      ret = strcmp(navnihyp,NAME_ONLY[cmpix]);
      ret2= strcmp(comp,COMP_NAMES[cmpix]);
      
      if(ret == 0 && ret2 == 0)       // found station and component ?
      {
	if(ph[1]=='P')
	{
	  PPH[cmpix]=MSECS;
	}
	if(ph[1]=='S')
	{
	  SPH[cmpix]=MSECS;
	}
	if(koda!=0)
	{
	  COD[cmpix]=MSECS+koda;
	}
      }
    }
  }        
//for(i=0;i<nchannels;i++)
//  printf("%2d %s %2d %8.2f %8.2f %8.2f %8.2f\n",i,STATION_NAMES[i],ncomp[i],MSECS_START[i],PPH[i],SPH[i],COD[i]);
//------------------------------------  
// GENERATE PLOT EACH STATION
//------------------------------------
  for(cmpix = 0; cmpix < nchannels; cmpix++)
  {
    for(i=0;i<20;i++)
      fil[i]='\0';
    if(STATION_NAMES[cmpix][0] != '_')
    {
      for(i=0;i<5;i++)
      {
        if(STATION_NAMES[cmpix][i+3] != '_')
        {
          fil[i]=STATION_NAMES[cmpix][i+3];
          kar++;
        }
      }
    }else{
      for(i=0;i<5;i++)
      {
        if(STATION_NAMES[cmpix][i+1] != '_')
        {
          fil[i]=STATION_NAMES[cmpix][i+1];
          kar++;
        }
      }	  
    }

    ret=strncmp(fil,last_station,kar);                       // compare current station with previous station
    if(ret != 0)                                             // New station ?
    {
      if(prt >= 1)
        printf("New station single station %s\n",fil);
      sprintf(pngfile,"%s/loc/%s.png",topdir_html,fil);   // YES, file name format: BER.png or PB10.png
      sprintf(last_station,"%s",fil);
    
      xsiz = 845;
      ysiz = 345; 
    
      yincr=86;                                                // Set distance increment in pixels between traces
      avstand=yincr;
        
      im  = gdImageCreate(xsiz, ysiz);                        // Allocate image: tot_wd pixels width by tot_hg pixels high

      bg  = gdImageColorAllocate(im, 255, 255, 255); // WHITE first color definition defines the background color
      tl  = gdImageColorAllocate(im, 200, 200, 200); // GRAY  color for timelines
      tk  = gdImageColorAllocate(im,   0,   0,   0); // BLACK color for text
      tt  = gdImageColorAllocate(im,   0,   0, 255); // BLUE color for title
      t5  = gdImageColorAllocate(im,   0,   0,   0); // BLACK color for 5 minute marks
      sg1 = gdImageColorAllocate(im, 255,   0,   0); // RED   color seismogram
      sg2 = gdImageColorAllocate(im,   0, 255,   0); // GREEN color seismogram
      sg3 = gdImageColorAllocate(im,   0,   0, 255); // BLUE color seismogram
      sg4 = gdImageColorAllocate(im,   0,   0,   0); // BLACK color seismogram     

      ix_old=idx;
      iy_old=avstand;
      gdImageLine(im, 0, 0, xsiz-1, 0, sg4);                     // draw frame  
      gdImageLine(im, xsiz-1, 0, xsiz-1, ysiz-1, sg4);                 // draw frame
      gdImageLine(im, xsiz-1, ysiz-1, 0, ysiz-1, sg4);                 // draw frame
      gdImageLine(im, 0, ysiz-1, 0, 0, sg4);                     // draw frame
//------------------------------------------------- 
// find minimum start time in seconds this station       
//-------------------------------------------------
      MINI=MSECS_START[cmpix];
      kan=cmpix;

      for(kl=0;kl<ncomp[cmpix];kl++)
      {      
        if(MSECS_START[cmpix+kl] <= MINI)
        {
	  MINI=MSECS_START[cmpix+kl];
	  kan=cmpix+kl;
	  nemin=nusmp[cmpix+kl];
	  rate=srates[cmpix+kl];
        }
      }

//--------------------------------------------------
// draw station name and start time
//--------------------------------------------------- 
      gdImageString(im,gdFontGetLarge(),10,15,fil,sg3);        
      gdImageString(im,gdFontGetLarge(),80,15,TIME_START[kan],tk);      
      sprintf(timestring,"%s",TIME_START[kan]);
///    printf("TIME_START: %s rate: %3d nemin: %4d %s\n",timestring,rate,nemin,fil);
      sscanf(timestring,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f6.3",&YR,dum,&imon,dum,&iday,dum,&ihrn,dum,&imin,dum,&sek);
//    printf("sek: %5.2f",sek);
      for(kl=0;kl<7;kl++)
      {
        if((kl * 10) > sek)
        {
	  diff = (kl * 10) - sek;
	  break;
        }
      }
      ret=kl*10;
      if(ret==60)
        ret=0;

      sprintf(sec10,"%2d",ret);
//    printf("  sec10: %s\n",sec10);
      incr = (float)(xsiz-1)/((float)rate * ((float)nemin/rate));
  
      ix_old = 0;
      ix_new = 0;
      iy_old = 335;
      iy_new = 335;
      idx    = 0;
      mark10= diff * rate;
      for(i=0;i<nusmp[cmpix];i++)
      {
        idx = idx + incr;
        ix_new = idx;
	
        gdImageLine(im, ix_old, iy_old, ix_new, iy_new, tl); // draw timescale
        if(i==mark10)
        {
          gdImageLine(im, ix_new, 40, ix_new, 335, tl);      // draw 10 seconds marks		    
          if(sec10[0]==' ')
	    gdImageLine(im, ix_new, 320, ix_new, 335, sg4);
          gdImageString(im,gdFontGetLarge(),ix_new-8,313,sec10,tk); // draw in black minute marks
          mark10 = mark10 + 10.0 * rate;
          ret=ret+10;
	  if(ret == 60)
            ret = 0;
          sprintf(sec10,"%2d",ret);
        }
        ix_old=ix_new;
      }
//--------------------------------------------------------
// draw station components
//--------------------------------------------------------
      avstand=yincr;
      iy_old=yincr;
      iy_new=yincr;
   
      for(kl=0;kl<ncomp[cmpix];kl++)
      {
        if(COMPWGAPS[cmpix+kl] == 1)
        {        
          ix_old=incr * srates[cmpix] * (MSECS_START[cmpix+kl]-MINI );
          idx = ix_old;
          ix=nusmp[cmpix];
          MaxAmpRaw(ix,cmpix+kl,&MAXAMP_RAW);
          Scale_to_100_Raw(ix,cmpix+kl,MAXAMP_RAW);
          MaxAmpFlt(ix,cmpix+kl,&MAXAMP_FLT);
          Scale_to_100_Flt(ix,cmpix+kl,MAXAMP_FLT);
//printf("ncomp: %2d kl:%2d MAX_RAW: %10.3f  MAX_FLT: %10.3f\n",ncomp[cmpix],kl,MAXAMP_RAW,MAXAMP_FLT);
          for(i=0;i<ix;i++)                          // plot this buffer
          {
            ix_new=(int)idx;
//          iy_new=(int)vraw[cmpix+kl][i]*1.0+avstand;
            iy_new=(int)vflt[cmpix+kl][i]*1.01+avstand;
            gdImageLine(im, ix_old, iy_old, ix_new, iy_new, sg4);  // draw from old to new point
            ix_old=ix_new;
            iy_old=iy_new;
            idx=idx+incr;
          }
//-----------------------------------------------
// draw phases this component
//-----------------------------------------------
          if(PPH[cmpix+kl] > 0)
          {

	    gdImageSetThickness(im,2);
            sprintf(dum,"P");
            displ=incr * srates[cmpix] * (PPH[cmpix+kl] - MINI);
            gdImageLine(im, displ, avstand+30, displ, avstand-30, sg1); 
            gdImageLine(im, displ, avstand, displ-30, avstand-30, sg2);
            gdImageString(im,gdFontGetLarge(),displ-10,avstand-23,dum,sg3);
	    gdImageSetThickness(im,1);	
          }
          if(SPH[cmpix+kl] > 0)
          {	
	    gdImageSetThickness(im,2);
	    sprintf(dum,"S");
            displ=incr * srates[cmpix] * (SPH[cmpix+kl] - MINI);
            gdImageLine(im, displ, avstand+30, displ, avstand-30, sg1);
            gdImageLine(im, displ, avstand, displ-30, avstand-30, sg2);
            gdImageString(im,gdFontGetLarge(),displ-10,avstand-23,dum,sg3); 
	    gdImageSetThickness(im,1);	
          }
          if(COD[cmpix+kl] > 0)
          {	
	    gdImageSetThickness(im,2);
	    sprintf(dum,"C");
            displ=incr * srates[cmpix] * (COD[cmpix+kl] - MINI);
            gdImageLine(im, displ, avstand+30, displ, avstand-30, sg1);
            gdImageLine(im, displ, avstand, displ-30, avstand-30, sg2);
            gdImageString(im,gdFontGetLarge(),displ-10,avstand-23,dum,sg3); 
	    gdImageSetThickness(im,1);	
          }
          idx=0;                                               // update counter before next component
          ix_old=0;
          avstand = avstand + yincr;
          iy_old=avstand;
          iy_new=avstand;
      
        }          
      }
      avstand=yincr;
//--------------------------------------------------
// write to png file
//--------------------------------------------------
      out = fopen (pngfile, "wb");
      gdImagePng (im, out);
      fclose (out);
      gdImageDestroy(im);                   /* Destroy the image in memory. */
   
    }   // ret
    
    
    
  }

  if(prt >= 1)
    printf("PLOT ALL CHANNELS\n");  
//-----------------------------------------------
// find minimum start time all components
//-----------------------------------------------
  MINI=MSECS_START[0];
  kan=cmpix;
  for(kl=0;kl<nchannels;kl++)
  {      
    if(MSECS_START[kl] < MINI)
    {
      MINI=MSECS_START[kl];
      kan=kl;
      nemin=nusmp[kl];
      rate=srates[kl];
    }
  }

//----------------------------------
// plot start time
//----------------------------------
  gdImageString(im,gdFontGetLarge(),80,15,TIME_START[kan],tk);      

  sprintf(timestring,"%s",TIME_START[kan]);
  sscanf(timestring,"%4d%1c%3d%1c%2d%1c%2d%1c%f6.3",&YR,dum,&DY,dum,&ihrn,dum,&imin,dum,&sek);
  for(kl=0;kl<7;kl++)
  {
    if((kl * 10) > sek)
    {
      diff = (kl * 10) - sek;
      break;
    }
  }

 
  ret=kl*10;
  if(ret==60)
    ret=0;
  sprintf(sec10,"%2d",ret);
//incr = (float)845/((float)rate * ((float)nemin/rate));

  incr = (float)(xsiz-1)/((float)rate * ((float)nemin/rate));

  if(prt >= 1)
    printf("RATE: %d NEMIN: %d INCR: %10.5f\n",rate,nemin,incr);

  im  = gdImageCreate(xsiz, 4000);                 // Allocate the image: tot_wd pixels width by tot_hg pixels high
  bg  = gdImageColorAllocate(im, 255, 255, 255); // WHITE first color definition defines the background color
  tl  = gdImageColorAllocate(im, 200, 200, 200); // GRAY  color for timelines
  tk  = gdImageColorAllocate(im,   0,   0,   0); // BLACK color for text
  tt  = gdImageColorAllocate(im,   0,   0, 255); // BLUE color for title
  t5  = gdImageColorAllocate(im,   0,   0,   0); // BLACK color for 5 minute marks
  sg1 = gdImageColorAllocate(im, 255,   0,   0); // RED   color seismogram
  sg2 = gdImageColorAllocate(im,   0, 255,   0); // GREEN color seismogram
  sg3 = gdImageColorAllocate(im,   0,   0, 255); // BLUE color seismogram
  sg4 = gdImageColorAllocate(im,   0,   0,   0); // BLACK color seismogram  
//------------------------------------  
// GENERATE PLOT ALL COMPONENTS
//------------------------------------
  yincr = 50;
  avstand=yincr;
  sprintf(pngfile,"%s/loc/ALL.png",topdir_html);     // file to write png
//  printf("PNG_PLOTS_NEW: %s  %d channels\n",pngfile,nchannels);
  ix=999999999;
  for(cmpix = 0; cmpix < nchannels; cmpix++)
  {
    if(nusmp[cmpix] < ix)
      ix = nusmp[cmpix];
  }
  for(cmpix = 0; cmpix < nchannels; cmpix++)
  {
    dg=sg4;      

    ix_old=0;
    iy_old=avstand;
    ix_new=0;
    iy_new=avstand;
   
    gdImageLine(im, 0, 0, xsiz-1, 0, dg);                       // plot frame
    gdImageLine(im, xsiz-1, 0, xsiz-1, 3950, dg);                   // plot frame
    gdImageLine(im, xsiz-1, 3950, 0, 3950, dg);                   // plot frame
    gdImageLine(im, 0, 3950, 0, 0, dg);                       // plot frame  
  

    iy_old=avstand;
    iy_new=avstand;

    ix_old=incr * srates[cmpix] * (MSECS_START[cmpix]-MINI );
  
    incr = 169.0/(float)(srates[cmpix] * 60);
    incr = (float)(xsiz-1)/((float)(srates[cmpix]) * 300);
  
    ix_old = incr * srates[cmpix] * (MSECS_START[cmpix]-MINI );
  
//printf("%2d %s secs: %5.2f %4d ix_old: %5d %6.4f\n",cmpix,STATION_NAMES[cmpix],(MSECS_START[cmpix]-MINI ),srates[cmpix],ix_old,incr);
    if(COMPWGAPS[cmpix] == 1)  
    {  
  
      idx = ix_old;

      sprintf(dum,"%s",STATION_NAMES[cmpix]);
      strncpy(nam,dum,12);

      gdImageString(im,gdFontGetLarge(),10,iy_new-15,dum,sg3);

      for(i=0;i<nusmp[cmpix];i++)                          // plot this buffer
      {
        ix_new=(int)idx;
        iy_new=(int)vflt[cmpix][i]*0.5001+avstand;
        gdImageLine(im, ix_old, iy_old, ix_new, iy_new, dg);  // draw from old to new point
        ix_old=ix_new;
        iy_old=iy_new;
        idx=idx+incr;
      }
//-----------------------------------------------
// draw phases this component
//-----------------------------------------------
      if(PPH[cmpix] > 0)
      {
        gdImageSetThickness(im,2);    
        sprintf(dum,"P");
        displ=incr * srates[cmpix] * (PPH[cmpix] - MINI);
        gdImageLine(im, displ, avstand+30, displ, avstand-30, sg1); 
        gdImageLine(im, displ, avstand, displ-30, avstand-30, sg2);
        gdImageString(im,gdFontGetLarge(),displ-10,avstand-23,dum,sg3); 
        gdImageSetThickness(im,1);    
      }
      if(SPH[cmpix] > 0)
      {
        gdImageSetThickness(im,2);    
        sprintf(dum,"S");
        displ=incr * srates[cmpix] * (SPH[cmpix] - MINI);
        gdImageLine(im, displ, avstand+30, displ, avstand-30, sg1);
        gdImageLine(im, displ, avstand, displ-30, avstand-30, sg2);
        gdImageString(im,gdFontGetLarge(),displ-10,avstand-23,dum,sg3); 
        gdImageSetThickness(im,1);    
      }
      idx=0;
      ix_old=0;
      avstand = avstand + yincr;
  
    }  
  }
  out = fopen (pngfile, "wb");
  gdImagePng (im, out);
  fclose (out);
//  printf("PNG_PLOTS_NEW: Close file\n");
  gdImageDestroy(im);                   /* Destroy the image in memory. */
}

#endif


int Check_SS(char sfilepath[])
{
  FILE *sf;
  FILE *out;
  int no,i,br,l,n;
  int totlines;
  char linjer[500];
  char params1[MAXCHA][500];
  char toplot1[MAXCHA][500];
  int index[500];
  char navn[256];

  int goon;
  int cr;
  int readings_left;
  float residual;
  int ant=0;
  int idx;
  int bytes_read;
  char dummy[500];
  float res[256];
  float res_save[256];
  float max_res;
  sf=fopen(sfilepath,"r");

  if(sf == NULL)
  {
    printf("rtpng:CHECK_S:Can't open S-file: %s\n",sfilepath);
    exit(0);
  }else{
    if(prt >= 1)
      printf("rtpng:Found....................: %s\n",sfilepath);

    br=0;
    cr=0;
    no = 200;
    totlines=0;
    sprintf(navn,"CAZ7");
    while ( fgets ( linjer, sizeof linjer, sf ) != NULL ) /* read a line from file */
    {
//      printf("%s",linjer);
      if (strstr(linjer, navn) )
      {
//	printf("%s", linjer);
        break;
      }
    }

    ant=0;
    while ( fgets ( linjer, sizeof linjer, sf ) != NULL ) /* read a line from file */
    {
     if(prt >= 1)   
        printf("%s",linjer);
      sprintf(HYPFILE[ant],"%s",linjer);
      ant++;
    }
    fclose(sf);
}
no_lines=ant;
if(prt >= 1)
  printf("NO_LINES: %d\n",no_lines); 
return(no_lines);

}

//---------------------------------------------------------------------------
// param_cmdlin
//
// Read command line parameters.
//
// Returns 0 if OK, and -1 on ERROR
//---------------------------------------------------------------------------
static int
param_cmdlin(int argcount, char **argvec)
{
  int cnt;
  int err = 0;

  if (argcount <= 1)
    err++;

  /* Process all command line arguments */
for (cnt = 1; cnt < argcount; cnt++)
{
  if (strcmp (argvec[cnt], "-h") == 0)
  {
    howto();
    exit (0);
  }
  else if (strcmp (argvec[cnt], "-sfile") == 0)
  {
    strcpy(sfilename, argvec[++cnt]);
    if(prt > 0)
      printf("param_cmdlin: sfilename..................: %s\n",sfilename);	  
  }
  else if (strcmp (argvec[cnt], "-wavefile") == 0)
  {
    wavefiledef = 1;
    strcpy(wfilename, argvec[++cnt]);
    if(prt > 0)    
      printf("param_cmdlin: wfilename..................: %s\n",wfilename);	  
  }
  else if (strcmp (argvec[cnt], "-locate") == 0)
  {
    locate = atoi(argvec[++cnt]);
    if(prt > 0)   
      printf("param_cmdlin: autolocate.................: %d\n",locate);	  
  }
  else if (strcmp (argvec[cnt], "-iter") == 0)
  {
    iterations = atoi(argvec[++cnt]);
    if(prt > 0)    
      printf("param_cmdlin: iterations.................: %d\n",iterations);    
  }
  else if (strcmp (argvec[cnt], "-maxres") == 0)
  {
    maxres = atof(argvec[++cnt]);
    if(prt > 0)    
      printf("param_cmdlin: maxres.....................: %6.1f\n",maxres);    
  }
  else if (strcmp (argvec[cnt], "-prt") == 0)
  {
    prt = atoi (argvec[++cnt]);
    if(prt > 0)    
      printf("param_cmdlin: prt........................: %d\n",prt);
  }
  else if (strcmp (argvec[cnt], "-keep") == 0)
  {
    keep = atoi (argvec[++cnt]);
    if(prt > 0)    
      printf("param_cmdlin: keep.......................: %d\n",keep);
  }
  else if (strcmp (argvec[cnt], "-mail") == 0)
  {
    mail1 = 1;
    strcpy(mailaddress1, argvec[++cnt]);
    if(prt > 0)    
      printf("param_cmdlin: mailaddress1...............: %s\n",mailaddress1);	  
  }  
  else if (strncmp (argvec[cnt], "-", 1 ) == 0)
  {
    printf("Unknown option: %s\n", argvec[cnt]);
    exit (1);
  }

}

  /* If errors then report the usage message and quit */
  if (err)
    {
      howto();
      exit (1);
    }
//---------------------------------------------
// mseed2askii
//---------------------------------------------
  /* Check the input files for any list files, if any are found
   * remove them from the list and add the contained list */
  if ( filelist )
    {
      struct listnode *prevln, *ln;
      char *lfname;

      prevln = ln = filelist;
      while ( ln != 0 )
        {
          lfname = ln->data;

          if ( *lfname == '@' )
            {
              /* Remove this node from the list */
              if ( ln == filelist )
                filelist = ln->next;
              else
                prevln->next = ln->next;

              /* Skip the '@' first character */
              if ( *lfname == '@' )
                lfname++;

              /* Read list file */
//              readlistfile (lfname);

              /* Free memory for this node */
              if ( ln->key )
                free (ln->key);
              free (ln->data);
              free (ln);
            }
          else
            {
              prevln = ln;
            }
	  
          ln = ln->next;
        }
    }
//----------------------------------------------------  

  return 0;
}                           // param_cmdlin()
//--------------------------------------------------------------------------
// howto:
// Print the -h help message and exit.
//--------------------------------------------------------------------------
static void
howto(void)
{
  printf("\nHelp: AUTOPHASE [options]\n\n");
  printf("Options:\n");
  printf(" -h             this help message\n");
  printf("\n");
  printf("Options:\n");
  printf(" -sfile    txt  name of s-file, full path\n");
  printf(" -wavefile txt  name of waveform file, full path\n");
#ifdef RTQUAKE  
  printf(" -locate   n    0 - no autolocation, 1 - autolocation (default = 0)\n");
  printf(" -iter     n    number of iterations (default = 200)\n");
  printf(" -maxres   n    maximum residual (default = 3.0)\n");
  printf(" -prt      n    1-10, the higher number, the more printout (debug)\n");
  printf(" -keep     n    1 - keep first part of s-file, 0 - write new sfile\n");
  printf(" -mail     txt  valid email address to send event info\n");
#endif  
  printf("\n");
}                         // howto()
//--------------------------------------------------------------------------
// Read_Parameters()
// read parameters from rtquake.par 
//--------------------------------------------------------------------------
void Read_Parameters()
{
  FILE *par;
  char  dummy[256];
  char  linjer[256];
  char  keyword[256];
  float val;
  int   valint;
  int   br = 0;
  
  sprintf(dummy,"%s/com/rtquake.par",topdir_rt);
  par = fopen(dummy,"r");
  if(par == NULL)
  {
    printf("AUTOPHASE: rtquake.par does not exist in %s/com. Use program defaults.\n",topdir_rt);
    br=1;      
  }
  
  if(br == 0)
  {
    printf("AUTOPHASE: read rtquake.par\n");
    sprintf(keyword,"LOCATION");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//	printf("val: %5.1f\n",val);
	locate = (int)val;
        break;
      }
    }
    rewind(par);
    sprintf(keyword,"AUTOMAG");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
	automag = (int)val;
        break;
      }
    }   
    rewind(par);    
    sprintf(keyword,"KEEP");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//        printf("val: %5.1f\n",val);
	keep = (int)val;
        break;
      }
    }   
    rewind(par);    
    sprintf(keyword,"ITERATION");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//        printf("val: %5.1f\n",val);
	iterations = (int)val;
        break;
      }
    }   
    rewind(par);    
    sprintf(keyword,"PRINTING");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//        printf("val: %5.1f\n",val);
//	prt = (int)val;	
        break;
      }
    }
    
    
    
    rewind(par);    
    sprintf(keyword,"MAX_RESIDUAL");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//        printf("val: %5.1f\n",val);
	maxres = val;
//	printf("MAX_RES: %5.2f\n",maxres);
//	exit(0);
        break;
      }
    }
    
 
    rewind(par);    
    sprintf(keyword,"MINDIFF_SP");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//        printf("val: %5.1f\n",val);
	MINDIFF = val;
//	printf("MINDIFF: %5.2f\n",MINDIFF);
//	exit(0);
        break;
      }
    } 
 
    
    rewind(par);
    sprintf(keyword,"MINSTALOC");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%d",dummy,&valint);
//        printf("val: %5.1f\n",val);
	no_stations_trg = valint;
//	printf("MINSTALOC  no_stations_trg: %d\n",no_stations_trg);
        break;
      }
    }       
    rewind(par);
    
    sprintf(keyword,"GEOLOCATION");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
        if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//        printf("val: %5.1f\n",val);
	geolocation = (int)val;
        break;
      }
    }   
    rewind(par);  
    
    
    sprintf(keyword,"GEODETAIL");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
        if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%f",dummy,&val);
//        printf("val: %5.1f\n",val);
	geodetail = (int)val;
        break;
      }
    }   
    rewind(par);     

    
    sprintf(keyword,"PHASES");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%40c%d",dummy,&valint);
//        printf("val: %5.1f\n",val);
	sphases = valint;
//	printf("PHASES  sphases: %d\n",sphases);
        break;
      }
    }       
    rewind(par);    
    
    
    sprintf(keyword,"MAIL1");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%50c%s",dummy,mailaddress1);
	sscanf(linjer,"%40c%f",dummy,&val);	
//        printf("val: %5.1f  %s\n",val,mailaddress1);
	mail1 = (int)val;	
        break;
      }
    }       
    rewind(par);    
    sprintf(keyword,"MAIL2");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%50c%s",dummy,mailaddress2);
	sscanf(linjer,"%40c%f",dummy,&val);	
//        printf("val: %5.1f  %s\n",val,mailaddress1);
	mail2 = (int)val;	
        break;
      }
    }      
    rewind(par);    
    sprintf(keyword,"MAIL3");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%50c%s",dummy,mailaddress3);
	sscanf(linjer,"%40c%f",dummy,&val);	
//        printf("val: %5.1f  %s\n",val,mailaddress1);
	mail3 = (int)val;	
        break;
      }
    }      
    
    rewind(par);    
    sprintf(keyword,"MAIL4");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%50c%s",dummy,mailaddress4);
	sscanf(linjer,"%40c%f",dummy,&val);	
//        printf("val: %5.1f  %s\n",val,mailaddress1);
	mail4 = (int)val;	
        break;
      }
    }        
    
    rewind(par);    
    sprintf(keyword,"MAIL5");
    while ( fgets ( linjer, sizeof linjer, par ) != NULL ) /* read a line from file */
    {
  
      if (strstr(linjer, keyword) )
      {
	if(prt > 0)
	  printf("%s", linjer);
	sscanf(linjer,"%50c%s",dummy,mailaddress5);
	sscanf(linjer,"%40c%f",dummy,&val);	
//        printf("val: %5.1f  %s\n",val,mailaddress1);
	mail5 = (int)val;	
        break;
      }
    }      
    fclose(par);        
  }
}                    // Read_Parameters()
//------------------------------------------------------------------------
// les_linje(
//
// read s-file lines
//------------------------------------------------------------------------
int les_linje(FILE *streamfp,char linje[])
{
  int l;
  int br;
  br=0;
  for(l=0;l<85;l++)
  {
    linje[l]=fgetc(streamfp);
    if(linje[l] == 0xa && l == 0)
    {
      br=2;
      break;
    }
    if(linje[l] == 0xa && l > 0)
    {
      linje[l]='\0';
      break;        
    }
    if(linje[l] == EOF)
    {
//      printf("End of file. File contains: %2d items\n",i);

      br=1;
      return(br);
    }
  }
  return(br);
}                        // les_linje
//-------------------------------------------------------------------------------
// readS
//
// open s-file and read the '1' line to get the time for trigger
// continue to read line '6' to get the wavefile name
//-------------------------------------------------------------------------------
void readS(char sfilename[],char MINTRGTID[],char TRG_TID[],char fnam[],int *subnet,char yyr_trg[],char mmo_trg[])
{
  FILE *sfile;
  char record[256];
  char yr_trg[20];
  char mo_trg[20];
  char da_trg[20];
  char hr_trg[20];
  char mi_trg[20];
  char se_trg[20];
  
  char dm[50];
  int  i;
  int  br;
  int  sn;
  
  if ((sfile = fopen (sfilename, "rb")) == NULL)
  {
    printf("readS: Can't open sfile file: %s\n",sfilename);
    exit(0);
  }
  for(i=0;i<50;i++)
  {
    br = les_linje(sfile,record);  // read first line in new station, and save samplerate + timestamp
    if(prt >= 4)
      printf("%s\n",record);
    if(record[79]=='1')
    {
      sscanf(record,"%s",yr_trg);
      sscanf(record,"%s",yyr_trg);
      dm[0]=record[6];
      if(dm[0]==' ') dm[0]='0';
      dm[1]=record[7];
      dm[2]='\0';
      sscanf(dm,"%s",mo_trg);
      sscanf(dm,"%s",mmo_trg);
      dm[0]=record[8];

      if(dm[0]==' ') dm[0]='0';
      dm[1]=record[9];
      dm[2]='\0';

      sscanf(dm,"%s",da_trg);
      dm[0]=record[11];
      if(dm[0]==' ') dm[0]='0';
      dm[1]=record[12];
      dm[2]='\0';
      sscanf(dm,"%s",hr_trg);
      dm[0]=record[13];
      if(dm[0]==' ') dm[0]='0';
      dm[1]=record[14];
      dm[2]='\0';
      sscanf(dm,"%s",mi_trg);
      dm[0]=record[16];
      if(dm[0]==' ') dm[0]='0';
      dm[1]=record[17];
      dm[2]=record[18];
      dm[3]=record[19];
      dm[4]='\0';
      sscanf(dm,"%s",se_trg);
//      printf("%s %s %s %s %s %s\n",yr_trg,mo_trg,da_trg,hr_trg,mi_trg,se_trg);
      sprintf(MINTRGTID,"%s/%s/%s %s:%s:%s",da_trg,mo_trg,yr_trg,hr_trg,mi_trg,se_trg);
      sprintf(TRG_TID,"%s-%s-%s-%s:%s:%s",yr_trg,mo_trg,da_trg,hr_trg,mi_trg,se_trg);        
//      printf("readS:...................................: MINTRGTID: %s\n",MINTRGTID);
//      printf("readS:...................................: TRG_TID:   %s\n",TRG_TID);      
    }
    if(record[79]=='6')
      break;
  }
  sprintf(fnam,"%s",record);
  for(i=0;i<100;i++)
  {
    fnam[i]=fnam[i+1];
    if(fnam[i]==' ')
    {
      fnam[i]='\0';
      break;
    }
  }
  if(prt >= 1)
  {
    printf("Extract waveform filename from original s-file line 6\n");
    printf("readS:...................................: %s\n",fnam);
  }
  fclose(sfile);
  sscanf(fnam,"%32c%d",dm,&sn);
  *subnet = sn;
}
/***************************************************************************
 * addnode:
 *
 * Add node to the specified list.
 *
 * Return a pointer to the added node on success and NULL on error.
 ***************************************************************************/
static struct listnode *
addnode (struct listnode **listroot, void *key, int keylen,
	 void *data, int datalen)
{
  struct listnode *lastlp, *newlp;
  
  if ( data == NULL )
    {
      fprintf (stderr, "addnode(): No data specified\n");
      return NULL;
    }

  lastlp = *listroot;
  while ( lastlp != 0 )
    {
      if ( lastlp->next == 0 )
        break;

      lastlp = lastlp->next;
    }
  
  /* Create new listnode */
  newlp = (struct listnode *) malloc (sizeof (struct listnode));
  memset (newlp, 0, sizeof (struct listnode));
  
  if ( key )
    {
      newlp->key = malloc (keylen);
      memcpy (newlp->key, key, keylen);
    }
  
  if ( data)
    {
      newlp->data = malloc (datalen);
      memcpy (newlp->data, data, datalen);
    }
 
  newlp->next = 0;
  
  if ( lastlp == 0 )
    *listroot = newlp;
  else
    lastlp->next = newlp;
  
  return newlp;
}  /* End of addnode() */
/***************************************************************************
 * writeascii:
 * 
 * Write data buffer to output file as ASCII.
 *
 * Returns the number of samples written or -1 on error.
 ***************************************************************************/
static int
writeascii (MSTrace *mst,int chan)
{
  char outfile[1024];
  char *outname;
  char timestr[50];
  char srcname[50];
  char *samptype;
  
  int line, col, cnt, samplesize;
  int lines;
  void *sptr;


  
  if ( ! mst )
  {
//    printf("Return -1 from writeascii\n");
    return -1;
  
  }
  if ( mst->numsamples == 0 || mst->samprate == 0.0 )
  {
//    printf("Return 0 from writeascii\n");
    return 0;
  }


  
  if ( verbose )
    printf ("Writing ASCII for %.8s.%.8s.%.8s.%.8s\n",
	     mst->network, mst->station, mst->location, mst->channel);
  sprintf(STASJONSCOMP[chan],"%.8s.%.8s.%.8s.%.8s",mst->network, mst->station, mst->location, mst->channel);  
  /* Generate source name and ISO time string */
  mst_srcname (mst, srcname, 1);
  ms_hptime2isotimestr (mst->starttime, timestr, 1);
    
  /* Set sample type description */

  if ( mst->sampletype == 'i' )
    {
      samptype = "INTEGER";
     
    }

if(prt == -10)
{  
  /* Generate and open output file name if single file not being used */
  if ( ! ofp )
    {
      /* Create output file name: Net.Sta.Loc.Chan.Qual.Year-Month-DayTHour:Min:Sec.Subsec.txt */
      snprintf (outfile, sizeof(outfile), "%s.%s.%s.%s.%c.%s.txt",
		mst->network, mst->station, mst->location, mst->channel,
		mst->dataquality, timestr);
    
         
    
      /* Open output file */
      if ( (ofp = fopen (outfile, "wb")) == NULL )
	{
	  fprintf (stderr, "Cannot open output file: %s (%s)\n",
		   outfile, strerror(errno));
	  return -1;
	}
      
      outname = outfile;
    }
}  
  /* Header format:
   * "TIMESERIES Net_Sta_Loc_Chan_Qual, ## samples, ## sps, isotime, SLIST|TSPAIR, INTEGER|FLOAT|ASCII, Units" */
  
       /* Print header line */
if(prt == -10)
{
      fprintf (ofp, "TIMESERIES %s, %lld samples, %g sps, %s, SLIST, %s\n",
	       srcname, (long long int)mst->numsamples, mst->samprate, timestr, samptype);
}
      sprintf (COMP_HDR[chan], "TIMESERIES %s , %lld samples, %g sps, %s , SLIST, %s",
	       srcname, (long long int)mst->numsamples, mst->samprate, timestr, samptype);

// printf("writeascii chan: %2d COMP_HDR: %s\n",chan,COMP_HDR[chan]);
     
//      lines = (mst->numsamples / slistcols) + ((slistcols == 1) ? 0 : 1);
      lines = mst->numsamples;
//      printf("lines: %d numsamples: %d\n",lines,mst->numsamples);
      
      if ( (samplesize = ms_samplesize(mst->sampletype)) == 0 )
	{
	  fprintf (stderr, "Unrecognized sample type: %c\n", mst->sampletype);
	}
      

      else
	for ( cnt = 0, line = 0; line < lines; line++ )
	  {
		if ( cnt < mst->numsamples )
		  {
		    sptr = (char*)mst->datasamples + (cnt * samplesize);
		    
		    if ( mst->sampletype == 'i' )
		      {
if(prt == -10)
			  fprintf (ofp, "%d", *(int32_t *)sptr);
//			printf("%d\n",*(int32_t *)sptr);
        verdier[chan][cnt]=*(int32_t *)sptr;
		      }

		      cnt++;
		  }
if(prt == -10)
	    fprintf (ofp, "\n");
	  }

if(prt == -10)
{  
  if ( outname == outfile )
    {
      fclose (ofp);
      ofp = 0;
    }
}  
//  fprintf (stderr, "Wrote %lld samples from %s to %s\n",
//	   (long long int)mst->numsamples, srcname, outname);
  
  return mst->numsamples;
}  /* End of writeascii() */
/*----------------------------------------------------------------*/
/* Read wavefile converted from miniseed to askii: trigger.ask    */
/*----------------------------------------------------------------*/
int read_trigger(int comp_cnt,int en_to)
{
  char linje[100];
  char station[25];
  char current_station[256];
  char stdstation[256];
  int  ns;
  int  sr;
  int  sr_first;
  char timestamp[256];
  char timestampcpy[256];
  float value[MAXSMP];
  int nsamp;
  int tegn;
  int ret;
  int first;
  int i,l,n,no,p,unti;
  int cmp;
  int teller;
  int len;
  char dum[100];
  int encontrado=0;



  p=0;
  first = 0;
  cmp=0;

  for(teller=0;teller < comp_cnt;teller++)
  { 
    encontrado=0;
   sprintf(linje,"%s",COMP_HDR[teller]);
//printf("\n");
//   printf("%s\n",linje);
   unti=0;
   for(n=0;n<30;n++)
   {
     if(linje[n]==',')
       unti=n;
     if(unti != 0)
       break;
   }
   for(n=11;n<unti;n++)
   {
     if(linje[n] == ' ' && linje[n+1] != ',')
     {
       linje[n]='_';
       break;
     }
   }
//   printf("%s\n",linje);
   
//   sscanf(linje,"%s %s %s %d %s %d %s %s",dum,station,dum,&ns,dum,&sr,dum,timestamp);
//   printf("2 %2d %s %5d %5d %s\n",teller,station,ns,sr,timestamp);

//TIMESERIES _OSG__S Z_D , 7984 samples, 48.01 sps, 1996-06-25T03:37:20.651000 , SLIST, INTEGER
//2 11 _OSG__S_Z_D    48 1980829728 1996-06-25T03:37:20.651000


   float ssrr;
   sscanf(linje,"%s %s %s %d %s %f %s %s",dum,station,dum,&ns,dum,&ssrr,dum,timestamp);
   sr = (int)ssrr;
//   printf("2 %2d %s %5d %5d %s\n",teller,station,ns,sr,timestamp);



  
  for(no=0;no<ns;no++)
    value[no]=verdier[teller][no];
  sr_first = sr;
  sprintf(current_station,"%s",station);
  sprintf(timestampcpy,"%s",timestamp);
//  printf("3 %2d STATION: %s %2d\n",teller,current_station,strlen(current_station));
  len=strlen(current_station);
  
//  printf("current_station: %s   station: %s\n",current_station,station);
  
// Check station name. A standard station name of the form XX_OSL___00_EHZ is generated
// where the XX represent missing networkname
//-------------------------------------------------------------------
// 00000000001111111
// 01234567890123456
// C_12__BHZ_D

  if(station[0] != '_')                      // check if station name does NOT contain network name
  {
// 00000000001111111
// 01234567890123456
// C_12__BHZ_D   
    if(station[1] == '_' && station[4] == '_' && station[5] == '_' && station[9] == '_') 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';                    // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = '_';
      stdstation[6]  = '_';
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[6];
      stdstation[13] = current_station[7];
      stdstation[14] = current_station[8];
      stdstation[15] = '\0';
//      printf("%s          %s\n",current_station,stdstation);
      encontrado++;      
    }
// 00000000001111111
// 01234567890123456
// C_123__BHZ_D     
    if(station[1] == '_' && station[5] == '_' && station[6] == '_' && station[10] == '_')  // C_123__BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';                    // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = '_';
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[7];
      stdstation[13] = current_station[8];
      stdstation[14] = current_station[9];
      stdstation[15] = '\0';
//      printf("%s         %s\n",current_station,stdstation);
      encontrado++;      
    }
// 00000000001111111
// 01234567890123456
// C_1234__BHZ_D     
    if(station[1] == '_' && station[6] == '_' && station[7] == '_' && station[11] == '_')  // C_1234__BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';                    // networkname      
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = current_station[5];
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[8];
      stdstation[13] = current_station[9];
      stdstation[14] = current_station[10];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s        %s\n",current_station,stdstation);
      encontrado++;      
    }
// 00000000001111111
// 01234567890123456
// C_12345__BHZ_D     
    if(station[1] == '_' && station[7] == '_' && station[8] == '_' && station[12] == '_')  // C_PB099_BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';                    // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = current_station[5];
      stdstation[7]  = current_station[6];
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[9];
      stdstation[13] = current_station[10];
      stdstation[14] = current_station[11];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s       %s\n",current_station,stdstation);
      encontrado++;      
    } 
// 00000000001111111
// 01234567890123456
// C_12__00_BHZ_D      
    if(station[1] == '_' && station[7] == '_' && station[12] == '_')  // C_PB099__BHE_D 
    {
     
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';                    // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = current_station[5];
      stdstation[7]  = current_station[6];
      stdstation[8]  = '_';
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[9];
      stdstation[13] = current_station[10];
      stdstation[14] = current_station[11];
      stdstation[15] = '\0';
//      printf("stdstation:  %s\n",stdstation);
//      printf("%s     %s\n",current_station,stdstation);
      encontrado++;      
    }        
    if(station[1] == '_' && station[5] == '_' && station[8] == '_') // G_LVC_00_BHZ_D
    {
//      printf("LOC: %c%c\n",station[7],station[8]);
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';                    // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = '_';
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = current_station[6];
      stdstation[10] = current_station[7];
      stdstation[11] = '_';
      stdstation[12] = current_station[9];
      stdstation[13] = current_station[10];
      stdstation[14] = current_station[11];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s     %s\n",current_station,stdstation);
      encontrado++;      
    }
     if(station[1] == '_' && station[6] == '_' && station[9] == '_') // G_SFJD_10_BHZ_D
    {
//      printf("LOC: %c%c\n",station[7],station[8]);
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = current_station[5];
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = current_station[7];
      stdstation[10] = current_station[8];
      stdstation[11] = '_';
      stdstation[12] = current_station[10];
      stdstation[13] = current_station[11];
      stdstation[14] = current_station[12];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s    %s\n",current_station,stdstation);
      encontrado++;      
    }    
    if(station[1] == '_' && station[7] == '_' && station[10] == '_') // G_SFJD1_10_BHZ_D
    {
//      printf("LOC: %c%c\n",station[7],station[8]);
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = current_station[5];
      stdstation[7]  = current_station[6];
      stdstation[8]  = '_';      
      stdstation[9]  = current_station[8];
      stdstation[10] = current_station[9];
      stdstation[11] = '_';
      stdstation[12] = current_station[11];
      stdstation[13] = current_station[12];
      stdstation[14] = current_station[13];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s   %s\n",current_station,stdstation);
      encontrado++;      
    }        
    if(station[2] == '_' && station[5] == '_' && station[10] == '_')  // CX_PB0___BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = '_';
      stdstation[6]  = '_';
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[7];
      stdstation[13] = current_station[8];
      stdstation[14] = current_station[9];
      stdstation[15] = '\0';
//      printf("%s       %s\n",current_station,stdstation);
      encontrado++;      
    }
    if(station[2] == '_' && station[6] == '_' && station[11] == '_')  // CX_PB0___BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = '_';
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[8];
      stdstation[13] = current_station[9];
      stdstation[14] = current_station[10];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s      %s\n",current_station,stdstation); 
      encontrado++;       
    } 
    if(station[2] == '_' && station[7] == '_' && station[12] == '_')  // CX_PB09__BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = current_station[6];
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[9];
      stdstation[13] = current_station[10];
      stdstation[14] = current_station[11];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s     %s\n",current_station,stdstation); 
      encontrado++;      
    }
    if(station[2] == '_' && station[8] == '_' && station[13] == '_')  // CX_PB099__BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = current_station[6];
      stdstation[7]  = current_station[7];
      stdstation[8]  = '_';
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[10];
      stdstation[13] = current_station[11];
      stdstation[14] = current_station[12];
      stdstation[15] = '\0';
//      printf("stdstation:  %s\n",stdstation);
//      printf("%s    %s\n",current_station,stdstation);
      encontrado++;      
    }
    if(station[2] == '_' && station[6] == '_' && station[9] == '_') // GE_LVC_00_BHZ_D
    {
//      printf("LOC: %c%c\n",station[7],station[8]);
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = '_';
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = current_station[7];
      stdstation[10] = current_station[8];
      stdstation[11] = '_';
      stdstation[12] = current_station[10];
      stdstation[13] = current_station[11];
      stdstation[14] = current_station[12];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s    %s\n",current_station,stdstation);
      encontrado++;      
    } 
    if(station[2] == '_' && station[8] == '_' && station[11] == '_' && station[15] == '_')  // CX_PB099__BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = current_station[6];
      stdstation[7]  = current_station[7];
      stdstation[8]  = '_';
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[12];
      stdstation[13] = current_station[13];
      stdstation[14] = current_station[14];
      stdstation[15] = '\0';
//      printf("stdstation:  %s\n",stdstation);
//      printf("%s  %s\n",current_station,stdstation);
      encontrado++;      
    }
    if(station[2] == '_' && station[8] == '_' && station[12] == '_' && station[16] == '_')  // CX_PB099__BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = current_station[6];
      stdstation[7]  = current_station[7];
      stdstation[8]  = '_';
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[13];
      stdstation[13] = current_station[14];
      stdstation[14] = current_station[15];
      stdstation[15] = '\0';
//      printf("stdstation:  %s\n",stdstation);
//      printf("%s %s\n",current_station,stdstation);
      encontrado++;      
    }         
    if(station[2] == '_' && station[8] == '_' && station[12] == '_' && station[16] == '_')  // CX_PB0___BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = current_station[6];
      stdstation[7]  = current_station[7];
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[13];
      stdstation[13] = current_station[14];
      stdstation[14] = current_station[15];
      stdstation[15] = '\0';
//      printf("%s %s\n",current_station,stdstation);
      encontrado++;      
    }
    

    if(station[2] == '_' && station[7] == '_' && station[10] == '_') // GE_SFJD_10_BHZ_D
    {
//      printf("LOC: %c%c\n",station[7],station[8]);
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = current_station[1];     // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[3];
      stdstation[4]  = current_station[4];
      stdstation[5]  = current_station[5];
      stdstation[6]  = current_station[6];
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = current_station[8];
      stdstation[10] = current_station[9];
      stdstation[11] = '_';
      stdstation[12] = current_station[11];
      stdstation[13] = current_station[12];
      stdstation[14] = current_station[13];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s   %s\n",current_station,stdstation);
      encontrado++;      
    }


    if(station[1] == '_' && station[5] == '_' && station[11] == '_')  // C_PB0___BHE_D 
    {
      stdstation[0]  = current_station[0];     // networkname
      stdstation[1]  = '_';                    // networkname
      stdstation[2]  = '_';
      stdstation[3]  = current_station[2];
      stdstation[4]  = current_station[3];
      stdstation[5]  = current_station[4];
      stdstation[6]  = '_';
      stdstation[7]  = '_';
      stdstation[8]  = '_';      
      stdstation[9]  = '0';
      stdstation[10] = '0';
      stdstation[11] = '_';
      stdstation[12] = current_station[8];
      stdstation[13] = current_station[9];
      stdstation[14] = current_station[10];
      stdstation[15] = '\0';
//      printf("%s\n",stdstation);
//      printf("%s   %s\n",current_station,stdstation);
      encontrado++;      
    }

   
    if(encontrado == 0)
      printf("%s                    NOT FOUND\n",current_station);
    

    
  }else{
    stdstation[0]  ='X';
    stdstation[1]  ='X';
    stdstation[2]  ='_';
    stdstation[3]  = current_station[1];
    stdstation[4]  = current_station[2];
    stdstation[5]  = current_station[3];
    stdstation[6]  = current_station[4];
    stdstation[7]  = current_station[5];
    stdstation[8]  ='_';
    stdstation[9]  ='0';
    stdstation[10] ='0';
    stdstation[11] ='_';
    tegn=0;
    for(i=0;i<5;i++)
    {
      if(current_station[i+1] != '_')
        tegn++;
    }
    switch(tegn)
    {
      case 3:
      stdstation[12]  = current_station[6];
      stdstation[13]  = current_station[7];
      stdstation[14]  = current_station[8];
      stdstation[15]  ='\0';
      if(stdstation[14] == '_')
      {
        stdstation[14] = stdstation[13];
	stdstation[13] = '_';
      }
      break;
      case 4:
      stdstation[12]  = current_station[7];
      stdstation[13]  = current_station[8];
      stdstation[14]  = current_station[9];
      stdstation[15]  ='\0';
      if(stdstation[14] == '_')
      {
        stdstation[14] = stdstation[13];
	stdstation[13] = '_';
      }
      break;
      case 5:
      stdstation[12]  = current_station[8];
      stdstation[13]  = current_station[9];
      stdstation[14]  = current_station[10];
      stdstation[15]  ='\0';
      if(stdstation[14] == '_')
      {
        stdstation[14] = stdstation[13];
	stdstation[13] = '_';
      }
      break;
    }
  }
//--------------------------------------------------------------
// process this component for phases
//--------------------------------------------------------------
//printf("teller: %3d process station: %s\n",teller,stdstation);
      if(en_to == 2)
      {
//	printf("process\n");
        process_component(cmp,stdstation,ns,sr_first,timestampcpy,value);
      cmp++;
      }else{
        sprintf(ALLCOMP[cmp],"%s",stdstation);
	cmp++;
      }
}
//exit(0);





      return(cmp);
}

//----------------------------------------------------------------------------------
// process each component for phases
//----------------------------------------------------------------------------------
void process_component(int cmp,char station[],int nsamp,int sr,char timestamp[],float value[])
{

  float env[MAXSMP];  
  int readings;
  int    i,j;
  float  dc;
  float  dt;
  float  srate;
  float  eqavst;
  int    orden;
  double win1;
  double win2;
  double startmsec;
  char   stname[30];
  float  fl;
  float  fh;
  int    ch;
  int    cod;
  int    pp;
  int    ss;
  float  cutoff  = 5.0;   // start value to register a pick
  int    nenvfil = 50;
  float  hanfil  = 0.33;
  float  sta     = 0.1;
  float  lta     = 1.5;
  int    npks;
  float  tstart  = 0.0;
  float  MxAmp;
  int    pixA;
  int    pixB;
  int    minval;
  int    maxval;
  int    pn;
  int    sx;
  int    wind;
  int    pindex;
  int    lindex;
  float  diff;
  float  diffs;
  int    nep;
  int    ws=5;                  // sta in samples
  int    wl=200;                // lta in samples
  double ms1,ms2,ms3,ms4,ms5;
  char   t1[30];
  char   t2[30];
  char   t3[30];
  char   t4[30];
  char   t5[30];
  char   dm[200];
  float  d1,d2,d3,d4,d5;
  char   cmpnavn[20];
  int    cmpno;    
  ch=0;
  fl=2.0;
  fh=8.0;
  

  
/*************************************************************************************/
/*     PICKER                                                                        */
/*************************************************************************************/

  int    YEAR;
  int    DOY;
  int    HOUR;
  int    MINU;
  int    iSEC;
  int    MSEC;
  char   dummy[200];
  int    n;
  int    space;
    
  BOOLEAN_INT useMemory = TRUE_INT; 

  double longTermWindow = 10.0; // NOTE: auto set below
    
  double threshold1 = Picker_TH1;
  double threshold2 = Picker_TH2;
    
  double tUpEvent = 0.5; // NOTE: auto set below
  double filterWindow = 4.0; // NOTE: auto set below
    //
    // auto set values
    // get dt
  double dtt = 1.0/(float)sr;
    
  filterWindow = 300.0 * dtt;
  filterWindow = Picker_FW  * dtt;    
    
  long iFilterWindow = (long) (0.5 + filterWindow * 1000.0);
  if (iFilterWindow > 1)
    filterWindow = (double) iFilterWindow / 1000.0;
    //
  longTermWindow = 500.0 * dtt; // seconds
  longTermWindow = Picker_LTW * dtt; // seconds    
    
  long ilongTermWindow = (long) (0.5 + longTermWindow * 1000.0);
  if (ilongTermWindow > 1)
    longTermWindow = (double) ilongTermWindow / 1000.0;
    //
    //tUpEvent = 10.0 * dt;   // time window to take integral of charFunct version
    
  tUpEvent = 20.0 * dtt; // AJL20090522
  tUpEvent = Picker_TUP * dtt;    
    
  long itUpEvent = (long) (0.5 + tUpEvent * 1000.0);
  if (itUpEvent > 1)
    tUpEvent = (double) itUpEvent / 1000.0;
/*
    printf("dt            : %f\n",dtt);
    printf("filterWindow  : %f\n",filterWindow);
    printf("longTermWindow: %f\n",longTermWindow);
    printf("threshold1    : %f\n",threshold1);
    printf("threshold2    : %f\n",threshold2);
    printf("tUpEvent      : %f\n",tUpEvent);
*/    
    // do picker function test
    // definitive pick data
  PickData** pick_list_definative = NULL;
  int    num_picks_definative = 0;
    // persistent memory
  FilterPicker5_Memory* mem = NULL;

  int    proc_samples = 0;
  int    read_samples = nsamp;
//    printf("read_samples: %4d\n",read_samples);
        // temporary data
  PickData** pick_list = NULL; // array of num_picks ptrs to PickData structures/objects containing returned picks
  int    num_picks = 0;

  Pick(
       dtt,
       value,
       read_samples,
       filterWindow,
       longTermWindow,
       threshold1,
       threshold2,
       tUpEvent,
       &mem,
       useMemory,
       &pick_list,
       &num_picks,
       "TEST"
      );
//        printf("num_picks: %d\n",num_picks);    

        // save pick data
  for (n = 0; n < num_picks; n++) 
  {
    PickData* pick = *(pick_list + n);
    pick->indices[0] += proc_samples; // pick indices returned are relative to start of packet
    pick->indices[1] += proc_samples;
    addPickToPickList(pick, &pick_list_definative, &num_picks_definative);
  }
        // clean up temporary data
  free(pick_list); // do not use free_PickList() since we want to keep PickData objects

  proc_samples += read_samples;

//printf("timestamp: %s\n",timestamp);

  int    month, day;

//                2013-03-02T14:51:51.419539
  sscanf(timestamp,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%2d%1c%3d",&YEAR,dummy,&month,dummy,&day,dummy,&HOUR,dummy,&MINU,dummy,&iSEC,dummy,&MSEC);

  STARTMSECS(&startmsec,timestamp);
//printf("YEAR: %d month: %d day: %d HOUR: %d MINUTE: %d SEC: %d MSEC: %d\n",YEAR,month,day,HOUR,MINU,SEC,MSEC);
//    MonthDay(YEAR, DOY, &month, &day);
//printf("month: %d day: %d\n",month,day);

//    double sec = (double) sachdr.B + (double) SEC + (double) MSEC / 1000.0;
  double sec = (double) iSEC + (double) MSEC / 1000.0;
    // id fields
  char   onset[] = "?";
  char*  kstnm;
  kstnm = calloc(1, 16 * sizeof (char));
  sprintf(kstnm, "STAT");
  char* kinst;
  kinst = calloc(1, 16 * sizeof (char));
  sprintf(kinst, "DIG");
  if (strstr(kinst, "(count") != NULL)
    strcpy(kinst, "(counts)");
  char*  kcmpnm;
  kcmpnm = calloc(1, 16 * sizeof (char));
  sprintf(kcmpnm, "CMP");
  char   phase[16];
    // create NLL picks
  char* pick_str;
  pick_str = calloc(1, 1024 * sizeof (char));
  for (n = 0; n < num_picks_definative; n++) 
  {
    sprintf(phase, "P%d_", n);
    pick_str = printNlloc(pick_str,
          *(pick_list_definative + n), dtt, kstnm, kinst, kcmpnm, onset, phase,
          YEAR, month, day, HOUR, MINU, sec);
        // write pick to <pick_file> in NLLOC_OBS format
//        fprintf(fp, "%s\n", pick_str);
//        printf("%s\n",pick_str);
    sprintf(PICKLINES[n],"%s",pick_str);
  }

//  printf("num_picks_definative: %d\n",num_picks_definative);
  for(n=0;n<num_picks_definative; n++)
  {
//      printf("%2d %2d %s PICKLINES: %s\n",allpick_ix,n,station,PICKLINES[n]);
    sprintf(ALLPICKS[allpick_ix],"%2d %s %s\n",n,station,PICKLINES[n]);
    allpick_ix++;
//      printf("%c\n",PICKLINES[n][26]);
  }

  for(n=0;n<200;n++)
    PICKTIMES[cmp][n] = '\0';
    
  sprintf(PICKTIMES[cmp],"%3d",num_picks_definative);

    
  if(num_picks_definative > 0)
  {
    for(n=0;n<num_picks_definative; n++)
    {
      space=n * 21 +1 + 3 + n;
      if(n == 0)
        PICKTIMES[cmp][space-1] = ' ';

      if(PICKLINES[n][34]==' ')PICKLINES[n][34]='0';
      PICKTIMES[cmp][space] = PICKLINES[n][34];       // DAY
      if(PICKLINES[n][35]==' ')PICKLINES[n][35]='0';        
      PICKTIMES[cmp][space +  1] = PICKLINES[n][35];  // DAY
      PICKTIMES[cmp][space +  2] = '/';
      if(PICKLINES[n][32]==' ')PICKLINES[n][32]='0';
      PICKTIMES[cmp][space +  3] = PICKLINES[n][32];  // MONTH
      PICKTIMES[cmp][space +  4] = PICKLINES[n][33];  // MONTH
      PICKTIMES[cmp][space +  5] = '/';
      PICKTIMES[cmp][space +  6] = PICKLINES[n][30];  // YEAR
      PICKTIMES[cmp][space +  7] = PICKLINES[n][31];  // YEAR
      PICKTIMES[cmp][space +  8] = ' ';
      if(PICKLINES[n][37]==' ')PICKLINES[n][37]='0';
      PICKTIMES[cmp][space +  9] = PICKLINES[n][37];  // HOUR
      PICKTIMES[cmp][space + 10] = PICKLINES[n][38];  // HOUR
      PICKTIMES[cmp][space + 11] = ':';
      if(PICKLINES[n][39]==' ')PICKLINES[n][39]='0';
      PICKTIMES[cmp][space + 12] = PICKLINES[n][39];  // MINUTE
      PICKTIMES[cmp][space + 13] = PICKLINES[n][40];  // MINUTE
      PICKTIMES[cmp][space + 14] = ':';
      if(PICKLINES[n][44]==' ')PICKLINES[n][44]='0';
      PICKTIMES[cmp][space + 15] = PICKLINES[n][44];  // SEC.MSEC
      PICKTIMES[cmp][space + 16] = PICKLINES[n][45];  // SEC.MSEC
      PICKTIMES[cmp][space + 17] = PICKLINES[n][46];  // SEC.MSEC
      if(PICKLINES[n][47]==' ')PICKLINES[n][47]='0';
      PICKTIMES[cmp][space + 18] = PICKLINES[n][47];  // SEC.MSEC
      if(PICKLINES[n][48]==' ')PICKLINES[n][48]='0';
      PICKTIMES[cmp][space + 19] = PICKLINES[n][48];  // SEC.MSEC
      PICKTIMES[cmp][space + 20] = PICKLINES[n][49];  // SEC.MSEC
      PICKTIMES[cmp][space + 21] = PICKLINES[n][26];  // polarization                 
        
    }
//      printf("PICKTIMES: %s %s\n",station,PICKTIMES[cmp]);
  }

    // clean up
//    fclose(fp);
  free(pick_str);
  free(kcmpnm);
  free(kinst);
  free(kstnm);
  free_PickList(pick_list_definative, num_picks_definative); // PickData objects freed here
  free_FilterPicker5_Memory(&mem);	
/*************************************************************************************/
/*     End of PICKER                                                                 */
/*************************************************************************************/
//printf("%s\n",PICKTIMES[cmp]);
  sprintf(STATION_NAMES[cmp],"%s",station);
//printf("process_component STATION_NAMES: %s\n",STATION_NAMES[cmp]);  
  sprintf(TIME_START[cmp],"%s",timestamp);
  TIME_START[cmp][10] = ' ';
        
  for(i=0;i<MAXSMP;i++)
    fseismo[i]=0;
    
  srates[cmp]=sr;
  dc=DC(nsamp,value);                          // find dc for raw data
  if(debug == 1)
    printf("%s  dc: %5.2f\n",station,dc);
  
  for(i=0;i<nsamp;i++)                         // remove dc
    value[i] = value[i] - dc;
  for(i=0;i<nsamp;i++)                         // save raw data, dc removed in matrix vraw
    vraw[cmp][i]=value[i];
  nusmp[cmp]=nsamp;                            // save nsamp per channel
  for(i=0;i<nsamp;i++)                         // raw data dc removed into fseismo         
    fseismo[i] = vraw[cmp][i];
  dt = 1.0/(float)(sr);                        // samplerate in seconds

  eqavst=dt;                                   // samplerate for filter
  orden=4;                                     // filter order
  if(sr != srold)
  {    
    design(orden,"BP","BU",30.0,0.3,fl,fh,eqavst); // design filter coeficients 2.0-8.0 hz
    srold=sr;
  }
  for(j=0;j<(nsects*3+1);j++)
  {
    snc[cmp][j]=sn[j];
    sdc[cmp][j]=sd[j];
  }
  cset[cmp]=nsects;

  appl(cmp,nsamp);                             // filter one channel (fseismo)

  ZERO_START();                                // adjust start of signal after filtering

  for(i=0;i<nsamp;i++)
    vflt[cmp][i]=fseismo[i];                   // save filtered data in matrix vflt

  STALTAPHFL(nsamp, ws, wl);                   // STA/LTA ratios of fseismo, store in ratios+ltas

  station_comp(cmp);
   
/*
    printf("%s %s  %s\n",station,trg_tim_p[cmp],trg_tim_s[cmp]);
*/
  sscanf(PICKTIMES[cmp],"%d",&readings);
//    if(readings != 0)    
//    printf("cmp: %2d  readings: %2d\n",cmp,readings);

  sscanf(ALLCOMP[cmp],"%s %d",cmpnavn,&cmpno);
//printf("cmp: %d\n",cmpno);

  switch(readings)
  {
    case 0:
    break;
    case 1:
    sscanf(PICKTIMES[cmp],"%4c%21c",dm,t1);
    t1[21]='\0';
    printf("%s %s\n",station,t1);
    break;
      
    case 2:
    sscanf(PICKTIMES[cmp],"%4c%21c",dm,t1);
    t1[21]='\0';
    CNVMSECS(&ms1,t1);
    sscanf(PICKTIMES[cmp],"%26c%21c",dm,t2);
    t2[21] = '\0';
    CNVMSECS(&ms2,t2);
    d1=ms2 - ms1;
    printf("%s %s %5.2f\n",station,t1,d1);      
    break;
      
    case 3:
    sscanf(PICKTIMES[cmp],"%4c%21c",dm,t1);
    t1[21]='\0';
    CNVMSECS(&ms1,t1);
    sscanf(PICKTIMES[cmp],"%26c%21c",dm,t2);
    t2[21] = '\0';
    CNVMSECS(&ms2,t2);
    sscanf(PICKTIMES[cmp],"%48c%21c",dm,t3);
    t3[21] = '\0';
    CNVMSECS(&ms3,t3);      
    d1 = ms2 - ms1;
    d2 = ms3 - ms1;
    printf("%s %s %5.2f %5.2f\n",station,t1,d1,d2);
    break;
      
    case 4:
    sscanf(PICKTIMES[cmp],"%4c%21c",dm,t1);
    t1[21]='\0';
    CNVMSECS(&ms1,t1);
    sscanf(PICKTIMES[cmp],"%26c%21c",dm,t2);
    t2[21] = '\0';
    CNVMSECS(&ms2,t2);
    sscanf(PICKTIMES[cmp],"%48c%21c",dm,t3);
    t3[21] = '\0';
    CNVMSECS(&ms3,t3);      
    sscanf(PICKTIMES[cmp],"%70c%21c",dm,t4);
    t4[21] = '\0';
    CNVMSECS(&ms4,t4);
    d1 = ms2 - ms1;
    d2 = ms3 - ms1; 
    d3 = ms4 - ms1;
    printf("%s %s %5.2f %5.2f %5.2f\n",station,t1,d1,d2,d3);      
    break;
  }
    
    
  switch(readings)
  {
    case 0:
    lindex = phase_picks_0(cmp,readings);
    break;
    case 1:
    lindex = phase_picks_1(cmp,readings,startmsec,sr);      
    break;
    case 2:
    if(cmpno == 3)
    {
      lindex = phase_picks_2(cmp,readings,startmsec,sr);
    }else{
      lindex = phase_picks_encomp(cmp,readings,startmsec,sr,cmpno);
    }
    break;
    case 3:
    if(cmpno == 3)
    {
      lindex = phase_picks_2(cmp,readings,startmsec,sr);
    }else{
      lindex = phase_picks_encomp(cmp,readings,startmsec,sr,cmpno);
    }
    break;
    case 4:
    if(cmpno == 3)
    {
      lindex = phase_picks_2(cmp,readings,startmsec,sr);
    }else{
      lindex = phase_picks_encomp(cmp,readings,startmsec,sr,cmpno);
    }
    break;
    case 5:
    if(cmpno == 3)
    {
      lindex = phase_picks_2(cmp,readings,startmsec,sr);
    }else{
      lindex = phase_picks_encomp(cmp,readings,startmsec,sr,cmpno);
    }
    break;
    case 6:
    if(cmpno == 3)
    {
      lindex = phase_picks_2(cmp,readings,startmsec,sr);
    }else{
      lindex = phase_picks_encomp(cmp,readings,startmsec,sr,cmpno);
    }
    break;
    default:
    lindex = phase_picks_2(cmp,readings,startmsec,sr);
	
  }
        
  if(SETCODA == 1)
  {
    if(lindex >= 0 && DistInd[21] != 'D')                     // only compute coda if index found and not a D(istant) event
    {
      cod = CODA_N(lindex,ltas,nsamp);                        // find coda in ltas after s-phase    
      if((int)((float)(cod - lindex) * dt) > 25.0)
      {
        varighet[cmp] = (int)((float)(cod - lindex) * dt);    
      }
    }
  }   
}
//----------------phase_picks_0--------------------------------
// No phases found by picker, write blank string for p and s
//----------------------------------------------------------
int phase_picks_0(int cmp,int readings)
{
  sprintf(TRIGGER_TIMES_P[cmp],"                         ");
  sprintf(TRIGGER_TIMES_S[cmp],"                         ");
  return(-1);
}

//----------------phase_picks_1--------------------------------
// 1 phase found by picker, if z component, use it as p
//----------------------------------------------------------
int phase_picks_1(int cmp,int readings,double startmsec,int sr)
{
  char dm[200];
  char temptid1[200];
  double msec1;
  int lindex = -1;
  sprintf(TRIGGER_TIMES_P[cmp],"                         ");
  sprintf(TRIGGER_TIMES_S[cmp],"                         ");
  if(trig_components[cmp][2] == 'Z')                            //--------VERTICAL----------
  {
    sscanf(PICKTIMES[cmp],"%4c%21c",dm,temptid1);
    temptid1[21]='\0';
    sprintf(TRIGGER_TIMES_P[cmp],"%s",temptid1);
    sprintf(TRIGGER_TIMES_S[cmp],"                         ");

    CNVMSECS(&msec1,temptid1);
    lindex = (int)(msec1-startmsec)*sr;

    if(PICKTIMES[cmp][25] == '+') POLARIZATION[cmp] = 'C';
    if(PICKTIMES[cmp][25] == '-') POLARIZATION[cmp] = 'D';
    if(PICKTIMES[cmp][25] == '?') POLARIZATION[cmp] = ' ';
 
//printf("TEST1: %s\n",PICKTIMES[cmp]); 
 
//	  printf("AA %c %x %c\n",PICKTIMES[cmp][25],POLARIZATION[cmp],POLARIZATION[cmp]); 
  }
  return(lindex);
}

//----------------phase_picks_2--------------------------------
// 2 phases found by picker, if z component, use 1 as p and 2 as s
//                           if horizontal, use 2 as s
//----------------------------------------------------------
int phase_picks_2(int cmp,int readings,double startmsec,int sr)
{
  char dm[200];
  char temptid1[200];
  char temptid2[200];
  char pick1[100];
  char pick2[100];
  char pick3[100];
  double msec1;
  double msec2;
  double msec3;
  double diff;
  int    lindex = -1;
  sprintf(TRIGGER_TIMES_P[cmp],"                         ");
  sprintf(TRIGGER_TIMES_S[cmp],"                         ");
  if(trig_components[cmp][2] == 'Z')                            //--------VERTICAL----------
  {
    sscanf(PICKTIMES[cmp],"%4c%21c",dm,temptid1);
    temptid1[21]='\0';
    sprintf(TRIGGER_TIMES_P[cmp],"%s",temptid1);
    sprintf(TRIGGER_TIMES_S[cmp],"                         ");    
    
    CNVMSECS(&msec1,temptid1);
    lindex = (int)(msec1-startmsec)*sr;    

    if(PICKTIMES[cmp][25] == '+') POLARIZATION[cmp] = 'C';
    if(PICKTIMES[cmp][25] == '-') POLARIZATION[cmp] = 'D';
    if(PICKTIMES[cmp][25] == '?') POLARIZATION[cmp] = ' ';  
//printf("TEST2: %s\n",PICKTIMES[cmp]);     
//printf("BB %x %c\n",POLARIZATION[cmp],POLARIZATION[cmp]);  

//printf("\n");
    
  }else{                                                        //--------HORIZONTAL--------------
    switch(readings)
    {
      case 2:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%26c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
  
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
   
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
//        printf("Time difference 1. and 2. S less than: %8.2f seconds. Discard both.\n",diff);

        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;

//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
//        printf("Difference 1. and 2. S-phase: %6.2f   > %6.2f seconds. Select 2.\n",diff,MINDIFF);
//printf(" Select 2.\n");
        sscanf(PICKTIMES[cmp],"%26c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      break;
      case 3:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%48c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
      
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%48c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{        
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      case 4:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%70c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
   
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%70c%21c",dm,temptid2);
        temptid2[21]='\0';
         if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{       
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      case 5:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%92c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
      
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%92c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{        
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      case 6:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%114c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
      
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%114c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{        
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      break;      
    }


  }
  return(lindex);
}




//----------------phase_picks_2--------------------------------
// 2 phases found by picker, if z component, use 1 as p and 2 as s
//                           if horizontal, use 2 as s
//----------------------------------------------------------
int phase_picks_encomp(int cmp,int readings,double startmsec,int sr,int cmpno)
{
  char dm[200];
  char temptid1[200];
  char temptid2[200];
  char pick1[100];
  char pick2[100];
  char pick3[100];
  double msec1;
  double msec2;
  double msec3;
  double diff;
  int    lindex = -1;
//  printf("encomp readings: %d\n",readings);
  sprintf(TRIGGER_TIMES_P[cmp],"                         ");
  sprintf(TRIGGER_TIMES_S[cmp],"                         ");
  if(trig_components[cmp][2] == 'Z')                            //--------VERTICAL----------
  {
//    printf("Z\n");
    sscanf(PICKTIMES[cmp],"%4c%21c",dm,temptid1);
    temptid1[21]='\0';
    sprintf(TRIGGER_TIMES_P[cmp],"%s",temptid1);
//    printf("P %s\n",temptid1);
    sprintf(TRIGGER_TIMES_S[cmp],"                         ");    
    
    CNVMSECS(&msec1,temptid1);
    lindex = (int)(msec1-startmsec)*sr;    

    if(PICKTIMES[cmp][25] == '+') POLARIZATION[cmp] = 'C';
    if(PICKTIMES[cmp][25] == '-') POLARIZATION[cmp] = 'D';
    if(PICKTIMES[cmp][25] == '?') POLARIZATION[cmp] = ' ';  
//printf("TEST2: %s\n",PICKTIMES[cmp]);     
//printf("BB %x %c\n",POLARIZATION[cmp],POLARIZATION[cmp]);  

//printf("\n");
    
                                                          //--------HORIZONTAL--------------
    switch(readings)
    {
      case 2:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%26c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
  
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
   
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
//        printf("Time difference 1. and 2. S less than: %8.2f seconds. Discard both.\n",diff);

//        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;

//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
//        printf("Difference 1. and 2. S-phase: %6.2f   > %6.2f seconds. Select 2.\n",diff,MINDIFF);
//printf(" Select 2.\n");
        sscanf(PICKTIMES[cmp],"%26c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
//          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
//          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      break;
      case 3:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%48c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
      
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
//        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%48c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
//          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{        
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
//          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      case 4:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%70c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
   
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%70c%21c",dm,temptid2);
        temptid2[21]='\0';
         if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
//          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{  
//	  printf("here  %s\n",temptid2);
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
//          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      case 5:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%92c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
      
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
//        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%92c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
//          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{        
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
//          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      case 6:
      sscanf(PICKTIMES[cmp],"%4c%21c",dm,pick1);
      pick1[21] = '\0';
      CNVMSECS(&msec1,pick1);
      sscanf(PICKTIMES[cmp],"%114c%21c",dm,pick2);
      pick2[21] = '\0';
      CNVMSECS(&msec2,pick2);
      
//      printf("DIFFERENCE: %8.2f  %8.2f ix: %d  %d\n",msec1-startmsec,msec2-startmsec,(int)((msec1-startmsec)*sr),(int)((msec2-startmsec)*sr));
//      lindex = (int)((msec1-startmsec)*sr);
      
      diff = msec2 - msec1;
      if(diff < MINDIFF)                                  // diff < 1 sec. 
      {
        printf("***** %8.2f *****\n",diff);
//        sprintf(TRIGGER_TIMES_P[cmp],"                         ");
        sprintf(TRIGGER_TIMES_S[cmp],"                         ");
	lindex = -1;
//      printf("%s %12.3f  %s %12.3f  %8.3f\n",pick1,msec1,pick2,msec2,msec2-msec1); 
      }else{                                           // diff > 1 sec. phase 2 probably S
        sscanf(PICKTIMES[cmp],"%114c%21c",dm,temptid2);
        temptid2[21]='\0';
        if(DistInd[21] == 'D') // if distant, drop s-picks
	{
	  printf("DISTANT, no S-picks.\n");
//          sprintf(TRIGGER_TIMES_P[cmp],"                         ");
          sprintf(TRIGGER_TIMES_S[cmp],"                         ");  
	}else{        
          sprintf(TRIGGER_TIMES_S[cmp],"%s",temptid2);
//          sprintf(TRIGGER_TIMES_P[cmp],"                         "); 
	}
      }
      break;      
    }

  }
  
  return(lindex);
}







void CNVMSECS(double *win1,char datetime[])
{
  char  DM[100];
  int   PYEAR;
  int   PMONT;
  int   PDAYS;
  int   PHOUR;
  int   PMINU;  
  float PSECO;
  long  LPYE;
  long  LPMO;
  long  LPHR;
  long  LPDA;
  long  LPMI;
  double PMSECS;

  sscanf(datetime,"%2d%c%2d%c%2d%c%2d%c%2d%c%f",&PDAYS,DM,&PMONT,DM,&PYEAR,DM,&PHOUR,DM,&PMINU,DM,&PSECO);
  LPYE = PYEAR;
  LPMO = PMONT;
  LPDA = PDAYS;
  LPHR = PHOUR;
  LPMI = PMINU;
  TIMSEC(LPYE,LPMO,LPDA,LPHR,LPMI,PSECO,&PMSECS);
  *win1 = PMSECS;
  
}
void STARTMSECS(double *win1,char datetime[])
{
  char  DM[100];
  int   PYEAR;
  int   PMONT;
  int   PDAYS;
  int   PHOUR;
  int   PMINU;  
  float PSECO;
  long  LPYE;
  long  LPMO;
  long  LPHR;
  long  LPDA;
  long  LPMI;
  double PMSECS;
//  printf("%s\n",datetime);
  
  sscanf(datetime,"%4d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f",&PYEAR,DM,&PMONT,DM,&PDAYS,DM,&PHOUR,DM,&PMINU,DM,&PSECO);

  LPYE = PYEAR;
  LPMO = PMONT;
  LPDA = PDAYS;
  LPHR = PHOUR;
  LPMI = PMINU;
  TIMSEC(LPYE,LPMO,LPDA,LPHR,LPMI,PSECO,&PMSECS);
  *win1 = PMSECS;
  
}

void station_comp(int cmp)
{
  int k;
  int nn  = 0;
  int tel = 0;
  char comp[10];

  for(k=3;k<10;k++)
  {
    if(STATION_NAMES[cmp][k] == '_')
    {
      break;
    }else{
      tel++;
    }
  }
  nn=tel;                      // station name length
//printf("nn: %d\n",nn);    
  for(k=0;k<10;k++)
    comp[k]='\0';
  for(k=0;k<nn;k++)
    comp[k]= STATION_NAMES[cmp][k+3];
  sprintf(trig_stations[cmp],"%s",comp);
//printf("trig_stations: %s\n",trig_stations[i]);
  for(k=0;k<10;k++)
    comp[k]='\0';
  switch(nn)
  {
    case 3:
    for(k=0;k<3;k++)
      comp[k]= STATION_NAMES[cmp][k+12];
    sprintf(trig_components[cmp],"%s",comp);
    break;      
    case 4:
    for(k=0;k<3;k++)
      comp[k]= STATION_NAMES[cmp][k+12];
    sprintf(trig_components[cmp],"%s",comp);
    break;
    case 5:
    for(k=0;k<3;k++)
      comp[k]= STATION_NAMES[cmp][k+12];
    sprintf(trig_components[cmp],"%s",comp);	
    break;
  }
//printf("trig_components: %s\n",trig_components[i]);
    
}
//--------------------------------------------------------------------
// compute dc-value of float array with nsamp samples
//--------------------------------------------------------------------
int DC(int nsamp,float value[])
{
  int k;
  float dc;
  dc=0;
  for(k=0;k<nsamp;k++)      // add all samples in buffer
    dc = dc + value[k];
  dc = dc/(float)nsamp;    // dc of nsamp samples
  return(dc);
}

/*  Subroutine to design IIR digital filters from analog prototypes. */
/*  Input Arguments: */
/*  ---------------- */
/*    IORD                Filter order (10 MAXIMUM) */
/*    TYPE                Character*2 variable containing filter type */
/*                          LOWPASS (LP) */
/*                          HIGHPASS (HP) */
/*                          BANDPASS (BP) */
/*                          BANDREJECT (BR) */
/*   APROTO              Character*2 variable designating analog prototype*/
/*                          Butterworth (BU) */
/*                          Bessel (BE) */
/*                          Chebyshev Type I (C1) */
/*                          Chebyshev Type II (C2) */
/*    A                   Chebyshev stopband attenuation factor */
/*    TRBNDW              Chebyshev transition bandwidth (fraction of */
/*                          lowpass prototype passband width) */
/*    FL                  Low-frequency cutoff */
/*    FH                  High-frequency cutoff */
/*    TS                  Sampling interval (in seconds) */
/*  Output Arguments: */
/*  ----------------- */
/*    SN                  Array containing numerator coefficients of */
/*                        second-order sections packed head-to-tail. */
/*    SD                  Array containing denominator coefficients */
/*                        of second-order sections packed head-to-tail. */

void design( iord, type,aproto,  a,trbndw,  fl,  fh,  ts)
int iord;
char *type;
char *aproto;
float a;
float trbndw;
float fl;
float fh;
float ts;
{
    /* System generated locals */
    float r__1;

    /* Local variables */
    complex p[10], z[10];
    char stype[3*10];
    float omegar, ripple;
    float fhw, eps, flw, dcvalue;

    /*  Analog prototype selection */

    if (strncmp(aproto, "BU", 2) == 0) {
    buroots(p, stype, &dcvalue, iord);
    }


    /*  Analog mapping selection */

    if (strncmp(type, "BP", 2) == 0) {
    r__1 = fl * ts / (float)2.;
    flw = warp(&r__1, &c_b12);
    r__1 = fh * ts / (float)2.;
    fhw = warp(&r__1, &c_b12);
    lptbp(p, z, stype, &dcvalue, &flw, &fhw);
    }


    /*  Bilinear analog to digital transformation */
    bilin2(&sn[1], &sd[1]);

    return;
}

/* BUROOTS -- SUBROUTINE TO COMPUTE BUTTERWORTH POLES FOR */
/*   NORMALIZED LOWPASS FILTER */
/* LAST MODIFIED:  SEPTEMBER 7, 1990 */
/*  OUTPUT ARGUMENTS: */
/*  ----------------- */
/*      P              COMPLEX ARRAY CONTAINING POLES */
/*                       CONTAINS ONLY ONE FROM EACH */
/*                       COMPLEX CONJUGATE PAIR, AND */
/*                       ALL REAL POLES */
/*      RTYPE          CHARACTER ARRAY INDICATING 2ND ORDER SECTION */
/*                       TYPE: */
/*                         (SP)  SINGLE REAL POLE */
/*                         (CP)  COMPLEX CONJUGATE POLE PAIR */
/*                         (CPZ) COMPLEX CONJUGATE POLE-ZERO PAIRS */
/*      DCVALUE        MAGNITUDE OF FILTER AT ZERO FREQUENCY */
/*  INPUT ARGUMENTS: */
/*  ---------------- */
/*      IORD           DESIRED FILTER ORDER */
int buroots(p,rtype, dcvalue, iord)
complex *p;
char *rtype;
float *dcvalue;
int iord;
{
    /* System generated locals */
    int i__1, i__2;
    double d__1, d__2;
    complex q__1;

    /* Local variables */
    int half, k;
    float angle, pi;

    /* Parameter adjustments */
    rtype -= 3;
    --p;

    /* Function Body */
    pi = (float)3.14159265;

    half = iord / 2;

    /* TEST FOR ODD ORDER, AND ADD POLE AT -1 */

    nsects = 0;
    if (half << 1 < iord) {
    p[1].r = (float)-1., p[1].i = (float)0.;
    strncpy(rtype + 3, "SP", 2);
    nsects = 1;
    }
    i__1 = half;
    for (k = 1; k <= i__1; ++k) {
    angle = pi * ((float) ((k << 1) - 1)/(float) (iord << 1)+(float).5);
    ++(nsects);
    i__2 = nsects;
    d__1 = cos(angle);
    d__2 = sin(angle);
    q__1.r = d__1, q__1.i = d__2;
    p[i__2].r = q__1.r, p[i__2].i = q__1.i;
        strncpy(rtype + nsects * 3, "CP", 2);
    }
    *dcvalue = (float)1.;
    return TRUE;
}
/* WARP -- FUNCTION, APPLIES TANGENT FREQUENCY WARPING TO COMPENSATE */
/*         FOR BILINEAR ANALOG -> DIGITAL TRANSFORMATION */
/* ARGUMENTS: */
/* ---------- */
/*      F       ORIGINAL DESIGN FREQUENCY SPECIFICATION (HERTZ) */
/*      TS      SAMPLING INTERVAL (SECONDS) */
/*  LAST MODIFIED:  SEPTEMBER 20, 1990 */

static double warp(f, ts)
float *f, *ts;
{
    /* System generated locals */
    float ret_val;

    /* Local variables */
    float angle, twopi;

    twopi = (float)6.2831853;
    angle = twopi * *f * *ts / (float)2.;
    ret_val = tan(angle) * (float)2. / *ts;
    ret_val /= twopi;
    return ret_val;
}
/* Subroutine to convert an prototype lowpass filter to a bandpass filter via
*/
/*    the analog polynomial transformation.  The lowpass filter is */
/*   described in terms of its poles and zeros (as input to this routine).*/
/*    The output consists of the parameters for second order sections. */
/*  Input Arguments: */
/*  ---------------- */
/*    P                       Array containing poles */
/*    Z                       Array containing zeros */
/*    RTYPE                   Character array containing type information */
/*                              (SP) single real pole  or */
/*                              (CP) complex conjugate pole pair  or */
/*                              (CPZ) complex conjugate pole/zero pairs */
/*    DCVALUE                 Zero frequency value of filter */
/*    FL                      Low-frequency cutoff */
/*    FH                      High-frequency cutoff */
/*  Output Arguments: */
/*  ----------------- */
/*    SN                      Numerator polynomials for second order */
/*                              sections. */
/*    SD                      Denominator polynomials for second order */
/*                              sections. */
/*                              This subroutine doubles the number of */
/*                              sections. */

static int lptbp(p, z, rtype, dcvalue, fl, fh)
complex *p, *z;
char *rtype;
float *dcvalue;
float *fl, *fh;
{
    /* System generated locals */
    int i__1, i__2, i__3, i__4, i__5, i__6, i__7;
    double d__1;
    complex q__1, q__2, q__3, q__4, q__5, q__6, q__7, q__8, q__9, q__10;

    /* Local variables */
    int iptr;
    float a, b;
    complex h;
    int i, n;
    complex s;
    float scale;
    complex ctemp, p1, p2;
    float twopi;
    complex z1, z2;
    float pi;

    /* Parameter adjustments */
    rtype -= 3;
    --z;
    --p;

    /* Function Body */
    pi = (float)3.14159265;
    twopi = pi * (float)2.;
    a = twopi * twopi * *fl * *fh;
    b = twopi * (*fh - *fl);

    n = nsects;
    nsects = 0;
    iptr = 1;
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
    if (strncmp(rtype + i * 3, "CPZ", 3) == 0) {
        i__2 = i;
        q__3.r = b * z[i__2].r, q__3.i = b * z[i__2].i;
            q__2 = cpowi(q__3, 2);
        d__1 = a * (float)4.;
        q__1.r = q__2.r - d__1, q__1.i = q__2.i;
        ctemp.r = q__1.r, ctemp.i = q__1.i;
            q__1 = csqrt1(ctemp);
        ctemp.r = q__1.r, ctemp.i = q__1.i;
        i__2 = i;
        q__3.r = b * z[i__2].r, q__3.i = b * z[i__2].i;
        q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
        q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
        z1.r = q__1.r, z1.i = q__1.i;
        i__2 = i;
        q__3.r = b * z[i__2].r, q__3.i = b * z[i__2].i;
        q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
        q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
        z2.r = q__1.r, z2.i = q__1.i;
        i__2 = i;
        q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
            q__2 = cpowi(q__3, 2);
        d__1 = a * (float)4.;
        q__1.r = q__2.r - d__1, q__1.i = q__2.i;
        ctemp.r = q__1.r, ctemp.i = q__1.i;
            q__1 = csqrt1(ctemp);
        ctemp.r = q__1.r, ctemp.i = q__1.i;
        i__2 = i;
        q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
        q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
        q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
        p1.r = q__1.r, p1.i = q__1.i;
        i__2 = i;
        q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
        q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
        q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
        p2.r = q__1.r, p2.i = q__1.i;
            q__2 = conjg(z1);
        q__1.r = z1.r * q__2.r - z1.i * q__2.i, q__1.i = z1.r * q__2.i +
            z1.i * q__2.r;
        sn[iptr] = q__1.r;
        sn[iptr + 1] = z1.r * (float)-2.;
        sn[iptr + 2] = (float)1.;
            q__2 = conjg(p1);
        q__1.r = p1.r * q__2.r - p1.i * q__2.i, q__1.i = p1.r * q__2.i +
            p1.i * q__2.r;
        sd[iptr] = q__1.r;
        sd[iptr + 1] = p1.r * (float)-2.;
        sd[iptr + 2] = (float)1.;
        iptr += 3;
            q__2 = conjg(z2);
        q__1.r = z2.r * q__2.r - z2.i * q__2.i, q__1.i = z2.r * q__2.i +
            z2.i * q__2.r;
        sn[iptr] = q__1.r;
        sn[iptr + 1] = z2.r * (float)-2.;
        sn[iptr + 2] = (float)1.;
            q__2 = conjg(p2);
        q__1.r = p2.r * q__2.r - p2.i * q__2.i, q__1.i = p2.r * q__2.i +
            p2.i * q__2.r;
        sd[iptr] = q__1.r;
        sd[iptr + 1] = p2.r * (float)-2.;
        sd[iptr + 2] = (float)1.;
        iptr += 3;
        nsects += 2;
    } else if (strncmp(rtype + i * 3, "CP", 2) == 0) {
        i__2 = i;
        q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
            q__2 = cpowi(q__3, 2);
        d__1 = a * (float)4.;
        q__1.r = q__2.r - d__1, q__1.i = q__2.i;
        ctemp.r = q__1.r, ctemp.i = q__1.i;
            q__1 = csqrt1(ctemp);
        ctemp.r = q__1.r, ctemp.i = q__1.i;
        i__2 = i;
        q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
        q__2.r = q__3.r + ctemp.r, q__2.i = q__3.i + ctemp.i;
        q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
        p1.r = q__1.r, p1.i = q__1.i;
        i__2 = i;
        q__3.r = b * p[i__2].r, q__3.i = b * p[i__2].i;
        q__2.r = q__3.r - ctemp.r, q__2.i = q__3.i - ctemp.i;
        q__1.r = q__2.r * (float).5, q__1.i = q__2.i * (float).5;
        p2.r = q__1.r, p2.i = q__1.i;
        sn[iptr] = (float)0.;
        sn[iptr + 1] = b;
        sn[iptr + 2] = (float)0.;
            q__2 = conjg(p1);
        q__1.r = p1.r * q__2.r - p1.i * q__2.i, q__1.i = p1.r * q__2.i +
            p1.i * q__2.r;
        sd[iptr] = q__1.r;
        sd[iptr + 1] = p1.r * (float)-2.;
        sd[iptr + 2] = (float)1.;
        iptr += 3;
        sn[iptr] = (float)0.;
        sn[iptr + 1] = b;
        sn[iptr + 2] = (float)0.;
            q__2 = conjg(p2);
        q__1.r = p2.r * q__2.r - p2.i * q__2.i, q__1.i = p2.r * q__2.i +
            p2.i * q__2.r;
        sd[iptr] = q__1.r;
        sd[iptr + 1] = p2.r * (float)-2.;
        sd[iptr + 2] = (float)1.;
        iptr += 3;
        nsects += 2;
    } else if (strncmp(rtype + i * 3, "SP", 2) == 0) {
        sn[iptr] = (float)0.;
        sn[iptr + 1] = b;
        sn[iptr + 2] = (float)0.;
        sd[iptr] = a;
        i__2 = i;
        sd[iptr + 1] = -(double)b * p[i__2].r;
        sd[iptr + 2] = (float)1.;
        iptr += 3;
        ++(nsects);
    }
    }
/* Scaling - use fact that the bandpass filter amplitude at sqrt( omega_l **/
/*            equals the amplitude of the lowpass prototype at d.c. */
    d__1 = sqrt(a);
    q__1.r = (float)0., q__1.i = d__1;
    s.r = q__1.r, s.i = q__1.i;
    h.r = (float)1., h.i = (float)0.;

    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {
    i__2 = iptr + 2;
    q__6.r = sn[i__2] * s.r, q__6.i = sn[i__2] * s.i;
    i__3 = iptr + 1;
    q__5.r = q__6.r + sn[i__3], q__5.i = q__6.i;
    q__4.r = q__5.r * s.r - q__5.i * s.i, q__4.i = q__5.r * s.i + q__5.i *
         s.r;
    i__4 = iptr;
    q__3.r = q__4.r + sn[i__4], q__3.i = q__4.i;
    q__2.r = h.r * q__3.r - h.i * q__3.i, q__2.i = h.r * q__3.i + h.i *
        q__3.r;
    i__5 = iptr + 2;
    q__10.r = sd[i__5] * s.r, q__10.i = sd[i__5] * s.i;
    i__6 = iptr + 1;
    q__9.r = q__10.r + sd[i__6], q__9.i = q__10.i;
    q__8.r = q__9.r * s.r - q__9.i * s.i, q__8.i = q__9.r * s.i + q__9.i *
         s.r;
    i__7 = iptr;
    q__7.r = q__8.r + sd[i__7], q__7.i = q__8.i;
        q__1 = cdiv(q__2, q__7);
    h.r = q__1.r, h.i = q__1.i;
    iptr += 3;
    }
    q__2.r = *dcvalue, q__2.i = (float)0.;
    d__1 = h.r;
    q__5 = conjg(h);
    q__4.r = d__1 * q__5.r, q__4.i = d__1 * q__5.i;
    q__3 = csqrt1(q__4);
    q__1 = cdiv(q__2, q__3);
    scale = q__1.r;
    sn[1] *= scale;
    sn[2] *= scale;
    sn[3] *= scale;
    return TRUE;
}

/* Transforms an analog filter to a digital filter via the bilinear transforma
ti*/
/*   Assumes both are stored as second order sections.  The transformation is
*/
/*    done in-place. */
/*  Input Arguments: */
/*  ---------------- */
/*   SN                   Array containing numerator polynomial coefficients f
or*/
/*                           second order sections.  Packed head-to-tail. */
/*   SD                   Array containing denominator polynomial coefficients
 f*/
/*                           second order sections.  Packed head-to-tail. */

static int bilin2(sn, sd)
float *sn, *sd;
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    int iptr, i;
    float scale, a0, a1, a2;

    /* Parameter adjustments */
    --sd;
    --sn;

    /* Function Body */
    iptr = 1;
    i__1 = nsects;
    for (i = 1; i <= i__1; ++i) {
    a0 = sd[iptr];
    a1 = sd[iptr + 1];
    a2 = sd[iptr + 2];
    scale = a2 + a1 + a0;
    sd[iptr] = (float)1.;
    sd[iptr + 1] = (a0 - a2) * (float)2. / scale;
    sd[iptr + 2] = (a2 - a1 + a0) / scale;
    a0 = sn[iptr];
    a1 = sn[iptr + 1];
    a2 = sn[iptr + 2];
    sn[iptr] = (a2 + a1 + a0) / scale;
    sn[iptr + 1] = (a0 - a2) * (float)2. / scale;
    sn[iptr + 2] = (a2 - a1 + a0) / scale;
    iptr += 3;
    }
    return TRUE;
}

static complex cmul(a,b)
complex a,b;
{
    complex c;
    c.r = a.r * b.r - a.i * b.i;
    c.i = a.i * b.r + a.r * b.i;
    return c;
}

static complex cpowi(a,n)
complex a;
int n;
{
    int i;
    complex c;
    c.i = 0.;
    c.r = 1.;
    for (i = 1; i <= n; ++i) {
    c = cmul(c, a);
    }
    return c;

}
static complex csqrt1(z)
complex z;
{   complex c;
    float x,y,w,r;
    if ((z.r == 0.0) && (z.i == 0.0)) {
        c.r=0.0;
        c.i=0.0;
        return c;
    } else {
        x=fabs(z.r);
        y=fabs(z.i);
        if (x >= y) {
            r=y/x;
            w=sqrt(x)*sqrt(0.5*(1.0+sqrt(1.0+r*r)));
        } else {
            r=x/y;
            w=sqrt(y)*sqrt(0.5*(r+sqrt(1.0+r*r)));
        }
        if (z.r >= 0.0) {
            c.r=w;
            c.i=z.i/(2.0*w);
        } else {
            c.i=(z.i >= 0) ? w : -w;
            c.r=z.i/(2.0*c.i);
        }
        return c;
    }
}

static complex conjg(z)
complex z;
{   complex c;
    c.r=z.r;
    c.i = -z.i;
    return c;
}

static complex cdiv(a,b)
complex a,b;
{   complex c;
    float r,den;
    if (fabs(b.r) >= fabs(b.i)) {
        r=b.i/b.r;
        den=b.r+r*b.i;
        c.r=(a.r+r*a.i)/den;
        c.i=(a.i-r*a.r)/den;
    } else {
        r=b.r/b.i;
        den=b.i+r*b.r;
        c.r=(a.r*r+a.i)/den;
        c.i=(a.i*r-a.r)/den;
    }
    return c;
}
/*  Subroutine to apply an iir filter to a data sequence.                   */
/*    The filter is assumed to be stored as second order sections.          */
/*    Filtering is in-place.                                                */
/*    Zero-phase (forward and reverse) is an option.                        */

//appl(sw,chan,nsamp,peker)
void appl(int fnd,int nsamp)
//int      sw;
//int      chan;
//int      nsamp;
//float    *peker;
{
//float    *tpek;
    /* System generated locals */
int      i1;
int      i2;

    /* Local variables */
int      jptr;
int      i;
int      j;
float    b0;
float    b1;
float    b2;
float    a1;
float    a2;
float    x1;
float    x2;
float    y1;
float    y2;
float    output;
//tpek=peker;

/* Parameter adjustments                                                    */
/*    --data;*/

/* Function Body                                                            */
jptr = 1;
i1 = cset[fnd];

for (j = 1; j <= i1; ++j)
{
    x1 = save_x1[fnd][j];
    x2 = save_x2[fnd][j];
    y1 = save_y1[fnd][j];
    y2 = save_y2[fnd][j];
    b0 = snc[fnd][jptr];
    b1 = snc[fnd][jptr + 1];
    b2 = snc[fnd][jptr + 2];
    a1 = sdc[fnd][jptr + 1];
    a2 = sdc[fnd][jptr + 2];
    i2 = nsamp;
//    i2 = ns[fnd];
//    peker=tpek;
    for (i = 0; i < i2; ++i)
    {
//      output = b0 * (*peker) + b1 * x1 + b2 * x2;
      output = b0 * fseismo[i] + b1 * x1 + b2 * x2;

      output -= a1 * y1 + a2 * y2;
      y2 = y1;
      y1 = output;
      x2 = x1;
      x1 = fseismo[i];
      fseismo[i] = output;
//      peker++;
    }

    jptr += 3;

    save_y2[fnd][j]=y2;
    save_y1[fnd][j]=y1;
    save_x2[fnd][j]=x2;
    save_x1[fnd][j]=x1;

}

}
//----------------------------------------------------------------------------
// first samples after filter set to real values
//----------------------------------------------------------------------------
void ZERO_START()
{
  int s;
  for (s=0;s<300;s++)
    fseismo[s]=fseismo[s+300];
}
//--------------------------------------------------------------------------------
// STALTAPHFL
// computes Ratio= STA/LTA stores in ratios and lta in ltas
//--------------------------------------------------------------------------------
void STALTAPHFL(int ne,int ws,int wl)
  {
    float extra=(float)0.0;
    float sta1=(float)0.0;
    float lta1=(float)0.0;
    float k1=(float)0.0;
    float k2=(float)0.0;
    float R=(float)0.0;
    int s,ss;
    sta1=(float)0.0;
    lta1=(float)0.0;
    
//      System.out.println("STALTAPHFL: ne:"+ne);

    for(s=1;s<800;s++)
    {
      extra=(float)fseismo[s]-(float)fseismo[s-1];
      extra=extra * extra;
      k1=fabsf((float)fseismo[s-1]);
      k2=k1 * k1;
      sta1=sta1+(k2+ (float)3.0*extra-sta1)/(float)ws;
      k1=fabsf((float)fseismo[s]);
      k2=k1 * k1;
      lta1=lta1+(k2-lta1)/(float)wl;
    }

    for(ss=1;ss<(ne-1);ss++)
    {
      extra=(float)fseismo[ss]-(float)fseismo[ss-1];
      extra=extra * extra;
      k1=fabsf((float)fseismo[ss-1]);
      k2=k1 * k1;
      sta1=sta1+(k2+ (float)3.0*extra-sta1)/(float)ws;
//      ssttaa[ss] = sta1;
      k1=fabsf((float)fseismo[ss]);
      k2=k1 * k1;
      lta1=lta1+(k2-lta1)/(float)wl;
      R=sta1/lta1;
      ratios[ss]=R;
      ltas[ss] = lta1;
    }

  }

//*=========================================================================      
//*                                                                               
//* Routine to convert from day-of-year to date                                   
//*                                                         Leif Kvamme 12-1-85   
void DTE (DOY,DAY,MON,YR)                                          
int DOY;
int *DAY;
int *MON;
int YR;
{
int MTH[14],J,M,N;
for(J=1;J<8;J=J+2)
    MTH[J] = 31;
for(J=8;J<13;J=J+2)
    MTH[J] = 31;
MTH[2] = 28;
MTH[4] = 30;
MTH[6] = 30;
MTH[9] = 30;
MTH[11]= 30;
if ((YR%4) == 0) MTH[2] = 29;
M = 0;                                                                    
for(J=1;J<13;J++)
{
    M = M + MTH[J];
    N = DOY - M;                                                              
    if (N <= 0)
    {                                                       
        *MON = J;
        *DAY = N + MTH[J];                                                     
        goto four;                                                               
    }
}
four:                                                                   
J=0;
}
void TIMSEC (YR,MTH,DAY,HR,MIN,SECS,MSECS)                         
int YR;
int MTH;
int DAY;
int HR;
int MIN;
float SECS;
double *MSECS;                         /* total seconds to be returned      */
{
int DYR;                               /* flag for leap year                */
int IYR;                               /* number of leap-years since 1900   */
int YDY;                               /* number of days in current year    */

if(YR <= 50) YR=YR+100;                /* check for 2000 or more in 2 digits*/

if(YR >= 1900) YR=YR - 1900;
DYR = 0;
if((YR % 4) == 0) DYR = 1;
IYR = YR/4 - DYR;
//                -- Seconds to beginning of     
*MSECS = (float)(IYR*366) + (float)((YR-IYR)*365);
//                -- current year                
*MSECS = *MSECS*86400.0;
//                -- January                     
if (MTH == 1) YDY = DAY;
//                -- February                    
if (MTH == 2) YDY = DAY + 31;
//                -- ....                        
if (MTH == 3) YDY = DAY + DYR + 59;       
if (MTH == 4) YDY = DAY + DYR + 90;                                     
if (MTH == 5) YDY = DAY + DYR + 120;                                    
if (MTH == 6) YDY = DAY + DYR + 151;                                    
if (MTH == 7) YDY = DAY + DYR + 181;                                    
if (MTH == 8) YDY = DAY + DYR + 212;                                    
if (MTH == 9) YDY = DAY + DYR + 243;                                    
if (MTH ==10) YDY = DAY + DYR + 273;                                    
if (MTH ==11) YDY = DAY + DYR + 304;                                    
if (MTH ==12) YDY = DAY + DYR + 334;                                    
*MSECS = *MSECS + (float)(YDY*86400 + HR*3600 + MIN*60) + SECS;
}
void SECTIM (MSECS,YR,DOY,MTH,DAY,HR,MIN,fSEC)                      
double MSECS;
int *YR;
int *DOY;
int *MTH;
int *DAY;
int *HR;
int *MIN;
float *fSEC;
{
int DDAY,MMTH;
//             -- Temporary seconds         
double SSEC;
//             -- Seconds per day           
double SECDAY;
//             -- Seconds per year          
double SECYR;
//              -- Leap-year indicators      
float  IND, SIN;
//              -- Counter                   
int    I;
SECDAY=86400.0;
SECYR =31536000.0;
//*-------------------------------------------------------------------------      
//*  Find year:                                                                   
//*                                                                               
SSEC = 0.0;
for(I=1;I<201;I++)
{
    IND = 0.0;
//               -- Leap year                 
    if ((I%4) == 0) IND = 1.0;
//               -- Add years                 
    SSEC = SSEC + SECYR + IND*SECDAY;
//               -- Year found                
    if (SSEC > MSECS) goto found;              
        SIN = IND;
}
found:                                                              
*YR = I - 1;
//*                                                                               
//*  Find day-of-year and date:                                                   
//*                                                                               
//c-- Reset remaining seconds   
SSEC = MSECS - (SSEC - SECYR - IND*SECDAY);

for(I=1;I<367;I++)
{
    SSEC = SSEC - SECDAY;
    if (SSEC < 0) goto incr;                                              
}
incr:

I = I - 1;
//           -- 366 days of year          
if (SIN > 0.0) I = I + 1;
//           --                           
if (I == 0)
{                           
//           -- Justifying if change      
    *YR = *YR - 1;
//           -- of year                   
    I  = 365;                                  
    if ((*YR%4) == 0) I = 366;
}


*DOY = I;
//-- convert from doy to date  
DTE (I,&DDAY,&MMTH,*YR);
*DAY=DDAY;
*MTH=MMTH;


if(*YR >= 100) *YR=*YR-100;            /* test for year 2000 or more        */





//*                                                                               
//*  Find hour:                                                                   
//*                                                                               
SSEC = SSEC + SECDAY;

for(I=1;I<25;I++)
{
    SSEC = SSEC - 60.0*60.0;
    if (SSEC < 0) goto seks;                                              
}
seks:
*HR = I - 1;
//*                                                                               
//*  Find minutes and remaining seconds:                                          
//*                                                                               
SSEC = SSEC + 60.0*60.0;
for(I=1;I<61;I++)
{
    SSEC = SSEC - 60.0;
    if (SSEC < 0) goto otte;
}
otte:                                              
   *MIN = I - 1;
   *fSEC = SSEC + 60.0;
if(*YR < 50)
{
    *YR=*YR+2000;
}else{
    *YR=*YR+1900;
}
}

void CHK_ALL_PICS(double *win1,double *win2)
{
  char  PDUMM[500];
  int   PYEAR;
  int   PMONT;
  int   PDAYS;
  int   PHOUR;
  int   PMINU;  
  float PSECO;
  long  LPYE;
  long  LPMO;
  long  LPHR;
  long  LPDA;
  long  LPMI;
  double PMSECS;  
  double PTIMES[1000];
  double PMINTID;
  double LASTTID;
  double FIRSTMINTID;
  double WINMAX;
  double FROM_MSECS;
  double TO_MSECS;
  double PMAXTID;
  double WINSTART[1000];
  double WINSTOPP[1000];
  double w1;
  double w2;
  int    MAXES[1000];  
  int    PIX;
  int    mostpicks;  
  int    i,k;
  int    SEKUNDER;
  int    maxtell;
  int    maxcounter;
  int    tell;
  int    sw;
  
//----------------------------------------------------------------
// save all times in PTIMES
//----------------------------------------------------------------  
  for(i=0;i<allpick_ix;i++)
  {
    sscanf(ALLPICKS[i],"%47c%4d%2d%2d%2d%2d%3c%f",PDUMM,&PYEAR,&PMONT,&PDAYS,&PHOUR,&PMINU,PDUMM,&PSECO);
    LPYE = PYEAR;
    LPMO = PMONT;
    LPDA = PDAYS;
    LPHR = PHOUR;
    LPMI = PMINU;
    TIMSEC(LPYE,LPMO,LPDA,LPHR,LPMI,PSECO,&PMSECS);
    PTIMES[i] = PMSECS;
  } 
//---------------------------------------------------------------
// find time first and last pick
//---------------------------------------------------------------
  PMINTID = 9333333333.0;
  for(i = 0; i < allpick_ix; i++)
  {
    if(PTIMES[i] < PMINTID)
    {
      PMINTID=PTIMES[i];
      PIX = i;
    }
  }
  printf("\n");
  printf("pix: %2d mintid: %12.2f %s",PIX,PMINTID,ALLPICKS[PIX]);
  LASTTID = 0.0;
  for(i = 0; i < allpick_ix; i++)
  {
    if(PTIMES[i] > LASTTID)
    {
      LASTTID=PTIMES[i];
      PIX = i;
    }
  }
  printf("pix: %2d maxtid: %12.2f %s",PIX,LASTTID,ALLPICKS[PIX]);
  WINMAX=LASTTID-PMINTID;
  SEKUNDER = (int)(WINMAX) + 1;
//  printf("MIN-MAX PICKS: %12.2f seconds. SEKUNDER: %3d\n",WINMAX,SEKUNDER);  // ok
  printf("Number of seconds between first and last phase-pick: %12.2f (%d)\n",WINMAX,SEKUNDER);
  FIRSTMINTID=PMINTID;  
  maxtell=0;
  maxcounter=0;
  PMINTID=FIRSTMINTID;
  PMAXTID=PMINTID+120;
  printf("PMINTID: %12.2f PMAXTID: %12.2f\n",PMINTID,PMAXTID);  // ok
  printf("SEKUNDER: %d\n",SEKUNDER);
  for(k=0;k<SEKUNDER;k++)
  {
    tell=0;
    for(i=0;i<allpick_ix;i++)
    {
      if(PTIMES[i] >= PMINTID && PTIMES[i] <= PMAXTID)
        tell++;
    }
//    printf("%12.2f - %12.2f  %2d\n",PMINTID,PMAXTID,tell);
    MAXES[k]=tell;
    WINSTART[k] = PMINTID;
    WINSTOPP[k] = PMAXTID;
    PMINTID=PMINTID+1.0;
    PMAXTID=PMAXTID+1.0;   
  }
  mostpicks = 0;
  for(k=0;k<SEKUNDER;k++)
  {
    if(MAXES[k] > mostpicks)
    {
      mostpicks = MAXES[k];
      PIX = k;
    }
  }
  printf("Max picks %2d in window %12.2f - %12.2f\n",mostpicks,WINSTART[PIX],WINSTOPP[PIX]);
  w1=WINSTART[PIX];
  w2=WINSTOPP[PIX];
  printf("w1: %12.2f   w2: %12.2f\n",w1,w2);
  *win1 = w1;
  *win2 = w2;
//sprintf(WND,"%12.2f %12.2f",w1,w2);
// printf("%s\n",WND); 
//  printf("here\n");

//  return;
}
int WHICH_S(int sx1,int sx2,char tchk1[], char tchk2[])
{
  char  PDUMM[200];  
  int   PYEAR;
  int   PMONT;
  int   PDAYS;
  int   PHOUR;
  int   PMINU;
  float PSECO;  
  long  LPYE;
  long  LPMO;
  long  LPHR;
  long  LPDA;
  long  LPMI;
  double PTI1;
  double PTI2;
//printf("phase inside\n"); 
  sscanf(tchk1,"%2d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f",&PDAYS,PDUMM,&PMONT,PDUMM,&PYEAR,PDUMM,&PHOUR,PDUMM,&PMINU,PDUMM,&PSECO);
  LPYE = PYEAR;
  LPMO = PMONT;
  LPDA = PDAYS;
  LPHR = PHOUR;
  LPMI = PMINU;
  TIMSEC(LPYE,LPMO,LPDA,LPHR,LPMI,PSECO,&PTI1);
  
  
  sscanf(tchk2,"%2d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f",&PDAYS,PDUMM,&PMONT,PDUMM,&PYEAR,PDUMM,&PHOUR,PDUMM,&PMINU,PDUMM,&PSECO);
  LPYE = PYEAR;
  LPMO = PMONT;
  LPDA = PDAYS;
  LPHR = PHOUR;
  LPMI = PMINU;
  TIMSEC(LPYE,LPMO,LPDA,LPHR,LPMI,PSECO,&PTI2);  
  
  if(PTI1 <= PTI2)
  {
    return(sx2);
  }else{
    return(sx1);
  }

}
int PHASE_INSIDE(double win1,double win2,char tid[])
//PHASE_INSIDE(char tid[])
{
  char  PDUMM[200];  
  int   PYEAR;
  int   PMONT;
  int   PDAYS;
  int   PHOUR;
  int   PMINU;
  float PSECO;  
  long  LPYE;
  long  LPMO;
  long  LPHR;
  long  LPDA;
  long  LPMI;
  double PTI;
//printf("phase inside\n"); 
  sscanf(tid,"%2d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f",&PDAYS,PDUMM,&PMONT,PDUMM,&PYEAR,PDUMM,&PHOUR,PDUMM,&PMINU,PDUMM,&PSECO);
  LPYE = PYEAR;
  LPMO = PMONT;
  LPDA = PDAYS;
  LPHR = PHOUR;
  LPMI = PMINU;
  TIMSEC(LPYE,LPMO,LPDA,LPHR,LPMI,PSECO,&PTI);
  ppsstid=PTI;
  if(PTI >= win1 && PTI <= win2)
  {
    printf("TID: %s OK\n",tid);    
  }else{
    printf("TID: %s OUTSIDE\n",tid);    
  }
}
void PHASE2MSEC(char tid[])
//PHASE_INSIDE(char tid[])
{
  char  PDUMM[200];  
  int   PYEAR;
  int   PMONT;
  int   PDAYS;
  int   PHOUR;
  int   PMINU;
  float PSECO;  
  long  LPYE;
  long  LPMO;
  long  LPHR;
  long  LPDA;
  long  LPMI;
  double PTI;
//printf("phase inside\n"); 
  sscanf(tid,"%2d%1c%2d%1c%2d%1c%2d%1c%2d%1c%f",&PDAYS,PDUMM,&PMONT,PDUMM,&PYEAR,PDUMM,&PHOUR,PDUMM,&PMINU,PDUMM,&PSECO);
  LPYE = PYEAR;
  LPMO = PMONT;
  LPDA = PDAYS;
  LPHR = PHOUR;
  LPMI = PMINU;
  TIMSEC(LPYE,LPMO,LPDA,LPHR,LPMI,PSECO,&PTI);
  ppsstid=PTI;
}
int createHTML2(float ml,float mc,float mw,float latt,float lonn,char tid[],char where[],char TRG_TID[])
{
  FILE *out;
  FILE *sfile;
  FILE *in;
  char filn2[200]; 
  char syscmd[200];  
  int i,br,k,l,ret;
  char record[100];
  char newrecord[100];
  char slines[200][100];
  char sli[100][100];    
  int ant=0;  
  int yval=15;  
  char filn[200];
  int lengde;
  
//  printf("%7.3f %7.3f %3.1f\n",latt,lonn,mw);

//  printf("AUTOPHASE: SFCODALOC........................: %s\n",SFCODALOC);
  if ((sfile = fopen (SFCODALOC, "rb")) == NULL)
  {
    printf("AUTOPHASE: Can't open sfile file: %s\n",SFCODALOC);
    return(-1);
  }
  for(i=0;i<200;i++)
  {
    br = les_linje(sfile,record);    
    sprintf(slines[i],"%s",record);
//    printf("%s\n",slines[i]);
    if(br==1)
    {
      ant=i;
      break;
    }
  }    
//  printf("ant %d\n",ant);  
  
  
  sprintf(filn,"%s/cod/%s.html",topdir_html,TRG_TID);
   
//  printf("AUTOPHASE: HTML.............................: %s\n",filn);
  out = fopen(filn,"w");
  if(out == NULL)
  {
    return(-1);
  }
  
  fprintf(out,"<!DOCTYPE html PUBLIC %c-//W3C//DTD HTML 4.01//EN%c %chttp://www.w3.org/TR/html4/strict.dtd%c>\n",0x22,0x22,0x22,0x22);  
  fprintf(out,"<html>\n");
  fprintf(out,"  <head>\n");
  fprintf(out,"  <meta content=%ctext/html; charset=UTF-8%c\n",0x22,0x22);
  fprintf(out," http-equiv=%ccontent-type%c>\n",0x22,0x22);
  fprintf(out,"  <style>\n");
  fprintf(out,"    body {\n");
  fprintf(out,"    margin: 0px;\n");
  fprintf(out,"    padding: 0px;\n");
  fprintf(out,"  }\n");
  fprintf(out,"  </style>\n");
  fprintf(out,"  </head>\n");
  fprintf(out,"  <body>\n");
  fprintf(out,"    <canvas id=%cmyCanvas%c width=%c1300%c height=%c740%c ></canvas>\n",0x22,0x22,0x22,0x22,0x22,0x22);
  fprintf(out,"    <script>\n");
  fprintf(out,"      var line1=%c%s%c\n",0x22,where,0x22);
  fprintf(out,"      var line2=%cUTC %s%c;\n",0x22,tid,0x22);

  if(automag == 1)
    fprintf(out,"      var line3=%cLat: %6.2f Lon: %6.2f Mw: %3.1f Ml: %3.1f Mc: %3.1f%c;\n",0x22,latt,lonn,mw,ml,mc,0x22);  
  
  
  fprintf(out,"      var line4=%cPreliminary locations. Solutions are automatic and may have large errors.%c\n",0x22,0x22);
  fprintf(out,"      var canvas = document.getElementById('myCanvas');\n");
  fprintf(out,"      var context = canvas.getContext('2d');\n");
  fprintf(out,"      var imageObj = new Image();\n");
  fprintf(out,"      context.fillStyle = %c#000000%c;\n",0x22,0x22);
  fprintf(out,"      context.fillRect(0,0,1300,740);\n");  
  fprintf(out,"      context.fillStyle = %c#dddddd%c;\n",0x22,0x22);
  fprintf(out,"      context.fillRect(0,0,640,740);\n");
  fprintf(out,"      imageObj.onload = function() {\n");
  fprintf(out,"      context.drawImage(imageObj, 0, 65);\n");
  fprintf(out,"      context.font = %c15px Arial%c;\n",0x22,0x22);
  fprintf(out,"      context.fillStyle = 'blue';\n");
  fprintf(out,"      context.fillText(line1,10,20);\n");
  fprintf(out,"      context.fillText(line2,10,40);\n");
  fprintf(out,"      context.fillText(line3,10,60);\n");
  fprintf(out,"      context.fillText(line4,10,730);\n");
  fprintf(out,"      context.font = %c14px Dejavu sans mono%c;\n",0x22,0x22);  
  fprintf(out,"      context.fillStyle = 'white';\n");  
  for(i=0;i<ant;i++)
  {
    fprintf(out,"      context.fillText(%c%s%c,650,%d);\n",0x22,slines[i],0x22,yval); 
    yval=yval+20;
  }
  fprintf(out,"      };\n");
  fprintf(out,"      imageObj.src = '%s.png';\n",TRG_TID);
  fprintf(out,"    </script>\n");
  fprintf(out,"  </body>\n");
  fprintf(out,"</html> \n");
  fclose(out);     
  


  sprintf(syscmd,"ls %s/cod/*.html > unsorted2",topdir_html);
  sprintf(syscmd,"ls %s/cod/*.html | xargs -n 1 basename > unsorted2",topdir_html);
  ret = system(syscmd);
  sprintf(filn2,"unsorted2");
  in = fopen(filn2,"r");
  if(in == NULL)
  {
    return(-1);
  }
  out = fopen("unsorted1","w");
  for(i=0;i<30;i++)
  {
    br = les_linje(in,record);
    if(br == 1)
      break;
//    printf("%s\n",record);
//    lengde=strlen(record);
//    for(k=0;k<26;k++)
//    {
//      newrecord[k] = record[lengde-26+k]; 
//    }
//    printf("%s\n",newrecord);
    fprintf(out,"../cod/%s\n",record);
//    fprintf(out,"%s/cod/%s\n",topdir_html,newrecord);    
  }
  fclose(in);
  fclose(out);
  
  
  sprintf(syscmd,"sort -r unsorted1 > sorted1");

  ret = system(syscmd);  

  sprintf(filn,"%s/map/AUTOLOC.html",topdir_html);
  out = fopen(filn,"w");
  if(out == NULL)
  {
    return(-1);
  }
  fprintf(out,"<!DOCTYPE html PUBLIC %c-//W3C//DTD HTML 4.01//EN%c %chttp://www.w3.org/TR/html4/strict.dtd%c>\n",0x22,0x22,0x22,0x22);  
  fprintf(out,"<html>\n");
  fprintf(out,"  <head>\n");
  fprintf(out,"  <meta content=%ctext/html; charset=UTF-8%c\n",0x22,0x22);
  fprintf(out," http-equiv=%ccontent-type%c>\n",0x22,0x22);
  fprintf(out,"<meta http-equiv='refresh' content='60'>\n");  
  fprintf(out,"<title>Auto-location </title>\n");
  fprintf(out,"<SCRIPT LANGUAGE=%cJavaScript%c>",0x22,0x22);
  fprintf(out,"function formHandler(form){");
  fprintf(out,"var URL = document.form1.site.options[document.form1.site.selectedIndex].value;");
  fprintf(out,"parent.main.location = URL;");
  fprintf(out,"}\n");
  fprintf(out,"</SCRIPT>\n");
  fprintf(out,"</head>\n");

  fprintf(out,"<body bgcolor=%c#FFFFFF%c text=%c#000000%c>\n",0x22,0x22,0x22,0x22);
  fprintf(out,"<form name=%cform1%c>\n",0x22,0x22);
 
  fprintf(out,"<select name=%csite%c size=1 onChange=%cformHandler(this)%c >\n",0x22,0x22,0x22,0x22);
  
  sprintf(filn2,"sorted1");
  in = fopen(filn2,"r");
  if(in == NULL)
  {
    return(-1);
  }
  ant=20;
  br = les_linje(in,record);
  for(l=0;l<200;l++)
  {
    if(record[l] == '2')
    {
      k=l;
      break;
    }
  }
  rewind(in);  
  for(i=0;i<20;i++)
  {
    br = les_linje(in,record);
//    printf("%s\n",record);
    sprintf(slines[i],"%s",record); 
//printf("%s\n",slines[i]);
for(l=0;l<200;l++)
{
  if(slines[i][l] =='.' && slines[i][l+1] == 'h')
  {
    slines[i][l+5] = '\0';
    break;
  }
}
//printf("%s\n",slines[i]);   
    for(l=0;l<21;l++)
      record[l]=record[k+l];
    record[l]='\0';
    sprintf(sli[i],"%s",record);
    if(br==1)
    {
      ant=i;
      break;
    }
  }    
  for(i=0;i<ant;i++)
  {
    fprintf(out,"<option value=%c%s%c>%s\n",0x22,slines[i],0x22,sli[i]);
  }
  fprintf(out,"</select>\n");
  fprintf(out,"<font face=%cDejavu sans mono%c color=%cblack%c</font>\n",0x22,0x22,0x22,0x22);

  fprintf(out,"<label for=%cmenu%c>Auto-location, phases after event recording.</label>\n",0x22,0x22);

  fprintf(out,"</form>\n");
  fprintf(out,"<IFRAME SRC=%c%s%c name=%cmain%c WIDTH=1305 HEIGHT=745 frameborder=1 border=1></IFRAME>\n",0x22,slines[0],0x22,0x22,0x22);
  fprintf(out,"</body>\n");
  fprintf(out,"<br>\n");
  fprintf(out,"<font size %c4%c>Nominatim Search Courtesy of <a href=%chttp://www.mapquest.com/%c target=%c_blank%c>MapQuest</a> <img src=%chttp://developer.mapquest.com/content/osm/mq_logo.png%c</FONT>\n",0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22);    
  fprintf(out,"</html>\n");
  fclose(out);

  
  return(0);



  
  
}

int Update_map_file(char snam[],int ant,float resid,int nst,int klon,int subnet,char TRG_TID[],char MINTRGTID[])
{
  FILE *sf;
  FILE *loc;
  FILE *out;
  FILE *gog;
  FILE *llc;
  FILE *gmt;
  FILE *sfile;
  int i,l,cr,br,tel;
  int karakterer;
  int dagen;
  int timen;
  int minuttet;
  int sekundet;
  int aret;
  int maned;
  int leng;
  float lat,lon;
  char linjer[200];
  char dummy[500];
  char filn[200];
  char mapname[200];
  char locname[200];
  char mapout[200];
  char gmtmap[200];
  char name[200];
  char copyhyp[200];
  char lastline[200];
  char where[600];
  char record[600];
  char melding0[500];  
  char melding[500]; 
  char melding2[1000];
  char melding3[1000];
  char png_request2[500];
  char png_request3[500];
  
  char msg1[500];
  
  char fnavn[200];
  float res[200];
  float mag;
  long size;
  float avg;
  float ml;
  float mc;
  float mw;
  int br1;
  int ix1;
  int zoom;
  int funnet;
  int ret;
  int cnt=0;
  tel=0;
  avg=0;
  if(prt >= 1)  
    printf("Update_map_file: %s\n",snam);
  for(i=0;i<256;i++)
  {
    if(snam[i] == '.' && snam[i+1] =='S')
      break;
  }

  for(l=0;l<20;l++)
    snam[l]=snam[i-11+l];
  
  sprintf(filn,"%s/rt/tmp%d/hyp.out",topdir_rt,subnet);

  sprintf(filn,"hyp.out");
  
//  printf("****************Open hyp.out: %s **************\n",filn);  
  sf=fopen(filn,"r");
  if(sf == NULL)
  {
    printf("Update_map_file: Can't open %s\n",filn);
    exit(0);
  }
  
  sprintf(mapout,"%s/map/map.out",topdir_html); 
  
// printf("Open: %s\n",mapout); 
  
  out=fopen(mapout,"r+");
  if(out == NULL)
  {
    printf("rtpick:Update_map_file: Can't open: %s\n",mapout);
    exit(0);
  }else{
    br=0;
    cr=0;
    for(i=0;i<(ant+5);i++)
    {
      for(l=0;l<90;l++)
      {
        linjer[l]=fgetc(sf);
        if(linjer[l] == 0xa)
        {
          cr=1;
          br++;
          if(br==1)
          {
            sscanf(linjer,"%23c %f %f",dummy,&lat,&lon);
            sscanf(linjer,"%56c%f3.1",dummy,&mag);
//	    printf("MAGNITUDES: %c %c %c\n",linjer[59],linjer[67],linjer[75]);
	    ml=0.0;
	    mc=0.0;
	    mw=0.0;
	    switch(linjer[59])
	    {
	      case 'L':
	      sscanf(linjer,"%56c%f3.1",dummy,&ml);
	      break;
	      case 'C':
	      sscanf(linjer,"%56c%f3.1",dummy,&mc);
	      break;
	      case 'W':
	      sscanf(linjer,"%56c%f3.1",dummy,&mw);
	      break;
	    }
	    switch(linjer[67])
	    {
	      case 'L':
	      sscanf(linjer,"%64c%f3.1",dummy,&ml);
	      break;
	      case 'C':
	      sscanf(linjer,"%64c%f3.1",dummy,&mc);
	      break;
	      case 'W':
	      sscanf(linjer,"%64c%f3.1",dummy,&mw);
	      break;
	    }
	    switch(linjer[75])
	    {
	      case 'L':
	      sscanf(linjer,"%72c%f3.1",dummy,&ml);
	      break;
	      case 'C':
	      sscanf(linjer,"%72c%f3.1",dummy,&mc);
	      break;
	      case 'W':
	      sscanf(linjer,"%72c%f3.1",dummy,&mw);
	      break;
	    }
//	    printf("LAT: %7.3f LON: %7.3f ML: %3.1f MC: %3.1f MW: %3.1f\n",lat,lon,ml,mc,mw);
          }
          if(br==3)
          {
            sscanf(linjer,"%s",name);
          }         
        }
        if(cr == 1)
        {
          if(br <= 5)
          {
            cr=0;
            break;
          }else{
            sscanf(linjer,"%65c %f",dummy,&res[tel]);

            avg=avg+fabs(res[tel]);
            tel++;
            cr=0;
            break;
          }
        }
      }

    }
    fseek(out,0,SEEK_END);
    size=ftell(out);
    fseek(out,size-80,SEEK_SET);
    for(l=0;l<80;l++)
    {
      linjer[l]=fgetc(out);
      if(linjer[l]==0xa)
      {
        fseek(out,0,SEEK_CUR);
        fprintf(out,"circle,red,%s %6.2f %2d,%8.3f,%8.3f\n",snam,resid,nst,lat,lon);
        fprintf(out,"google,yellow,%s %6.2f %2d,%8.3f,%8.3f\n",snam,resid,nst,lat,lon);
        break;
      }
    }
    fclose(out);
    fclose(sf);
  }


  sscanf(snam,"%2d%1c%2d%2d%1c%2d%3c%4d%2d",&dagen,dummy,&timen,&minuttet,dummy,&sekundet,dummy,&aret,&maned);
  sprintf(gmtmap,"%s/map/GMT_EPI%d.txt",topdir_html,klon);
  gmt=fopen(gmtmap,"a+");
  if(gmt == NULL)
  {
    printf("AUTOPHASE: Update_map_file: Can't open %s\n",gmtmap);
    exit(0);
  }else{
    fprintf(gmt,"%4d %2d %2d %2d %2d %2d %6.2f  %8.3f %2d %4.1f\n",aret,maned,dagen,timen,minuttet,sekundet,lon,lat,nst,mag);
    fclose(gmt);
  }
  
  
  sprintf(mapname,"%s/loc/%s.html",topdir_html,snam);
  loc=fopen(mapname,"w");
  fprintf(loc,"<!DOCTYPE html>\n");
  fprintf(loc,"<html>\n");
  fprintf(loc,"<body>\n");
  fprintf(loc," <img src=%chttp://maps.googleapis.com/maps/api/staticmap?center=%8.3f,%8.3f&zoom=7&size=600x600&maptype=roadmap&markers=color:blue|label:S|40.702147,-74.015794&markers=color:red|color:red|label:C|%8.3f,%8.3f&sensor=false%c>\n",0x22,lat,lon,lat,lon,0x22);
  fprintf(loc,"</body>\n");
  fprintf(loc,"</html>\n");
  fclose(loc);
    
  switch(geolocation)
  {
    case 0:                              // no geolocation
    sprintf(melding2,"Geocoding not active"); 
    break;
    case 1:                              // reverse geolocation mapquest
//    sprintf(where,"wget -T 3 -O geo2.xml %copen.mapquestapi.com/nominatim/v1/reverse.php?format=xml&lat=%f&lon=%f&zoom=%d%c 2>xml2.log",0x22,lat,lon,geodetail,0x22);
      
    sprintf(where,"wget -T 3 -O geo2.xml %copen.mapquestapi.com/nominatim/v1/reverse.php?key=Fmjtd%c7Cluu8290b2g%c2C75%c3Do5-947x0f&format=xml&lat=%f&lon=%f&zoom=%d%c 2>xml2.log",0x22,0x25,0x25,0x25,lat,lon,geodetail,0x22);      
      
      printf("%s\n",where);
    ret = system(where);
    break;
    case 2:                              // reverse geolocation nominatim
    sprintf(where,"wget -T 3 -O geo2.xml %cnominatim.openstreetmap.org/reverse?format=xml&lat=%f&lon=%f&zoom=%d&addressdetails=0%c 2>xml2.log",0x22,lat,lon,geodetail,0x22);   
//    printf("%s\n",where);
    ret = system(where);
    break;
  }
    
  if(geolocation == 1 || geolocation == 2)  // extract geographical name of location from xml file returned from mapquest or nominatim
  {
    if ((sfile = fopen ("geo2.xml", "rb")) == NULL)
    {
      printf("AUTOPHASE: Can't open geo2.xml file");
      return(-1);
    }
      
    br1 = les_linje2(sfile,record);  // read first line geo.xml
    if(record[0]=='<')
      cnt++;    
    br1 = les_linje2(sfile,record);  // read second line geo.xml
    if(record[0]=='<')
      cnt++;     
    br1 = les_linje2(sfile,record);  // read third line geo.xml
    if(record[0]=='<')
      cnt++;    
    fclose(sfile);
    if(cnt == 3)
    {
      funnet=0;  
      ix1 = 0;
      br1 = 0;
      for(l=0;l<1000;l++)
      {
        if(record[l] == '>')
        {    
	  funnet = 1;
	  for(i=(l+1);i<500;i++)
	  {
            if(record[i] == '<')
	    {
	      melding2[ix1] = '\0';
	      br1 = 1;
	      break;
	    }else{
	      melding2[ix1] = record[i];
	      ix1++;
	    }
	  }
	  if(br1 == 1)
	    break;
        }
      }  
    }else{
      sprintf(melding2,"Geocoding not active"); 
    } 
  }
  
  
  sprintf(msg1,"UTC: %s | Lat: |   %7.2f   | Lon: |   %7.2f   | Mw: | %3.1f | Ml: | %3.1f | Mc: | %3.1f | %s\n",MINTRGTID,lat,lon,mw,ml,mc,melding2);
//  printf("%s\n",msg1);
  sprintf(fnavn,"%s/map/ALL_LOC.txt",topdir_html);   
  if((out = fopen(fnavn,"a+")) == NULL)
  {
    printf("RTPPH: Can't open file ALL_LOC.txt\n"); 
  }
  fprintf(out,"%s",msg1);
  fclose(out);   
  
  if(automag ==1)
  {
    if(ml > 0.0 || mw > 0.0)
    {
      sprintf(melding,"UTC: %s Lat: %5.2f Lon: %5.2f Mw: %3.1f Ml: %3.1f %s",MINTRGTID,lat,lon,mw,ml,melding2);
      sprintf(melding3,"UTC: %s | Lat: | %5.2f | Lon: | %5.2f | %s | MW: %3.1f | ML: %3.1f\n",MINTRGTID,lat,lon,melding2,mw,ml);
    }else{
      sprintf(melding,"UTC: %s Lat: %5.2f Lon: %5.2f Mw: - Ml: - %s",MINTRGTID,lat,lon,melding2);
      sprintf(melding3,"UTC: %s | Lat: | %5.2f | Lon: | %5.2f | %s | MW: - | ML: -\n",MINTRGTID,lat,lon,melding2);
    }
  }else{
    sprintf(melding,"UTC: %s Lat: %5.2f Lon: %5.2f Mc: %3.1f %s",MINTRGTID,lat,lon,mc,melding2);
    sprintf(melding3,"UTC: %s | Lat: | %5.2f | Lon: | %5.2f | %s | MC: %3.1f |      \n",MINTRGTID,lat,lon,melding2,mc);     
  }
  printf("Map : %s",melding3);   
  printf("Mail: %s\n",melding);  

  if(automag == 1)                                       // smallmap.txt  
    sprintf(melding0,"Recorded event\n%s\nUTC: %s\nLat: %5.2f Lon: %5.2f Mw: %3.1f Ml: %3.1f Mc: %3.1f",melding2,MINTRGTID,lat,lon,mw,ml,mc);     
    
//  printf("AUTOPHASE: mail1............................: %d\n",mail1);
    
  sprintf(copyhyp,"cp %s/rt/tmp%d/hyp.out %s/rt/tmp%d/hyp.txt",topdir_rt,subnet,topdir_rt,subnet);
  sprintf(copyhyp,"cp hyp.out hyp.txt");    
  ret=system(copyhyp);
  for(l=0;l<500;l++)
    mail_message[l]='\0';

//sprintf(mail_message,"echo %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=7&size=900x1000&maptype=hybrid&markers=color:red_7Ccolor:red_7Clabel:Q_7C%f,%f&sensor=false%c | mutt  -a %c%s/loc/ALL.png%c -s %cTRIGGER%c -- %s &",0x22,lat,lon,lat,lon,0x22,0x22,topdir_rt,0x22,0x22,0x22,mailaddress1);
  sprintf(mail_message,"echo %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=7&size=900x1000&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c | mutt  -a %c%s/loc/ALL.png%c -s %c%dCOD %s%c -a %chyp.txt%c -- %s &",0x22,lat,lon,lat,lon,0x22,0x22,topdir_html,0x22,0x22,subnet,melding,0x22,0x22,0x22,mailaddress1);


  sprintf(mail_storage1,"echo %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=7&size=900x1000&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c | mutt  -a %c%s/loc/ALL.png%c -s %c%dCOD %s%c -a %chyp.txt%c -- %s &",0x22,lat,lon,lat,lon,0x22,0x22,topdir_html,0x22,0x22,subnet,melding,0x22,0x22,0x22,mailaddress1);
  sprintf(mail_storage2,"echo %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=7&size=900x1000&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c | mutt  -a %c%s/loc/ALL.png%c -s %c%dCOD %s%c -a %chyp.txt%c -- %s &",0x22,lat,lon,lat,lon,0x22,0x22,topdir_html,0x22,0x22,subnet,melding,0x22,0x22,0x22,mailaddress2);
  sprintf(mail_storage3,"echo %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=7&size=900x1000&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c | mutt  -a %c%s/loc/ALL.png%c -s %c%dCOD %s%c -a %chyp.txt%c -- %s &",0x22,lat,lon,lat,lon,0x22,0x22,topdir_html,0x22,0x22,subnet,melding,0x22,0x22,0x22,mailaddress3);
  sprintf(mail_storage4,"echo %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=7&size=900x1000&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c | mutt  -a %c%s/loc/ALL.png%c -s %c%dCOD %s%c -a %chyp.txt%c -- %s &",0x22,lat,lon,lat,lon,0x22,0x22,topdir_html,0x22,0x22,subnet,melding,0x22,0x22,0x22,mailaddress4);
  sprintf(mail_storage5,"echo %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=7&size=900x1000&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c | mutt  -a %c%s/loc/ALL.png%c -s %c%dCOD %s%c -a %chyp.txt%c -- %s &",0x22,lat,lon,lat,lon,0x22,0x22,topdir_html,0x22,0x22,subnet,melding,0x22,0x22,0x22,mailaddress5);


  for(l=0;l<500;l++)          // check for underscore in textline BEFORE '|'. This will accept underscore in directory names.
  {
    if(mail_message[l] == '|')   
      break;
    if(mail_message[l]=='_')
      mail_message[l]='%';
    if(mail_storage1[l]=='_')
      mail_storage1[l]='%';
    if(mail_storage2[l]=='_')
      mail_storage2[l]='%';
    if(mail_storage3[l]=='_')
      mail_storage3[l]='%';
    if(mail_storage4[l]=='_')
      mail_storage4[l]='%';
    if(mail_storage5[l]=='_')
      mail_storage5[l]='%';  	
  }

  zoom = 8;     
  sprintf(png_request2,"wget -T 1 -O %s/cod/%s.png %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=%d&size=640x640&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c 2>request.log ",topdir_html,TRG_TID,0x22,lat,lon,zoom,lat,lon,0x22);
//    printf("png_request2: %s\n",png_request2);      

  sprintf(png_request3,"wget -O %s/map/smallmap.png %chttp://maps.googleapis.com/maps/api/staticmap?center=%f,%f&zoom=%d&size=320x320&maptype=hybrid&markers=icon:http://maps.google.com/mapfiles/kml/pal3/icon33.png_7C%f,%f&sensor=false%c 2>request.log ",topdir_html,0x22,lat,lon,zoom,lat,lon,0x22);
//    printf("png_request3: %s\n",png_request3);

  leng = strlen(topdir_html);    
  for(l=leng+10;l<500;l++)
  {
    if(png_request2[l]=='_')
      png_request2[l]='%'; 
    if(png_request3[l]=='_')
      png_request3[l]='%';       
  }
  ret=system(png_request2); 
  ret=createHTML2(ml,mc,mw,lat,lon,MINTRGTID,melding2,TRG_TID); 
    
  ret=system(png_request3);
 
  sprintf(fnavn,"%s/map/smallmap.txt",topdir_html);   
  if((out = fopen(fnavn,"w")) == NULL)
  {
    printf("RTPPH: Can't open file smallmap.txt\n"); 
  }
  fprintf(out,"%s",melding0);
  fclose(out);     
    
  sprintf(fnavn,"%s/map/map.txt",topdir_html);   
  if((out = fopen(fnavn,"w")) == NULL)
  {
    printf("RTPPH: Can't open file map.txt\n"); 
  }
  fprintf(out,"%s",melding3);
  fclose(out);    
    
  sprintf(locname,"%s/map/LAST_LOC.txt",topdir_html);
  llc=fopen(locname,"w");
  if(llc == NULL)
  {
    printf("rtpick:Update_map_file: Can't open %s\n",locname);
    exit(0);
  }else{
    fprintf(llc,"%s | %6.2f | %8.3f | %8.3f | %2d | %4.1f | text\n",snam,resid,lat,lon,nst,mag);
    fclose(llc);
  }

  sprintf(copyhyp,"cp %s/rt/tmp%d/hyp.out %s/map",topdir_rt,subnet,topdir_html);
  sprintf(copyhyp,"cp hyp.out %s/map",topdir_html);    
  ret=system(copyhyp);  

}


void Create_Sfile(char filename[],char sfilepath[],char dbname[],int nchannels,int klon,int locate,char yr_trg[],char mo_trg[],char da_trg[],char hr_trg[],char mi_trg[],char se_trg[],char sfilename[],int subnet,char TRG_TID[],char MINTRGTID[])
{
  FILE *sf;
  FILE *so1;
  FILE *so2;
  int retur;
  int i;
  char fullpath[200];
  char syscmd[200];
  char comm0[200];
  char comm1[200];
  char comm2[200];
  char comm3[200];
  char comm4[200];
  char comm5[200];
  char comm6[200];
  char comm7[200];
  char comm8[200];
  char comm9[200];
  char com10[200];
  char com11[200];
  char com12[200];
  char com13[200];  
  char com14[200];
  char com15[200];  
  char com16[200]; 
  char com20[200];
  char com21[200];
  
  char snam[200];
  char dummy[200];
  char fpath[200];
  char year[5];
  char month[5];
  char day[5];
  char hour[5];
  char minute[5];
  char sec[5];
  
  char record[256];

  char seconds[10];
  char tempo[80];
  int ret;
  int lp;
  int kart;
  int ne;
  int rettur;
  int br;

  float resid;
  float res_lim;
  int nst;
  for(i=0;i<200;i++)
  {
    snam[i]='\0';
    fullpath[i]='\0';
  }

printf("-----------------------------------------------------------------------\n");
printf("Create s-file: %s\n",sfilepath);
printf("-----------------------------------------------------------------------\n");
  S_REC2(yr_trg,mo_trg,da_trg,hr_trg,mi_trg, se_trg, dummy,sfilepath,filename,nchannels,snam);
//---------------------------------------------
// check for auto locate
//---------------------------------------------
#ifdef RTQUAKE
  if(keep == 1 && locate == 1)
  {
    if(dbg==1)
      printf("AUTOPHASE: Create_Sfile.....................: Locate + new s-file.\n");

    sprintf(com13,"automag %s >> autotmp.txt",sfilepath);
    sprintf(com14,"hyp automag.out >> hyptemp.txt");
    sprintf(com15,"cp automag.out %s",sfilepath);

    sprintf(comm0,"rm hyptemp.txt");
    sprintf(comm1,"hyp %s >> hyptemp.txt",sfilepath);
    sprintf(comm2,"cp hyp.out %s", sfilepath);
    sprintf(comm3,"cp hyp_new.out %s", sfilepath);
    sprintf(comm4,"cp %s hyp_save.out",sfilepath);
    sprintf(comm5,"cp hyp_save.out %s",sfilepath);
    sprintf(comm6,"cp hyp.out hyp.tmp");
    sprintf(comm7,"cp hyp.tmp %s",sfilepath);
    sprintf(comm8,"cp hyp.out hyp_all.out");
    sprintf(comm9,"cp hyp_save.out %s",sfilepath);
    sprintf(com10,"cp %s s_org.out",sfilepath);
    sprintf(com11,"cp hyp.out %s/map",topdir_html);
    
    
    if(dbg==1)
      printf("AUTOPHASE: comm0............................: %s\n",comm0);
    ret=system(comm0);                                     // remove old hyptemp.txt
    if(dbg==1)
      printf("AUTOPHASE: com10............................: %s\n",com10);
    ret=system(com10);                                     // copy original s-file to s_org.out
    if(dbg==1)
      printf("AUTOPHASE: comm1............................: %s\n",comm1);
//exit(0);
    ret=system(comm1);                                     // run hyp, input s-file, this produces "hyp.out"
    if(dbg==1)
      printf("AUTOPHASE: comm4............................: %s\n",comm4);
    ret=system(comm4);                                     // save original s-file to hyp_save.out
    if(dbg==1)
      printf("AUTOPHASE: comm2............................: %s\n",comm2);
    ret=system(comm2);                                     // copy hyp.out new s-file
    if(dbg==1)
      printf("AUTOPHASE: comm8............................: %s\n",comm8);
    ret=system(comm8);                                     // copy hyp.out to hyp_all.out
//exit(0);
    res_lim = maxres;
    kart=0;
    rettur=0;
    
printf("-----------------------------------------------------------------------\n");
printf("Start iterations to reduce average residuals, and do location.\n");
printf("-----------------------------------------------------------------------\n");
    
    for(lp=0;lp<iterations;lp++)
    {
      ret=Check_S(&resid,&nst,res_lim,lp);       // read 'hyp.out', create 'hyp_new.out'
//printf("iter: %3d ret=%2d res=%6.3f nst:%2d\n",lp,ret,resid,nst);          
      if(ret==1)
      {
        ret=system(comm3);                                 // copy hyp_new to new s-file 
        ret=system(comm0);                                 // remove old hyptemp.txt

        ret=system(comm1);                                 // run hyp, input s-file
        ret=system(comm2);                                 // copy hyp.out new s-file
      }else{
        printf("AUTOPHASE: Average residual.................: %6.3f\n",resid);
        printf("AUTOPHASE: No more iterations...............: Number of stations: %d Avg.res.: %8.3f\n",nst,resid);
        if(  nst < no_stations_trg)
        {
          printf("AUTOPHASE: Too few stations (<%d)............: No location\n",no_stations_trg);
	  sendmail=-1;
          ret=system(comm5);
          kart=1;
          rettur = -1;
        }
        break;
      }
    }
    if(rettur==0)
    {
      ret=system(comm6);                                   // copy last hyp.out to hyp.tmp
      if(dbg==1)
        printf("AUTOPHASE: comm6............................: %s\n",comm6);
#ifdef RTQUAKE
      ret=system(com11);                                   // copy hyp.out to /map
      if(dbg==1)
        printf("AUTOPHASE: com11............................: %s\n",com11);
#endif
      if(dbg==1)
        printf("AUTOPHASE: comm2............................: %s\n",comm2);
      ret=system(comm2);                                   // copy hyp.out new s-file
    }else{
      ret=system(comm9);                                   // copy hyp_save.out to s-file
      if(dbg==1)
        printf("AUTOPHASE: comm9............................: %s\n",comm9);
    }
 
    if(kart==0)
    {
      int rss=resid*1000;
//      printf("AUTOPHASE: RSS..............................: %d\n",rss);
     
      if(rss != 0)
      {
	if(automag == 1)
	{
//--------------AUTOMAG------------------------------------------------- 
          printf("-----------------------------------------------------------------------\n");
          printf("Start automag.\n");
          printf("-----------------------------------------------------------------------\n");
	  if(dbg==1)
            printf("AUTOPHASE: AM:comm0.........................: %s\n",comm0);
          ret=system(comm0);                                     // remove old hyptemp.txt
          if(dbg==1)
	    printf("AUTOPHASE: AM:com10.........................: %s\n",com10);
          ret=system(com10);                                     // copy original s-file to s_org.out
          if(dbg==1)
	    printf("AUTOPHASE: AM:com13.........................: %s\n",com13);    
          ret=system(com13);                                     // run automag, creates automag.outfile
            printf("AUTOPHASE: AM:com15.........................: %s\n",com15);
          ret= system(com15);                                    // copy automag.out new s-file

            printf("AUTOPHASE: AM:com14.........................: %s\n",com14);
          ret=system(com14);                                     // run hyp, input automag.out, this produces "hyp.out"       
          if(dbg==1)
	    printf("AUTOPHASE: AM:comm2.........................: %s\n",comm2);
          ret=system(comm2);                                     // copy hyp.out new s-file
          if(dbg==1)
	    printf("AUTOPHASE: AM:comm8.........................: %s\n",comm8);
          ret=system(comm8);                                     // copy hyp.out to hyp_all.out
//--------------END AUTOMAG---------------------------------------------   
	}
	
        printf("-----------------------------------------------------------------------\n");
        printf("Update graphics.\n");
        printf("-----------------------------------------------------------------------\n");
        Update_map_file(sfilename,ne,resid,nst,klon,subnet,TRG_TID,MINTRGTID);
        sprintf(com20,"respng");
        ret= system(com20); 
        allpng=1;	

	
      }
    }
  }
#endif  
}










/** picker memory class ***/
// _DOC_ =============================
// _DOC_ FilterPicker5_Memory object/structure
// _DOC_ =============================

/** create FilterPicker5_Memory object/structure and initialize memory */

FilterPicker5_Memory* init_filterPicker5_Memory(
        double deltaTime,
        float* sample, int length,
        const double filterWindow,
        const double longTermWindow,
        const double threshold1,
        const double threshold2,
        const double tUpEvent) {

    int j, k;


    // _DOC_ =============================
    // _DOC_ picker memory for realtime processing of packets of data

    FilterPicker5_Memory* filterPicker5_Memory = calloc(1, sizeof (FilterPicker5_Memory));

    filterPicker5_Memory->longDecayFactor = deltaTime / longTermWindow;
    filterPicker5_Memory->longDecayConst = 1.0 - filterPicker5_Memory->longDecayFactor;
    filterPicker5_Memory->nLongTermWindow = 1 + (int) (longTermWindow / deltaTime);
    filterPicker5_Memory->indexEnableTriggering = filterPicker5_Memory->nLongTermWindow;
    filterPicker5_Memory->enableTriggering = FALSE_INT;
    filterPicker5_Memory->nTotal = -1;
    // _DOC_ set up buffers and memory arrays for previous samples and their statistics
    filterPicker5_Memory->numRecursive = 1; // number of powers of 2 to process
    int nTemp = 1;

    {
        int numPrevious = (int) (filterWindow / deltaTime); // estimate of number of previous samples to bufer
        while (nTemp < numPrevious) {
            filterPicker5_Memory->numRecursive++;
            nTemp *= 2;
        }
        numPrevious = nTemp; // numPrevious is now a power of 2
        //System.out.println("TP DEBUG numPrevious, numRecursive " + numPrevious + ", " + numRecursive);
    }
    filterPicker5_Memory->xRec = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->test = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->filteredSample = calloc(filterPicker5_Memory->numRecursive, sizeof (double*));
    for (k = 0; k < filterPicker5_Memory->numRecursive; k++) {
        filterPicker5_Memory->filteredSample[k] = calloc(3, sizeof (double));
        for (j = 0; j < 3; j++) {
            filterPicker5_Memory->filteredSample[k][j] = 0.0;
        }
    }
    filterPicker5_Memory->lastFilteredSample = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->mean_xRec = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->mean_stdDev_xRec = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->mean_var_xRec = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->period = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->lowPassConst = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->highPassConst = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    double window = deltaTime / (2.0 * PI);
    for (k = 0; k < filterPicker5_Memory->numRecursive; k++) {
        filterPicker5_Memory->mean_xRec[k] = 0.0;
        filterPicker5_Memory->mean_stdDev_xRec[k] = 0.0;
        filterPicker5_Memory->mean_var_xRec[k] = 0.0;
        filterPicker5_Memory->period[k] = window * 2.0 * PI;
        filterPicker5_Memory->lowPassConst[k] = deltaTime / (window + deltaTime);
        filterPicker5_Memory->highPassConst[k] = window / (window + deltaTime);
        window *= 2.0;
    }
    filterPicker5_Memory->lastSample = DOUBLE_MAX_VALUE;
    filterPicker5_Memory->lastDiffSample = 0.0;
    // AJL 20091214
    filterPicker5_Memory->charFunctUncertainty = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->charFunctUncertaintyLast = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    filterPicker5_Memory->uncertaintyThreshold = calloc(filterPicker5_Memory->numRecursive, sizeof (double));
    for (k = 0; k < filterPicker5_Memory->numRecursive; k++) {
        filterPicker5_Memory->uncertaintyThreshold[k] = threshold1 / 2.0;
    }
    filterPicker5_Memory->maxUncertaintyThreshold = threshold1 / 2.0;
    // END - AJL 20091214
    filterPicker5_Memory->minUncertaintyThreshold = 0.5;
    filterPicker5_Memory->maxAllowNewPickThreshold = 2.0;
    filterPicker5_Memory->nTUpEvent = (int) (0.5 + tUpEvent / deltaTime) + 1;
    if (filterPicker5_Memory->nTUpEvent < 1) {
        filterPicker5_Memory->nTUpEvent = 1;
    }
    filterPicker5_Memory->indexUncertainty = calloc(filterPicker5_Memory->numRecursive, sizeof (int*)); // AJL 20091214
    filterPicker5_Memory->polarityDerivativeSum = calloc(filterPicker5_Memory->numRecursive, sizeof (double*));
    filterPicker5_Memory->polaritySumAbsDerivative = calloc(filterPicker5_Memory->numRecursive, sizeof (double*));
    for (k = 0; k < filterPicker5_Memory->numRecursive; k++) {
        filterPicker5_Memory->indexUncertainty[k] = calloc(filterPicker5_Memory->nTUpEvent, sizeof (int)); // AJL 20091214
        filterPicker5_Memory->polarityDerivativeSum[k] = calloc(filterPicker5_Memory->nTUpEvent, sizeof (double));
        filterPicker5_Memory->polaritySumAbsDerivative[k] = calloc(filterPicker5_Memory->nTUpEvent, sizeof (double));
        for (j = 0; j < filterPicker5_Memory->nTUpEvent; j++) {
            filterPicker5_Memory->indexUncertainty[k][j] = 0; // END - AJL 20091214
            filterPicker5_Memory->polarityDerivativeSum[k][j] = 0.0;
            filterPicker5_Memory->polaritySumAbsDerivative[k][j] = 0.0;
        }
    }

    // _DOC_ criticalIntegralCharFunct is tUpEvent * threshold2
    filterPicker5_Memory->criticalIntegralCharFunct = (double) (filterPicker5_Memory->nTUpEvent) * threshold2; // one less than number of samples examined
    // _DOC_ integralCharFunctClipped is integral of charFunct values for last nTUpEvent samples, charFunct values possibly limited if around trigger time
    filterPicker5_Memory->integralCharFunctClipped = calloc(filterPicker5_Memory->nTUpEvent, sizeof (double));
    // flag to prevent next trigger until charFunc drops below threshold2
    filterPicker5_Memory->allowNewPickIndex = INT_UNSET;
    filterPicker5_Memory->charFunctClippedValue = calloc(filterPicker5_Memory->nTUpEvent, sizeof (double));
    filterPicker5_Memory->charFunctValue = calloc(filterPicker5_Memory->nTUpEvent, sizeof (double));
    filterPicker5_Memory->charFuntNumRecursiveIndex = calloc(filterPicker5_Memory->nTUpEvent, sizeof (double));
    for (k = 0; k < filterPicker5_Memory->nTUpEvent; k++) {
        filterPicker5_Memory->charFunctClippedValue[k] = 0.0;
        filterPicker5_Memory->charFunctValue[k] = 0.0;
        filterPicker5_Memory->charFuntNumRecursiveIndex[k] = 0;
    }
    filterPicker5_Memory->upEventBufPtr = 0;
    filterPicker5_Memory->pickPolarity = POLARITY_UNKNOWN;
    filterPicker5_Memory->triggerNumRecursiveIndex = -1;


    // initialize previous samples to mean sample value
    int nmean = filterPicker5_Memory->nLongTermWindow < length ? filterPicker5_Memory->nLongTermWindow : length;
    double sample_mean = 0.0;
    int i;
    for (i = 0; i < nmean; i++) {
        sample_mean += sample[i];
    }
    sample_mean /= (double) nmean;
    for (k = 0; k < filterPicker5_Memory->numRecursive; k++) {
        for (j = 0; j < 3; j++) {
            filterPicker5_Memory->filteredSample[k][j] = 0.0;
        }
    }
    filterPicker5_Memory->lastSample = sample_mean;


    return (filterPicker5_Memory);

}

/** clean up memory */

void free_FilterPicker5_Memory(FilterPicker5_Memory** pfilterPicker5_Memory) {
    
    if (*pfilterPicker5_Memory == NULL)
        return;

    free((*pfilterPicker5_Memory)->xRec);
    free((*pfilterPicker5_Memory)->test);
    int k;
    for (k = 0; k < (*pfilterPicker5_Memory)->numRecursive; k++)
        free((*pfilterPicker5_Memory)->filteredSample[k]);
    free((*pfilterPicker5_Memory)->filteredSample);
    free((*pfilterPicker5_Memory)->lastFilteredSample);
    free((*pfilterPicker5_Memory)->mean_xRec);
    free((*pfilterPicker5_Memory)->mean_stdDev_xRec);
    free((*pfilterPicker5_Memory)->mean_var_xRec);
    free((*pfilterPicker5_Memory)->period);
    free((*pfilterPicker5_Memory)->lowPassConst);
    free((*pfilterPicker5_Memory)->highPassConst);

    for (k = 0; k < (*pfilterPicker5_Memory)->numRecursive; k++) {
        free((*pfilterPicker5_Memory)->polarityDerivativeSum[k]);
        free((*pfilterPicker5_Memory)->polaritySumAbsDerivative[k]);
    }
    free((*pfilterPicker5_Memory)->polarityDerivativeSum);
    free((*pfilterPicker5_Memory)->polaritySumAbsDerivative);
    free((*pfilterPicker5_Memory)->integralCharFunctClipped);
    free((*pfilterPicker5_Memory)->charFunctClippedValue);
    free((*pfilterPicker5_Memory)->charFunctValue);
    free((*pfilterPicker5_Memory)->charFuntNumRecursiveIndex);

    for (k = 0; k < (*pfilterPicker5_Memory)->numRecursive; k++) {
        free((*pfilterPicker5_Memory)->indexUncertainty[k]);
    }
    free((*pfilterPicker5_Memory)->indexUncertainty);
    free((*pfilterPicker5_Memory)->charFunctUncertainty);
    free((*pfilterPicker5_Memory)->charFunctUncertaintyLast);
    free((*pfilterPicker5_Memory)->uncertaintyThreshold);


    free(*pfilterPicker5_Memory);
    *pfilterPicker5_Memory = NULL;

}
/*
 * This file is part of the Anthony Lomax C Library.
 *
 * Copyright (C) 2006-2009 Anthony Lomax <anthony@alomax.net www.alomax.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */



// AJL: based on FilterPicker5.java, 2008.07.14




//enum ResultType resultType = PICKS;


/*** function to calculate picks  */
// _DOC_ =============================
// _DOC_ FilterPicker5
// _DOC_ =============================

//public final float[] apply(double deltaTime, float[] sample) {

void Pick(

        double deltaTime, // dt or timestep of data samples
        float sample[], // array of num_samples data samples
        int num_samples, // the number of samples in array sample

        const double filterWindow, // FilterPicker5 filter window
        // _DOC_ the filter window (filterWindow) in seconds determines how far back in time the previous samples are examined.  The filter window will be adjusted upwards to be an integer N power of 2 times the sample interval (deltaTime).  Then numRecursive = N + 1 "filter bands" are created.  For each filter band n = 0,N  the data samples are processed through a simple recursive filter backwards from the current sample, and picking statistics and characteristic function are generated.  Picks are generated based on the maximum of the characteristic funciton values over all filter bands relative to the threshold values threshold1 and threshold2.
        const double longTermWindow, // FilterPicker5 long term window
        // _DOC_ the long term window (longTermWindow) determines: a) a stabilisation delay time after the beginning of data; before this delay time picks will not be generated. b) the decay constant of a simple recursive filter to accumlate/smooth all picking statistics and characteristic functions for all filter bands.
        const double threshold1, // FilterPicker5 threshold1
        // _DOC_ threshold1 sets the threshold to trigger a pick event (potential pick).  This threshold is reached when the (clipped) characteristic function for any filter band exceeds threshold1.
        const double threshold2, // FilterPicker5 threshold1
        // _DOC_ threshold2 sets the threshold to declare a pick (pick will be accepted when tUpEvent reached).  This threshold is reached when the integral of the (clipped) characteristic function for any filter band over the window tUpEvent exceeds threshold2 * tUpEvent (i.e. the average (clipped) characteristic function over tUpEvent is greater than threshold2)..
        const double tUpEvent, // FilterPicker5 tUpEvent
        // _DOC_ tUpEvent determines the maximum time the integral of the (clipped) characteristic function is accumlated after threshold1 is reached (pick event triggered) to check for this integral exceeding threshold2 * tUpEvent (pick declared).

        FilterPicker5_Memory** pmem, // memory structure/object
        // _DOC_ pointer to a memory structure/object is used so that this function can be called repetedly for packets of data in sequence from the same channel.
        // The calling routine is responsible for managing and associating the correct mem structures/objects with each channel.  On first call to this function for each channel set mem = NULL.
        BOOLEAN_INT useMemory, // set to TRUE_INT=1 if function is called for packets of data in sequence, FALSE_INT = 0 otherwise

        PickData*** ppick_list, // returned pointer to array of num_picks PickData structures/objects containing picks
        int* pnum_picks, // the number of picks in array *ppick_list
        char* channel_id // a string identifier for the data channel
        ) {

    int k;


    // _DOC_ =============================
    // _DOC_ apply algoritm

    // initialize memory object
    FilterPicker5_Memory* mem = *pmem;
    if (mem == NULL) {
        mem = init_filterPicker5_Memory(deltaTime, sample, num_samples, filterWindow, longTermWindow, threshold1, threshold2, tUpEvent);
    }

    // create array for time-series results
    float* sampleNew = NULL;
    if (resultType == TRIGGER || resultType == CHAR_FUNC) {
        sampleNew = calloc(num_samples, sizeof (float));
        //sampleNew[0] = sample[num_samples - 1] = 0.0f;
    }

    // _DOC_ set clipped limit of maximum char funct value to 5 * threshold1 to avoid long recovery time after strong events
    double maxCharFunctValue = 5.0 * threshold1;


    // _DOC_ =============================
    // _DOC_ loop over all samples
    BOOLEAN_INT error1_not_printed = TRUE_INT;
    double charFunctValueTrigger = -1.0; // AJL 20091216
    int indexUpEventTrigger = -1;
    int indexUncertaintyPick = -1;
    int n;
    for (n = 0; n < num_samples; n++) {

        BOOLEAN_INT acceptedPick = FALSE_INT;

        // _DOC_ update index of nTUpEvent length up event window buffers
        int upEventBufPtrLast = mem->upEventBufPtr;
        mem->upEventBufPtr = (mem->upEventBufPtr + 1) % mem->nTUpEvent;

        // _DOC_ =============================
        // _DOC_ characteristic function is  (E2 - mean_E2) / mean_stdDev_E2
        // _DOC_    where E2 = (filtered band value current - filtered band value previous)**2
        // _DOC_    where value previous is taken futher back for longer filter bands
        double charFunct = 0.0;
        double charFunctClipped = 0.0;
        // _DOC_ evaluate current signal values
        double currentSample = sample[n];
        // _DOC_ filters are applied to first difference of signal values
        double currentDiffSample = currentSample - mem->lastSample;
        double currentFilteredSample;
        // _DOC_ loop over numRecursive filter bands
        for (k = mem->numRecursive - 1; k >= 0; k--) {
            // _DOC_  apply two single-pole HP filters
            // _DOC_  http://en.wikipedia.org/wiki/High-pass_filter    y[i] := α * (y[i-1] + x[i] - x[i-1])
            currentFilteredSample = mem->highPassConst[k] * (mem->filteredSample[k][0] + currentDiffSample);
            double currentDiffSample2 = currentFilteredSample - mem->filteredSample[k][0];
            mem->filteredSample[k][0] = currentFilteredSample;
            currentFilteredSample = mem->highPassConst[k] * (mem->filteredSample[k][1] + currentDiffSample2);
            mem->filteredSample[k][1] = currentFilteredSample;
            // _DOC_  apply one single-pole LP filter
            // _DOC_  http://en.wikipedia.org/wiki/Low-pass_filter    y[i] := y[i-1] + α * (x[i] - y[i-1])
            currentFilteredSample = mem->filteredSample[k][2] + mem->lowPassConst[k] * (currentFilteredSample - mem->filteredSample[k][2]);
            mem->lastFilteredSample[k] = mem->filteredSample[k][2];
            mem->filteredSample[k][2] = currentFilteredSample;
            double dy = currentFilteredSample;
            /* TEST */ //
            mem->test[k] = dy;
            //
            mem->xRec[k] = dy * dy;
            double charFunctClippedTest = 0.0; // AJL 20091214
            if (mem->mean_stdDev_xRec[k] <= DOUBLE_MIN_VALUE) {
                if (mem->enableTriggering && error1_not_printed) {
//TU                    sprintf(message_str, "WARNING: %s: mem->mean_stdDev_xRec[k] <= Float.MIN_VALUE (this should not happen! - dead trace?)\n", channel_id);
//TU                    info(message_str);
//TU                    error1_not_printed = FALSE_INT;
                }
            } else {
                double charFunctTest = (mem->xRec[k] - mem->mean_xRec[k]) / mem->mean_stdDev_xRec[k];
                charFunctClippedTest = charFunctTest; // AJL 20091214
                // _DOC_ limit maximum char funct value to avoid long recovery time after strong events
                if (charFunctClippedTest > maxCharFunctValue) {
                    charFunctClippedTest = maxCharFunctValue;
                    // save corrected mem->xRec[k]
                    mem->xRec[k] = maxCharFunctValue * mem->mean_stdDev_xRec[k] + mem->mean_xRec[k];
                }
                // _DOC_ characteristic function is maximum over numRecursive filter bands
                if (charFunctTest >= charFunct) {
                    charFunct = charFunctTest;
                    charFunctClipped = charFunctClippedTest;
                    mem->charFuntNumRecursiveIndex[mem->upEventBufPtr] = k;
                }
                // _DOC_ trigger index is highest frequency with CF >= threshold1 over numRecursive filter bands
                if (charFunctTest >= threshold1) {
                    mem->charFuntNumRecursiveIndex[mem->upEventBufPtr] = k;
                }
            }
            // AJL 20091214
            // _DOC_ =============================
            // _DOC_ update uncertainty and polarity fields
            // _DOC_ uncertaintyThreshold is at minimum char function or char funct increases past uncertaintyThreshold
            mem->charFunctUncertainty[k] = charFunctClippedTest; // no smoothing
            // AJL 20091214 mem->charFunctLast = charFunctClipped;
            BOOLEAN_INT upCharFunctUncertainty =
                    ((mem->charFunctUncertaintyLast[k] < mem->uncertaintyThreshold[k]) && (mem->charFunctUncertainty[k] >= mem->uncertaintyThreshold[k]));
            mem->charFunctUncertaintyLast[k] = mem->charFunctUncertainty[k];
            // _DOC_ each time characteristic function rises past uncertaintyThreshold store sample index and initiate polarity algoirithm
            if (upCharFunctUncertainty) {
                mem->indexUncertainty[k][mem->upEventBufPtr] = n - 1;
            } else {
                mem->indexUncertainty[k][mem->upEventBufPtr] = mem->indexUncertainty[k][upEventBufPtrLast];
            }
            // END - AJL 20091214
            if (upCharFunctUncertainty) {
                // _DOC_ initialize polarity algorithm, uses derivative of signal
                mem->polarityDerivativeSum[k][mem->upEventBufPtr] = 0.0;
                mem->polaritySumAbsDerivative[k][mem->upEventBufPtr] = 0.0;
            } else {
                mem->polarityDerivativeSum[k][mem->upEventBufPtr] = mem->polarityDerivativeSum[k][upEventBufPtrLast];
                mem->polaritySumAbsDerivative[k][mem->upEventBufPtr] = mem->polaritySumAbsDerivative[k][upEventBufPtrLast];
            }
            // _DOC_   accumulate derivative and sum of abs of derivative for polarity estimate
            // _DOC_   accumulate since last indexUncertainty
            double polarityderivativeIncrement = mem->filteredSample[k][2] - mem->lastFilteredSample[k];
            mem->polarityDerivativeSum[k][mem->upEventBufPtr] += polarityderivativeIncrement;
            mem->polaritySumAbsDerivative[k][mem->upEventBufPtr] += fabs(polarityderivativeIncrement);
        }


        // _DOC_ =============================
        // _DOC_ trigger and pick logic
        // _DOC_ only apply trigger and pick logic if past stabilisation time (longTermWindow)
        if (mem->enableTriggering || mem->nTotal++ > mem->indexEnableTriggering) { // past stabilisation time

            mem->enableTriggering = TRUE_INT;

            // _DOC_ update charFunctClipped values, subtract oldest value, and save provisional current sample charFunct value
            // _DOC_ to avoid spikes, do not use full charFunct value, may be very large, instead use charFunctClipped
            mem->integralCharFunctClipped[mem->upEventBufPtr] =
                    mem->integralCharFunctClipped[upEventBufPtrLast] - mem->charFunctClippedValue[mem->upEventBufPtr] + charFunctClipped;
            mem->charFunctClippedValue[mem->upEventBufPtr] = charFunctClipped;
            mem->charFunctValue[mem->upEventBufPtr] = charFunct;

            // _DOC_ if new picks allowd, check if integralCharFunct over last tUpEvent window is greater than threshold
            if (mem->allowNewPickIndex != INT_UNSET && mem->integralCharFunctClipped[mem->upEventBufPtr] >= mem->criticalIntegralCharFunct) {

                // _DOC_ find last point in tUpEvent window where charFunct rose past threshold1 and integralCharFunct greater than threshold back to this point
                int m = mem->upEventBufPtr;
                double integralCharFunctClippedWindow = mem->charFunctClippedValue[m];
                int k = 0;
                while (k++ < mem->nTUpEvent - 1 && n - k > mem->allowNewPickIndex) {
                    m--;
                    if (m < 0) {
                        m += mem->nTUpEvent;
                    }
                    integralCharFunctClippedWindow += mem->charFunctClippedValue[m];
                    if (mem->charFunctValue[m] >= threshold1) {
                        int l = m - 1;
                        if (l < 0) {
                            l += mem->nTUpEvent;
                        }
                        if (mem->charFunctValue[l] < threshold1) {
                            // integralCharFunct is integralCharFunct from current point back to point m
                            if (integralCharFunctClippedWindow >= mem->criticalIntegralCharFunct) {
                                acceptedPick = TRUE_INT;
                                // _DOC_ save characteristic function value as indicator of pick strenth
                                charFunctValueTrigger = mem->charFunctValue[m]; // AJL 20091216
                                mem->triggerNumRecursiveIndex = mem->charFuntNumRecursiveIndex[m];
                                // _DOC_ set index for pick uncertainty begin and end
                                indexUpEventTrigger = n - k;
                                indexUncertaintyPick = mem->indexUncertainty[mem->triggerNumRecursiveIndex][m]; // AJL 20091214
                                // _DOC_ evaluate polarity based on accumulated derivative
                                // _DOC_    (=POS if derivative_sum > 0, = NEG if derivative_sum < 0,
                                // _DOC_     and if ratio larger abs derivative_sum / abs_derivative_sum > 0.667,
                                // _DOC_     =UNK otherwise)
                                int iPolarity = m + 1; // evaluate polarity at 1 point past trigger point
                                if (iPolarity >= mem->nTUpEvent) {
                                    iPolarity -= mem->nTUpEvent;
                                }
                                mem->pickPolarity = POLARITY_UNKNOWN;
                                if (mem->polarityDerivativeSum[mem->triggerNumRecursiveIndex][iPolarity] > 0.0 &&
                                        mem->polarityDerivativeSum[mem->triggerNumRecursiveIndex][iPolarity] / mem->polaritySumAbsDerivative[mem->triggerNumRecursiveIndex][iPolarity] > 0.667) {
                                    mem->pickPolarity = POLARITY_POS;
                                } else if (mem->polarityDerivativeSum[mem->triggerNumRecursiveIndex][iPolarity] < 0.0 &&
                                        -mem->polarityDerivativeSum[mem->triggerNumRecursiveIndex][iPolarity] / mem->polaritySumAbsDerivative[mem->triggerNumRecursiveIndex][iPolarity] > 0.667) {
                                    mem->pickPolarity = POLARITY_NEG;
                                }
                                info(message_str);
                                mem->allowNewPickIndex = INT_UNSET;
                                info(message_str);
                                break;
                            }
                        }
                    }
                }
            }

            // _DOC_ if no pick, check if charFunctUncertainty has dropped below threshold maxAllowNewPickThreshold to allow new picks
            if (!acceptedPick && mem->allowNewPickIndex == INT_UNSET) { // no pick and no allow new picks
                // AJL 20091214
                int k = 0;
                for (; k < mem->numRecursive; k++) {
                    if (mem->charFunctUncertainty[k] > mem->maxAllowNewPickThreshold) // do not allow new picks
                    {
                        break;
                    }
                }
                if (k == mem->numRecursive) {
                    mem->allowNewPickIndex = n;
                }
                // END AJL 20091214
            }
        }


        // _DOC_ =============================
        // _DOC_ update "true", long-term statistic based on current signal values based on long-term window
        // long-term decay formulation
        // _DOC_ update long-term means of x, dxdt, E2, var(E2), uncertaintyThreshold
        for (k = 0; k < mem->numRecursive; k++) {
            mem->mean_xRec[k] = mem->mean_xRec[k] * mem->longDecayConst + mem->xRec[k] * mem->longDecayFactor;
            double dev = mem->xRec[k] - mem->mean_xRec[k];
            mem->mean_var_xRec[k] = mem->mean_var_xRec[k] * mem->longDecayConst + dev * dev * mem->longDecayFactor;
            // _DOC_ mean_stdDev_E2 is sqrt(long-term mean var(E2))
            mem->mean_stdDev_xRec[k] = sqrt(mem->mean_var_xRec[k]);
            mem->uncertaintyThreshold[k] = mem->uncertaintyThreshold[k] * mem->longDecayConst + mem->charFunctUncertainty[k] * mem->longDecayFactor;
            if (mem->uncertaintyThreshold[k] > mem->maxUncertaintyThreshold) {
                mem->uncertaintyThreshold[k] = mem->maxUncertaintyThreshold;
            } else if (mem->uncertaintyThreshold[k] < mem->minUncertaintyThreshold) {
                mem->uncertaintyThreshold[k] = mem->minUncertaintyThreshold;
            }
        }


        // _DOC_ =============================
        //  _DOC_ act on result, save pick if pick accepted at this sample

        if (resultType == TRIGGER) { // show triggers
            if (acceptedPick) {
                sampleNew[n] = 1.0f;
            } else {
                sampleNew[n] = 0.0f;
            }
            // TEST...
            //sampleNew[n] = (float) mem->test[0];
            //sampleNew[n] = (float) mem->test[index_recursive];
            //sampleNew[n] = (float) mem->test[mem->numRecursive - 1];
            //
        } else if (resultType == CHAR_FUNC) { // show char function
            sampleNew[n] = (float) charFunctClipped;
        } else { // generate picks
            // PICK
            if (acceptedPick) {
                // _DOC_ if pick accepted, save pick time, uncertainty, strength (integralCharFunct) and polarity
                // _DOC_    pick time is at uncertainty threshold (characteristic function rose past
                // _DOC_       uncertaintyThreshold): indexUncertaintyPick
                // _DOC_    trigger time (characteristic function >= threshold1): indexUpEventTrigger
                // _DOC_    pick begin is pick time - (trigger time - uncertainty threshold)
                int indexBeginPick = indexUncertaintyPick - (indexUpEventTrigger - indexUncertaintyPick);
                int indexEndPick = indexUpEventTrigger;
                double triggerPeriod = mem->period[mem->triggerNumRecursiveIndex];
                // check that uncertainty range is >= triggerPeriod / 20.0  // 20101014 AJL
                double uncertainty = deltaTime * ((double) (indexEndPick - indexBeginPick));
                if (uncertainty < triggerPeriod / 20.0) {
                    int ishift = (int) (0.5 * (triggerPeriod / 20.0 - uncertainty) / deltaTime);
                    // advance uncertainty index
                    indexBeginPick -= ishift;
                    // delay trigger index
                    indexEndPick += ishift;
                }
                PickData* pickData = init_PickData();
                set_PickData(pickData, (double) indexBeginPick, (double) indexEndPick,
                        mem->pickPolarity, charFunctValueTrigger, // AJL 20091216
                        CHAR_FUNCT_AMP_UNITS, triggerPeriod);
                addPickToPickList(pickData, ppick_list, pnum_picks);

            }
        }


        mem->lastSample = currentSample;
        mem->lastDiffSample = currentDiffSample;

    }


    if (useMemory) {
        // corect memory index values for sample length
        int i;
        for (i = 0; i < mem->nTUpEvent; i++) {
            // AJL 20091214
            for (k = 0; k < mem->numRecursive; k++) {
                mem->indexUncertainty[k][i] -= num_samples;
            }
            // END - AJL 20091214
        }
        if (mem->allowNewPickIndex != INT_UNSET) {
            mem->allowNewPickIndex -= num_samples;
        }
    } else {
        free_FilterPicker5_Memory(&mem);
        mem = NULL;
    }
    *pmem = mem;


    if (resultType == TRIGGER || resultType == CHAR_FUNC) {
        int n;
        for (n = 0; n < num_samples; n++)
            sample[n] = sampleNew[n];
    }
    if (sampleNew != NULL)
        free(sampleNew);


}

/*
 * This file is part of the Anthony Lomax C Library.
 *
 * Copyright (C) 2008 Anthony Lomax <anthony@alomax.net www.alomax.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */



// AJL: based on TestPicker4.java, 2008.07.14




/** picker memory class */

PickData* init_PickData() {

	PickData* pickData = calloc(1, sizeof(PickData));

	pickData->polarity = POLARITY_UNKNOWN;
	pickData->indices[0] = pickData->indices[1] = -1;
	pickData->amplitude = 0.0;
	pickData->amplitudeUnits = NO_AMP_UNITS;
	pickData->period = 0.0;

	return(pickData);

}


/** picker memory class */

void set_PickData(PickData* pickData, double index0, double index1, int polarity, double amplitude, int amplitudeUnits, double period) {

        pickData->indices[0] = index0;
        pickData->indices[1] = index1;
        pickData->polarity = polarity;
        pickData->amplitude = amplitude;
        pickData->amplitudeUnits = amplitudeUnits;
        pickData->period = period;

}



/** clean up pick memory */

void free_PickData(PickData* pickData)
{
	if (pickData == NULL)
		return;

	free(pickData);

}





/** print data */

int fprintf_PickData(PickData* pickData, FILE* pfile)
{
	if (pfile == NULL || pickData == NULL)
		return(0);

	fprintf(pfile, "%d %f %f %f %d %f ",
		pickData->polarity, pickData->indices[0], pickData->indices[1], pickData->amplitude, pickData->amplitudeUnits, pickData->period
	       );


	return(1);

}



/** print in NLLOC_OBS format */

char* printNlloc(char* pick_str, PickData* pickData, double dt, char* label, char* inst, char* comp, char* onset,
		 char* phase, int year, int month, int day, int hour, int min, double sec) {


	// first motion
	char first_mot[16];
	strcpy(first_mot, "?");
	if (pickData->polarity == POLARITY_POS)
		strcpy(first_mot, "+");
	if (pickData->polarity == POLARITY_NEG)
		strcpy(first_mot, "-");

	// add pick time to time
	sec += dt * (pickData->indices[0] + pickData->indices[1]) / 2.0;
	while (sec >= 60.0) {
		min++;
		sec-= 60.0;
	}
	while (min >= 60) {
		hour++;
		min-= 60;
	}

	// code data and time integers
	long int idate, ihrmin;
	idate = year * 10000 + month * 100 + day;
	ihrmin = hour * 100 + min;

	// error
	char error_type[] = "GAU";
        // set uncertainty to half width between right and left indices
	double error = dt * fabs(pickData->indices[1] - pickData->indices[0]);
	error /= 2.0;
	if (error < 0.0) {
		error = 0.0;
        }

	// misc
	double coda_dur = 0.0;
	double amplitude = pickData->amplitude;
	double period = pickData->period;
	//double apriori_weight = 1.0;

	// write observation part of FORMAT_PHASE_2 phase line
	//sprintf(pick_str,
	//	"%-6s %-4s %-4s %-1s %-6s %-1s %8.8ld %4.4ld %9.4lf %-3s %9.2le %9.2le %9.2le %9.2le %9.4lf",
	// write observation part of orig NLL phase line
	sprintf(pick_str,
		"%-6s %-4s %-4s %-1s %-6s %-1s %8.8ld %4.4ld %9.4lf %-3s %9.3le %9.3le %9.3le %9.3le",
			label,
			inst,
			comp,
			onset,
			phase,
			first_mot,
			/*quality, */
			idate, ihrmin,
			sec,
			error_type, error,
			coda_dur,
			amplitude,
			period//,
			//apriori_weight
		);

	return(pick_str);

}



/** add a PickData to a PickData list */

#define SIZE_INCREMENT 16

void addPickToPickList(PickData* pickData, PickData*** ppick_list, int* pnum_picks) {

	PickData** newPickList = NULL;

	if (*pnum_picks == 0 || *ppick_list == NULL) {		// list not yet created
		*ppick_list = calloc(SIZE_INCREMENT, sizeof(PickData*));
	}
	else if ((*pnum_picks % SIZE_INCREMENT) == 0) {	// list will be too small
		newPickList = calloc(*pnum_picks + SIZE_INCREMENT, sizeof(PickData*));
		int n;
		for (n = 0; n < *pnum_picks; n++)
			newPickList[n] = (*ppick_list)[n];
		free(*ppick_list);
		*ppick_list = newPickList;
	}

	// add PickData
	(*ppick_list)[*pnum_picks] = pickData;
	(*pnum_picks)++;

}





/** clean up pick list memory */

void free_PickList(PickData** pick_list, int num_picks)
{
	if (pick_list == NULL || num_picks < 1)
		return;

	int n;
	for (n = 0; n < num_picks; n++)
		free_PickData(*(pick_list + n));

	free(pick_list);

}












