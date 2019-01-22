/* This program converts one station up to 3 component Ismes data file to
Seisan event tes file.*/

#include<stdio.h>
#include<string.h>
#include<iostream.h>
#include<stdlib.h>
#include<math.h>

/* here are introduced those functions that are used by main or other
functions */

void make_event(FILE *Feve,FILE *Ftes);
int nof_station(FILE *Feve,int n_of_ch,long shba);
void chaninfo_1(FILE *Feve,int *n_of_ch,long *pr_tr_samp,long *nof_samp,int *nof_samp_block,long *fsc_pvalue,float *sr,float *time,long *tmcorr);
void chaninfo_2(FILE *Feve,int *n_of_ch,long *shba,char name[4],char *sunit,char *sdir,long *lat,long *lon,long *alt,long *gain,long *motor);
void frsquare(FILE *Ftes,int num);
void mark1(FILE *Ftes);
void mark2(FILE *Ftes);
void mark3(FILE *Ftes);
void mark4(FILE *Ftes);
int DOY(char *Y,char *M,char *D);
int re_bl_size(int cycle ,int bsize ,long nof_points);
void timecorr(char *h,char *m,char *s,long *tmcorr,char *newh,char *newm,float *news);

main(int argc,char *argv[])
{
/* main function make interactive connection to operator to get the input
and output files and checks its validity and creating data is passed to
another function make_event().*/

char evefile[80],tesfile[80];
FILE *Feve,*Ftes;

if(argc>1)
strcpy(evefile,argv[1]);
else
/* program asks for input file */
{
cout<<"Enter an Ismes event file name (no extension): ";
gets(evefile);
}
strcat(evefile,".eve");
Feve=fopen(evefile,"rb+");
if(Feve==NULL)
  {
  cout<<"no such file available.\n";
  exit(0);
  }

if(argc>1)
strcpy(tesfile,argv[1]);
else
{
/* program asks for output file */
cout<<"Enter Seisan Tes file name (no extension): ";
gets(tesfile);
}
strcat(tesfile,".tes");
Ftes=fopen(tesfile,"wb+");
if(Ftes==NULL)
  {
  cout<<"it is not possible to create seisan tes file.\n";
  exit(0);
  }

/* program sends two file pointers to function make_event() */
make_event(Feve,Ftes);

/* here finishes the main function task */
fcloseall();
return 0;

}




/**********************************************************************/

  void make_event(FILE *Feve,FILE *Ftes)
  {

  /* introducing type of variables used in this function */

  int fhbl,shbl; //lenght of first header block in bytes and second one.
  long shba; //second header block address in bytes.
  int n_of_ch; //number of channels.
  long fsadd; //first sample address in bytes.
  char Y_,Y,M,D,h,m,s; //year, month, day, hour, min and second.
  char newh,newm; //these are corrected hour and minute for each channel
  float news; //this is the corrected second for each channel
  char century; //century marker 0 for less than 2000 else 1
  int DOY_; //day of year
  long srmant; //sampling rate mantissa.
  char srexpo; //sampling rate exponent.
  int u=1; //this variable is used for initial informations
  long pr_tr_samp; //number of pretrigger samples
  long nof_samp; //total number of samples
  int nof_samp_block; //number of samples in each block
  long fsc_pvalue; //full scale peak value in microvolts
  float sr,time; //sampling rate and time window
  long tmcorr; //time correction with respect to reference
  int c=32; // this is useable for creating free space in putc() function
  int ii1,ii2,ii3,ii4; //some integer counters needed for other parts
  int chan_count; //this variable counts numbers for channels in header

  char name[4]={' ',' ',' ',' '}; //channel name
  char sunit; //sensor unit
  char sdir; //sensor direction
  long lat,lon, alt; //sensor location
  long gain, motor; //recording system gain and sensor motor constant

  long x; //this number counts the address of starting of each block
  long data; //this variable is the character which is read and written

  /* this part are variables which are introduced for taking memory, these
  units are dynamic memories according to number of channels */
  int *n; // number of times needed to go to the beginning of blocks
  int *bsize; // block sizes for each individual channel
  long *nof_points; // number of points of data for each channel

  /* reading common data for all channels from event file */

  // a: first header block

  fseek(Feve,1L,0);
  fread(&fhbl,2,1,Feve);
  fseek(Feve,5L,0);
  fread(&shba,4,1,Feve);
  fseek(Feve,15L,0);
  fread(&n_of_ch,2,1,Feve);
  fseek(Feve,19L,0);
  fread(&Y,1,1,Feve);
  fread(&M,1,1,Feve);
  fread(&D,1,1,Feve);
  fread(&h,1,1,Feve);
  fread(&m,1,1,Feve);
  fread(&s,1,1,Feve);
  fread(&fsadd,4,1,Feve);

  // b: second header block

  fseek(Feve,long(shba),0);
  fread(&shbl,2,1,Feve);

/**********************************************************************/

  fsadd=fsadd&0x00ffffff;

/**********************************************************************/

  n=new int[n_of_ch];
  bsize=new int[n_of_ch];
  nof_points=new long[n_of_ch];

/**********************************************************************/

/* this loop creates information matrix of each channel, nof_points is
number of sample points for each individual cannel, bsize is block size
and n is number of blocks for each channel */
    for(ii1=1;ii1<=n_of_ch;++ii1)
      {
      chaninfo_1(Feve,&ii1,&pr_tr_samp,&nof_samp,&nof_samp_block,&fsc_pvalue,&sr,&time,&tmcorr);
      nof_points[ii1-1]=nof_samp;
      bsize[ii1-1]=nof_samp_block;
	if(nof_points[ii1-1]%bsize[ii1-1])
	n[ii1-1]=int((long)nof_points[ii1-1]/(long)bsize[ii1-1])+1;
	else
	n[ii1-1]=int((long)nof_points[ii1-1]/(long)bsize[ii1-1]);
      }

/**********************************************************************/

     if(Y>99)
     century=49;
     else
     century=48;

/**********************************************************************/

     DOY_=DOY(&Y,&M,&D);

/**********************************************************************/

     Y_=Y%100;

/**********************************************************************/

     if(n_of_ch<31)
     chan_count=30;
     else
     chan_count=n_of_ch-(n_of_ch%3)+3;

/**********************************************************************/

chaninfo_1(Feve,&u,&pr_tr_samp,&nof_samp,&nof_samp_block,&fsc_pvalue,&sr,&time,&tmcorr);
chaninfo_2(Feve,&u,&shba,name,&sunit,&sdir,&lat,&lon,&alt,&gain,&motor);

/**********************************************************************/

  mark1(Ftes);
  fprintf(Ftes,"   Iranian National Network   ");
  fprintf(Ftes,"%3d%c%2d ",n_of_ch,century,Y_);
  fprintf(Ftes,"%3d %2d %2d %2d %2d %2d.000 %9.3f",DOY_,M,D,h,m,s,time+(float)tmcorr/1000000.);
  frsquare(Ftes,11);
  mark1(Ftes);

  mark1(Ftes);
  frsquare(Ftes,80);
  mark1(Ftes);

  for(int ii6=1;ii6<=chan_count;++ii6)
    {
    if(ii6%3==1)
    mark1(Ftes);
    if(ii6<=n_of_ch)
      {
      chaninfo_1(Feve,&ii6,&pr_tr_samp,&nof_samp,&nof_samp_block,&fsc_pvalue,&sr,&time,&tmcorr);
      chaninfo_2(Feve,&ii6,&shba,name,&sunit,&sdir,&lat,&lon,&alt,&gain,&motor);
      fprintf(Ftes," %3s B  %c %7.2f %8.2f",name,sdir,(float)tmcorr/1000000.,time);
      }
    else
      frsquare(Ftes,26);
    if(ii6%3==0)
      {
      putc(c,Ftes);
      putc(c,Ftes);
      mark1(Ftes);
      }
    }

/* reading few data of first channel headers for common output header */
/**********************************************************************/
/*  u=nof_station(Feve,n_of_ch,shba);//this part is for test
  chaninfo_1(Feve,&n_of_ch,&pr_tr_samp,&nof_samp,&nof_samp_block,&fsc_pvalue,&sr,&time,&tmcorr);
  chaninfo_2(Feve,&n_of_ch,&shba,name,&sunit,&sdir,&lat,&lon,&alt,&gain,&motor);

  fseek(Feve,long(33+(n_of_ch-1)*27),0);     //THIS PART SHOULD BE OMITED
  fread(&srmant,3,1,Feve);
  fread(&srexpo,1,1,Feve);
  srmant=srmant&0x00ffffff;

  fprintf(Ftes,"%d %ld %d",fhbl,shba,n_of_ch);
  fprintf(Ftes," %d %d %d %d %d %d %ld %d",Y,M,D,h,m,s,fsadd,shbl);
  fprintf(Ftes," %ld %d %d %f \n",srmant,srexpo,u,time);
  fprintf(Ftes,"%ld %ld %d %ld %f\n",pr_tr_samp,nof_samp,nof_samp_block,fsc_pvalue,sr);
  fprintf(Ftes,"%s %c %c %ld %ld %ld %ld %ld",name,sunit,sdir,lat,lon,alt,gain,motor);
  mark1(Ftes);
  mark2(Ftes);
  frsquare(Ftes,12);
  mark3(Ftes);
  frsquare(Ftes,6);
  mark4(Ftes);
  fprintf(Ftes," %d",DOY(&Y,&M,&D));
  for(int ll=0;ll<n_of_ch;++ll)
  {
  fprintf(Ftes,"\n%ld %d %d",nof_points[ll],bsize[ll],n[ll]);
  }
  putc(c,Ftes); */
/**********************************************************************/

/*      in this part data and header of each channel is created       */

/**********************************************************************/

     for(ii2=1;ii2<=n_of_ch;++ii2)
     {
/*----->       <<<<<< header part of each channel >>>>>>        <-----*/

     chaninfo_1(Feve,&ii2,&pr_tr_samp,&nof_samp,&nof_samp_block,&fsc_pvalue,&sr,&time,&tmcorr);
     chaninfo_2(Feve,&ii2,&shba,name,&sunit,&sdir,&lat,&lon,&alt,&gain,&motor);
     timecorr(&h,&m,&s,&tmcorr,&newh,&newm,&news);
     mark2(Ftes);
     fprintf(Ftes,"%3s  B  %c%c%2d %3d %2d %2d ",name,sdir,century,Y_,DOY_,M,D);
     fprintf(Ftes,"%2d %2d %6.3f %7.2f %6ld ",newh,newm,news,sr,nof_samp);
     fprintf(Ftes,"%8.4f %9.4f %5ld 4   ",(float)lat/1000000,(float)lon/1000000,alt/1000);
     frsquare(Ftes,960);

/**********************************************************************/
     x=0;
     mark3(Ftes);
       for(ii3=1;ii3<=n[ii2-1];++ii3)
	 {
	 for(ii4=1;ii4<=n_of_ch;++ii4)
	   {
	   if((ii4==ii2)&&(re_bl_size(ii3,bsize[ii4-1],nof_points[ii4-1])!=0))
	     {
	     fseek(Feve,(long)(fsadd+x),0);
	     for(int ii5=0;ii5<re_bl_size(ii3,bsize[ii4-1],nof_points[ii4-1]);++ii5)
	       {
	       fread(&data,3,1,Feve);
	       if(data&0x00800000)
	       data=data|0xff000000;
	       else
	       data=data&0x00ffffff;
	       fwrite(&data,4,1,Ftes);
	       x+=3;
	       }
	     x+=4;
	     }
	   else if(re_bl_size(ii3,bsize[ii4-1],nof_points[ii4-1])!=0)
	   x+=((re_bl_size(ii3,bsize[ii4-1],nof_points[ii4-1]))*3+4);
	   }
	 }
       mark4(Ftes);
     }
   mark2(Ftes);
/**********************************************************************/

  delete n;
  delete bsize;
  delete nof_points;

/**********************************************************************/

  return;
  }

/**********************************************************************/

  /* this function computes the number of stations by comparing stations
  names in the second header block. */

  int nof_station(FILE *Feve,int n_of_ch,long shba)
  {
  int n=0;
  long stname=0;
  long stname1=0;
  for(int i=1;i<=n_of_ch;++i)
    {
    fseek(Feve,long(shba+8+(i-1)*30),0);
    fread(&stname,3,1,Feve);
      if(stname!=stname1) n++;
	for(int j=1;j<=4;++j)
	stname1=stname;
    }
  return n;
  }



/***********************************************************************/

  /* this function receives file name to be read, number of channel intended
  to be used and returns time window of this trace, also it points the
  addresses of pretrigger samples(pr_tr_samp), number of samples(nof_samp),
  number of samples in each data block(nof_samp_block), full scale peak value
  in microvolts(fsc_pvalue) and sampling rate(sr).*/

  void chaninfo_1(FILE *Feve,int *n_of_ch,long *pr_tr_samp,long *nof_samp,int *nof_samp_block,long *fsc_pvalue,float *sr,float *time,long *tmcorr)

  {
  long srmant; //sampling rate mantissa from Ismes file
  char srexpo; //sampling rate exponent from Ismes file

  fseek(Feve,long(33+(*n_of_ch-1)*27),0);
  fread(&srmant,3,1,Feve);
  fread(&srexpo,1,1,Feve);
  fread(tmcorr,4,1,Feve);
  fread(nof_samp_block,2,1,Feve);
  fread(nof_samp,4,1,Feve);
  fread(pr_tr_samp,4,1,Feve);
  fseek(Feve,1,1);
  fread(fsc_pvalue,4,1,Feve);

    srmant=srmant&0x00ffffff;
    if(srmant&0x00800000)
    {
    srmant=(srmant^0xffffffff)+1;
    *sr=1/((float)(srmant)*pow(2,srexpo));
    }
    else
    *sr=(float)srmant*pow(2,srexpo);
    *time=*nof_samp/(*sr);
    return;
  }




/*********************************************************************/

/* this function reads special informations of each individual channel consist
ing of channel name(name), sensor unit(sunit), sensor direction(sdir), latitude
(lat), longitude(lon), altitude(alt), system gain milivolt/volt(gain), motor
constant(motor). */

void chaninfo_2(FILE *Feve,int *n_of_ch,long *shba,char name[4],char *sunit,char *sdir,long *lat,long *lon,long *alt,long *gain,long *motor)
  {
  fseek(Feve,long(*shba+8+(*n_of_ch-1)*30),0);
  fgets(name,4,Feve);
  fseek(Feve,1,1);
  fread(sunit,1,1,Feve);
  fread(sdir,1,1,Feve);
  fread(lat,4,1,Feve);
  fread(lon,4,1,Feve);
  fread(alt,4,1,Feve);
  fseek(Feve,1,1);
  fread(gain,4,1,Feve);
  fread(motor,4,1,Feve);
  if(*sdir=='V') *sdir='Z';
  return;
  }




/*********************************************************************/

/* this function writes in specified file any number of free characters */
void frsquare(FILE *Ftes,int num)
  {
  int i,c=32;
  for(i=0;i<num;i++)
  putc(c,Ftes);
  return;
  }




/*********************************************************************/

/* this function returns some special kind of four characcters */
void mark1(FILE *Ftes)
  {
  long mark=80;
  fwrite(&mark,4,1,Ftes);
  return;
  }




/*********************************************************************/

/* this function returned some special kind of four characters */
void mark2(FILE *Ftes)
  {
  long mark=1040;
  fwrite(&mark,4,1,Ftes);
  return;
  }




/********************************************************************/

/* this function returns some special kind of eight characters */
void mark3(FILE *Ftes)
  {
  long mark=1040,marq=14644;
  fwrite(&mark,4,1,Ftes);
  fwrite(&marq,4,1,Ftes);
  return;
  }




/********************************************************************
/* this function returns some special kind of eight characters */
void mark4(FILE *Ftes)
  {
  long /*mark=1040,*/marq=14644;
  fwrite(&marq,4,1,Ftes);
//  fwrite(&mark,4,1,Ftes);
  return;
  }

/* this function gets number of year, month and day and computes the
number of day of that year */
int DOY(char *Y,char *M,char *D)
  {
  int x[12]={0,0,-3,-3,-4,-4,-5,-5,-5,-6,-6,-7};
  int daynum;
  char Y_;
  Y_=*Y+1900;
  if(Y_%400!=0&&Y_%100==0)
  daynum=(*M-1)*31+x[*M-1]+(*D);
  else if(Y_%4==0)
  daynum=(*M-1)*31+x[*M-1]+(*D)+1;
  else
  daynum=(*M-1)*31+x[*M-1]+(*D);
  return daynum;
  }

/* this function gets the current number of cycles for finding data block, also
size of initial block size and total number of data for that channel and then
finds real size of block for that channel in that specified cycle */
int re_bl_size(int cycle ,int bsize ,long nof_points)
  {
  int resize=0;
  if(int(nof_points/(long)bsize)>=cycle)
  resize=bsize;
  else if(int(nof_points/(long)bsize)+1==cycle)
  resize=int(nof_points%(long)bsize);
  else
  resize=0;
  return resize;
  }

/* this function makes real times for the first sample and returns that, which
will be used by make_event function to create each header for individual
channels */
void timecorr(char *h,char *m,char *s,long *tmcorr,char *newh,char *newm,float *news)
  {
  *news=(float)(*s)+((float)(*tmcorr)/1000000.);
  if(*news>=60)
    {
    *newm=(int)(*news/60)+(*m);
    *news=*news-((float)(*newm)-(float)(*m))*60.;
    if(*newm>=60)
      {
      *newh=(int)(*newm/60)+(*h);
      *newm=*newm-(*newh-(*h))*60;
      }
    else
    *newh=(*h);
    return;
    }

  if(*news<0)
    {
    *newm=(int)(*news/60)+(*m)-1;
    *news=(float)((*m)-(*newm))*60+(*news);
    if(*newm<0)
      {
      *newh=(int)(*newm/60)+(*h)-1;
      *newm=((*h)-(*newh))*60+(*newm);
      }
    else
    *newh=(*h);
    return;
    }

  *newm=(*m);
  *newh=(*h);
  return;
  }
