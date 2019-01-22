/* Moment Tensor Inversion */
/* Uses previously determined Green's functions in D. Helmbergers format*/

/* this version has included all c-routies used, all are linux versions 
   the graphics output is commented out so graphics routiens are not included
   changes can be found by searching jh (except malloc changes, see below)

   when using 500 input points, some memory overflow occurred (only under windows,
   linux ok), ran ok for 450 points. Since hard to find, all arguemnts to malloc 
   statements were multiplied by 2, then it worked 

   jh december 2011                               */
/* mar 26 2012 jh : change output to screen       */
/* apr 27 2012 pv : change depth to float         */
/* jul 26 2013 jh : put in gaute hope's (eg@gaute.vetsj.com) changes,*/ 
/*                  see july 13 */

#include"tdmt_invb.h"
#include "nrutil.h"  /*added july 13 */

 void gaussj();
 int correlate(),fclose();
 FILE *fopen(), *par,*fd, *fd1, *fd2, *fd3, *out, *synt;

main()
 {
 int i,j,l,N,Np,Z,k,ntr,nsta,vsize,nn,*npts,wflag=0;
 int cnt1, cnt2, cnt3, np2,QUAL,plotflag,WCNT;
 float depth;
 float *W, *tmpp,*tmp,*dt, cormax, mindist, dist,E,VR,junk;
 double **AIV,**AJ, **B;
 struct MOMENT M;
 struct DATA *ss;
 struct GREEN *gg;
/* jh changed size of line from 50 to 128, 50 was in conflict with reading 100
   char below */
 char infile[50],line[128];
 /**Variables needed for implementation of R Urhammers MT decomposition**/
 /**Algorythms**/
 char d_axis[3];
 int d_mtrep;
 double d_mf[6],d_mt[3][3],d_d[3],d_v[3][3],d_miso[3][3],d_mdc[3][3],
	d_mclvd[3][3],d_m0,d_mw,d_plunge[3],d_azimuth[3],d_strike[3],
	d_slip[3],d_dip[3],d_pcdc,d_pcclvd,d_pciso;
 float gfscale=1.0e+20; /*Dyne cm*/
 /**************/
 float Mo, Mw, Strike, Rake, Dip, Pdc, Piso, Pclvd;
 float St2, Rk2, Dp2;
 char extra[80];
 int isoflag=M.isoflag=5;

par=fopen("mt_inv.in","r");
/* jh changed to overwrite*/

out=fopen("mt_inv_redi.out","w");

/* jh remove synt.out if there*/
remove("synt.out");


fscanf(par,"%d %f %d %d\n",&nsta,&depth,&wflag,&plotflag);
fprintf(out,"Depth=%7.3f\n",depth);
fprintf(stderr,"Depth=%7.3f\n",depth);

AIV=(double **)malloc(sizeof(double *)*10);
for(i=0 ; i < 5 ; i++)
 AIV[i]=(double *)malloc(sizeof(double)*10);

 npts=(int *)malloc(sizeof(int)*20);
 dt  =(float *)malloc(sizeof(float)*20);


B=(double **)malloc(sizeof(double *)*10);
for(i=0 ; i < 5; i++)
 B[i]=(double *)malloc(sizeof(double)*2);


ss=(struct DATA *)malloc(sizeof(struct DATA)*(4*nsta));
gg=(struct GREEN *)malloc(sizeof(struct GREEN)*(4*nsta));



  mindist=100000.0;
  WCNT=0;
  for(i=0 ; i < nsta ; i++)
   {
   fscanf(par,"%s %f %f %d %d\n",infile,&dist,&(ss[i].azi),&Z,&Np);
   WCNT += Np;
   strcpy(ss[i].name,infile);
   ss[i].azi *= PI/180.0;
   fd=fopen(infile,"r");
   fgets(line,100,fd);
/*cjh  extra to read extra infor on line, probably not needed*/
   sscanf(line,"%d %s",&N,extra);

  
   fgets(line,100,fd);
   fgets(line,100,fd);
   fgets(line,100,fd);
   sscanf(line,"%d %f",&vsize,&junk);
   fclose(fd);

   tmp=(float *)malloc(6*vsize*sizeof(float));
/*   fprintf(stderr,"Read seismograms\n"); */
   readhelm(infile,&ntr,npts,dt,tmp);      /*Open and Read Seismograms*/
/*   fprintf(stderr,"Finished reading seismograms\n"); */
   nn=npts[0];

   if(mindist > dist) mindist = dist;
   ss[i].dist=dist;
   ss[i].np=nn;
   ss[i].dt=dt[0];
   ss[i].nn=Np;
   ss[i].zz=Z;


   /*Allocate data structure arrays*/
   ss[i].t=(float *)malloc(sizeof(float)*vsize*4);
   ss[i].r=(float *)malloc(sizeof(float)*vsize*4);
   ss[i].z=(float *)malloc(sizeof(float)*vsize*4);

   for(j=0 ; j < nn ; j++)
    {
    ss[i].t[j]=tmp[j];
    ss[i].r[j]=tmp[j + nn];
    ss[i].z[j]=tmp[j + 2*nn];
    }
   free(tmp);
   }

   /*Allocate Memory for Station Weights*/
   WCNT *= 3;
   W=(float *)malloc(sizeof(float)*WCNT*2);
         for(j=0; j < WCNT ; j++)
             W[j]=1.0;

   /*Allocate Memory for A matrix*/
   AJ=(double **)malloc(sizeof(double *)*10);
         for(j=0 ; j < 5 ; j++)
             AJ[j]=(double *)malloc(sizeof(double)*WCNT*2);

   for(i=0 ; i < nsta ; i++)
   {
   fscanf(par,"%s %d %d\n",infile,&Z,&Np);
   fd=fopen(infile,"r");
   fgets(line,100,fd);
   sscanf(line,"%d",&N);
   fgets(line,100,fd);
   fgets(line,100,fd);
   fgets(line,100,fd);
   sscanf(line,"%d %f",&vsize,&junk);
   fclose(fd);

   tmp=(float *)malloc(16*vsize*sizeof(float));
/*   fprintf(stderr,"Read Green\n"); */
   readhelm(infile,&ntr,npts,dt,tmp);    /*Open and Read Green's Func*/
   nn=npts[0];

   gg[i].np=nn;
   gg[i].dt=dt[0];
   gg[i].nn=Np;
   gg[i].zz=Z;

   /*Allocate Greens Function structure arrays*/
   gg[i].u1=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u2=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u3=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u4=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u5=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u6=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u7=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u8=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u9=(float *)malloc(sizeof(float)*vsize*4);
   gg[i].u10=(float *)malloc(sizeof(float)*vsize*4);

   for(j=0 ; j < nn ; j++)
    {
    gg[i].u1[j]=tmp[j];
    gg[i].u2[j]=tmp[j + nn];
    gg[i].u3[j]=tmp[j + 2*nn];
    gg[i].u4[j]=tmp[j + 3*nn];
    gg[i].u5[j]=tmp[j + 4*nn];
    gg[i].u6[j]=tmp[j + 5*nn] * (-1.0);/*Note the vertical GF's are*/ 
    gg[i].u7[j]=tmp[j + 6*nn] * (-1.0);/*flipped in earqt1.f and TW's*/
    gg[i].u8[j]=tmp[j + 7*nn] * (-1.0);/* Blackbox.f DVH conv. z + down*/
  if(isoflag==6)
      {
      gg[i].u9[j]=tmp[j + 8*nn];           /*Radial   exp*/
      gg[i].u10[j]=tmp[j + 9*nn];          /*Vertical exp note polarity*/
      }
  if(isoflag==5)
      {
      gg[i].u9[j]=0.0;
      gg[i].u10[j]=0.0;
      }
    }
   free(tmp);
   }

   /* Cross-Correlation to obtain zero shift*/
   for(i=0; i < nsta; i++)
     {
     N =ss[i].np;
     Np=ss[i].nn;
     Z =ss[i].zz;
     np2 = (int)(log10((float)N)/log10(2.0) + 0.5);

     np2=2<<(np2-1);
/*     printf("np %d\n",np2); jh */
     if(Z == 0)
       ss[i].zz=correlate(ss,gg,i,np2);/*Compute cross-correlation*/
                                       /*Return zshift for largest cor.*/


     }



/*Construct distance 1/R weighting*/
if(wflag==1 && (nsta >= 1))
  {
  fprintf(stderr,"Station Information\n");
  fprintf(out,"Station Information\n");
  l=0;
  for(i=0; i < nsta; i++)
     {
     cormax = ss[i].dist / mindist;
     fprintf(stderr,"Station(%d): %s  R=%.1fkm  AZI=%.1f  W=%.3f  Zcor=%d\n", i, ss[i].name,ss[i].dist,ss[i].azi*180.0/PI,cormax,ss[i].zz);
     fprintf(out,"Station(%d): %s  R=%.1fkm  AZI=%.1f  W=%.3f  Zcor=%d\n", i, ss[i].name,ss[i].dist,ss[i].azi*180.0/PI,cormax,ss[i].zz);
     N = ss[i].nn;
     for(j=0; j < 3*N; j++)
        W[l++]=cormax;

     }
  }
       


   /* INVERSION ROUTINE */

    for(i=0 ; i < 5 ; i++)                /*Normalize AtA and AIV matix*/
     for(l=0 ; l < 5 ; l++)
       AIV[i][l]=0.0;

    for(i=0 ; i < 5 ; i++)
      B[i][0]=0.0;

    cnt1=cnt2=cnt3=0;
    for(i=0; i < nsta; i++)
       {
       Np=ss[i].nn;
       Z =gg[i].zz;
       cnt1=cnt2 = cnt3;
       cnt2 += Np;
       cnt3 += 2*Np;
       for(j=Z; j < Z+Np; j++)                 /*Index over time*/
	 {
         /*Mxx term*/
	 AJ[0][cnt1]        = (double)(0.5*sin(2*ss[i].azi)*gg[i].u1[j]);
	 AJ[0][cnt2]        = (double)(0.5*(gg[i].u5[j] - cos(2*ss[i].azi)*gg[i].u3[j]));
	 AJ[0][cnt3]        = (double)(0.5*(gg[i].u8[j] - cos(2*ss[i].azi)*gg[i].u6[j]));

         /*Myy term*/
	 AJ[1][cnt1]        = (double)((-0.5)*sin(2*ss[i].azi)*gg[i].u1[j]);
	 AJ[1][cnt2]        = (double)(0.5*(gg[i].u5[j] + cos(2*ss[i].azi)*gg[i].u3[j]));
	 AJ[1][cnt3]        = (double)(0.5*(gg[i].u8[j] + cos(2*ss[i].azi)*gg[i].u6[j]));

         /*Mxy term*/
	 AJ[2][cnt1]        = (double)((-1.0)*cos(2*ss[i].azi)*gg[i].u1[j]);
	 AJ[2][cnt2]        = (double)((-1.0)*sin(2*ss[i].azi)*gg[i].u3[j]);
	 AJ[2][cnt3]        = (double)((-1.0)*sin(2*ss[i].azi)*gg[i].u6[j]);

         /*Mxz term*/
	 AJ[3][cnt1]        = (double)((-1.0)*sin(ss[i].azi)*gg[i].u2[j]);
	 AJ[3][cnt2]        = (double)(       cos(ss[i].azi)*gg[i].u4[j]);
	 AJ[3][cnt3]        = (double)(       cos(ss[i].azi)*gg[i].u7[j]);

         /*Myz term*/
	 AJ[4][cnt1]        = (double)(       cos(ss[i].azi)*gg[i].u2[j]);
	 AJ[4][cnt2]        = (double)(       sin(ss[i].azi)*gg[i].u4[j]);
	 AJ[4][cnt3]        = (double)(       sin(ss[i].azi)*gg[i].u7[j]);

	 cnt1++;
	 cnt2++;
	 cnt3++;
	 }
	 }




    for(i=0 ; i < 5 ; i++)                /*Compute AtA                */
     for(j=0 ; j < 5 ; j++)
      for(k=0 ; k < cnt3 ; k++)
	AIV[i][j] += AJ[i][k]* AJ[j][k] * (double)W[k];


    cnt1=cnt2=cnt3=0;
    tmp=(float *)malloc(20*N*vsize*sizeof(float));
    for(j=0; j < nsta; j++)
      {
      l=0;
      Z =ss[j].zz;
      Np=ss[j].nn;
      cnt1=cnt2 = cnt3;
      cnt2 += Np;
      cnt3 += 2*Np;
      for(i=Z ; i < Np+Z ; i++)
        {
        tmp[cnt1]        = ss[j].t[i];
        tmp[cnt2]        = ss[j].r[i];
        tmp[cnt3]        = ss[j].z[i];
	cnt1++;
	cnt2++;
	cnt3++;
        }
       }

    for(i=0 ; i < 5 ; i++)               /* Calculate Righthand Side */
      for(j=0 ; j < cnt3 ; j++)
         B[i][0] += AJ[i][j] * (double)tmp[j] * (double)W[j];


    minvdbl(AIV,B,5,1);                     /* Determine Solution Vector */

    M.mxx=(float)B[0][0];
    M.myy=(float)B[1][0];
    M.mxy=(float)B[2][0];
    M.mxz=(float)B[3][0];
    M.myz=(float)B[4][0];
    M.mzz=-1.0*(M.mxx + M.myy);

/*Call Bobs MT decomposition routines*/
/*The minus one is needed to map Helmbergers convention into Aki's*/
/*Jost and Hermann (1989) state that AKI's convention is -1*LANGSTONS*/
d_mtrep=1;
d_mt[0][0] = (double)(-1.0*gfscale*B[0][0]);
d_mt[0][1] = (double)(-1.0*gfscale*B[2][0]);
d_mt[0][2] = (double)(-1.0*gfscale*B[3][0]);
d_mt[1][0] = (double)(-1.0*gfscale*B[2][0]);
d_mt[1][1] = (double)(-1.0*gfscale*B[1][0]);
d_mt[1][2] = (double)(-1.0*gfscale*B[4][0]);
d_mt[2][0] = (double)(-1.0*gfscale*B[3][0]);
d_mt[2][1] = (double)(-1.0*gfscale*B[4][0]);
d_mt[2][2] = -1.0*(d_mt[0][0] + d_mt[1][1]);

fprintf(out,"Mxx=%.3f\nMxy=%.3f\nMxz=%.3f\nMyy=%.3f\nMyz=%.3f\nMzz=%.3f\n",
       d_mt[0][0]/gfscale,d_mt[0][1]/gfscale,d_mt[0][2]/gfscale,d_mt[1][1]/gfscale,
       d_mt[1][2]/gfscale,d_mt[2][2]/gfscale);

m0dcf_(&d_mtrep,d_mf,d_mt,d_d,d_v,d_miso,d_mdc,d_mclvd,
       &d_m0,&d_mw,d_axis,d_plunge,d_azimuth,d_strike,d_dip,
       d_slip,&d_pcdc,&d_pcclvd,&d_pciso);

Mo = (float) d_m0;
Mw = (float) d_mw;
Strike = (float) d_strike[0];
Rake   = (float) d_slip[0];
Dip    = (float) d_dip[0];
St2    = (float) d_strike[1];
Rk2    = (float) d_slip[1];
Dp2    = (float) d_dip[1];
Pdc    = (float) d_pcdc;
Pclvd  = (float) d_pcclvd;
Piso    = (float) d_pciso;

fprintf(stderr,"Mo=%g\nMw=%.1f\nStrike=%.0f ; %.0f\nRake=%.0f ; %.0f\nDip=%.0f; %.0f\nPdc=%.0f\nPclvd=%.0f\nPiso=%.0f\n",
       Mo,Mw,Strike,St2,Rake,Rk2,Dip,Dp2,Pdc,Pclvd,Piso);
fprintf(out,"Mo=%g\nMw=%.1f\nStrike=%.0f ; %.0f\nRake=%.0f ; %.0f\nDip=%.0f ; %.0f\nPdc=%.0f\nPclvd=%.0f\nPiso=%.0f\n",
       Mo,Mw,Strike,St2,Rake,Rk2,Dip,Dp2,Pdc,Pclvd,Piso);

fitcheck(ss,gg,W,M,Mo,nsta,isoflag,&E,&VR);  /*Function to compute vr and flag bad stations*/


/*if(plotflag==1)*/

  mt_plot(ss,gg,nsta,Strike,Rake,Dip,St2,Rk2,Dp2,M,d_mt,Pdc,Pclvd,Piso,Mo,Mw,E,VR);

/*********************/
fprintf(out,"Variance=%.3e\n",E);
fprintf(out,"VarRed=%.3e\n",VR);
fprintf(stderr,"Var/Pdc=%.3e\n",E/Pdc);
fprintf(out,"Var/Pdc=%.3e\n",E/Pdc);
if(VR < 20.0) QUAL=0;
if(VR > 20.0 && VR < 40.0) QUAL=1;
if(VR > 40.0 && VR < 60.0) QUAL=2;
if(VR > 60.0 && VR < 80.0) QUAL=3;
if(VR > 80.0) QUAL=4;
fprintf(out,"Quality=%d\n",QUAL);
fprintf(stderr,"Quality=%d\n",QUAL);


}


/**********************************  minvdbl_linux.c **************************/


 /*modified from original numerical recipes program for double
precision */

#include<stdio.h>
/*#define SWAP(a,b) {double temp=(a);(a)=(b);(b)=temp;}*/ /* july 13 */
void gaussj();
double **dsubmatrix();

minvdbl(x,y,n,m)
double **x,**y;
int n,m;
  {
  int i,j;
  double **p,**pp;

  p=dsubmatrix(x,0,n,0,n,1,1);
  pp=dsubmatrix(y,0,n,0,m,1,1);

  gaussj(p,n,pp,m);
  }

void gaussj(a,n,b,m)
double **a,**b;
int n,m;
{
	int *indxc,*indxr,*ipiv;
	int i,icol,irow,j,k,l,ll,*ivector();
	double big,dum,pivinv;
	void nrerror(),free_ivector();

	indxc=ivector(1,n);
	indxr=ivector(1,n);
	ipiv=ivector(1,n);
	for (j=1;j<=n;j++) ipiv[j]=0;
	for (i=1;i<=n;i++) {
		big=0.0;
		for (j=1;j<=n;j++)
			if (ipiv[j] != 1)
				for (k=1;k<=n;k++) {
					if (ipiv[k] == 0) {
						if (fabs(a[j][k]) >= big) {
							big=fabs(a[j][k]);
							irow=j;
							icol=k;
						}
					} else if (ipiv[k] > 1) nrerror("GAUSSJ: Singular Matrix-1");
				}
		++(ipiv[icol]);
		if (irow != icol) {
			for (l=1;l<=n;l++) SWAP(a[irow][l],a[icol][l])
			for (l=1;l<=m;l++) SWAP(b[irow][l],b[icol][l])
		}
		indxr[i]=irow;
		indxc[i]=icol;
		if (a[icol][icol] == 0.0) nrerror("GAUSSJ: Singular Matrix-2");
		pivinv=1.0/a[icol][icol];
		a[icol][icol]=1.0;
		for (l=1;l<=n;l++) a[icol][l] *= pivinv;
		for (l=1;l<=m;l++) b[icol][l] *= pivinv;
		for (ll=1;ll<=n;ll++)
			if (ll != icol) {
				dum=a[ll][icol];
				a[ll][icol]=0.0;
				for (l=1;l<=n;l++) a[ll][l] -= a[icol][l]*dum;
				for (l=1;l<=m;l++) b[ll][l] -= b[icol][l]*dum;
			}
	}
	for (l=n;l>=1;l--) {
		if (indxr[l] != indxc[l])
			for (k=1;k<=n;k++)
				SWAP(a[k][indxr[l]],a[k][indxc[l]]);
	}
	free_ivector(ipiv,1,n);
	free_ivector(indxr,1,n);
	free_ivector(indxc,1,n);
}

double **dsubmatrix(a,oldrl,oldrh,oldcl,oldch,newrl,newcl)
double **a;
int oldrl,oldrh,oldcl,oldch,newrl,newcl;
{
	int i,j;
	double **m;

	m=(double **) malloc((unsigned) (oldrh-oldrl+1)*sizeof(double*)*2);
        if (!m) fprintf(stderr,"allocation failure in submatrix()\n");
/* 	if (!m) nrerror("allocation failure in submatrix()"); 
 jh the commented out statemant did not compile, relaced with a print */   
	m -= newrl;

	for(i=oldrl,j=newrl;i<=oldrh;i++,j++) m[j]=a[i]+oldcl-newcl;

	return m;
}

void free_dsubmatrix(b,nrl,nrh,ncl,nch)
double **b;
int nrl,nrh,ncl,nch;
{
	free((char*) (b+nrl));
}


/*************************     correl2b_linux.c   ****************************/       

/*   jh comment out
#include"tdmt_invb.h"

                            */
void correl();
float *vector();

int correlate(ss,gg,i,np2)
    int i, np2;
    struct DATA   *ss;
    struct GREEN  *gg;
    {
    int j, l, Zcor[20], zvalue;
    float cormax[20], maximum=0.0;
    float *data1, *data2, *ans;


	data1=vector(1,np2);
	data2=vector(1,np2);
	ans  =vector(1,2*np2);

    for(j=0; j < 20; j++)  /*Initialize cormax for each component*/
	 {
	 Zcor[j]  =0;
         cormax[j]=0.0;
	 }

    for(j=0; j < np2; j++) /*Load Tangential Data*/
      if(j >= (ss[i].np))
        data1[j+1]=0.0;
      else
        data1[j+1]=ss[i].t[j];


    for(j=0; j < np2; j++) /*Load TSS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u1[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[0] < ABS(ans[j+1]))
	     {
	     cormax[0] = ABS(ans[j+1]);
	     Zcor[0] = j;
	     }
           if(maximum < cormax[0])
	     {
	     zvalue=Zcor[0];
	     maximum=cormax[0];
	     }

    for(j=0; j < np2; j++) /*Load TDS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u2[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[1] < ABS(ans[j+1]))
	     {
	     cormax[1] = ABS(ans[j+1]);
	     Zcor[1] = j;
	     }
    if(maximum < cormax[1])
	     {
	     zvalue=Zcor[1];
	     maximum=cormax[1];
	     }

    for(j=0; j < np2; j++) /*Load Radial Data*/
      if(j >= (ss[i].np))
        data1[j+1]=0.0;
      else
        data1[j+1]=ss[i].r[j];

    for(j=0; j < np2; j++) /*Load RSS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u3[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[2] < ABS(ans[j+1]))
	     {
	     cormax[2] = ABS(ans[j+1]);
	     Zcor[2] = j;
	     }
    if(maximum < cormax[2])
	     {
	     zvalue=Zcor[2];
	     maximum=cormax[2];
	     }

    for(j=0; j < np2; j++) /*Load RDS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u4[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[3] < ABS(ans[j+1]))
	     {
	     cormax[3] = ABS(ans[j+1]);
	     Zcor[3] = j;
	     }
    if(maximum < cormax[3])
	     {
	     zvalue=Zcor[3];
	     maximum=cormax[3];
	     }

    for(j=0; j < np2; j++) /*Load RDD*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u5[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[4] < ABS(ans[j+1]))
	     {
	     cormax[4] = ABS(ans[j+1]);
	     Zcor[4] = j;
	     }
    if(maximum < cormax[4])
	     {
	     zvalue=Zcor[4];
	     maximum=cormax[4];
	     }

    for(j=0; j < np2; j++) /*Load Vertical Data*/
      if(j >= (ss[i].np))
        data1[j+1]=0.0;
      else
        data1[j+1]=ss[i].z[j];

    for(j=0; j < np2; j++) /*Load ZSS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u6[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[5] < ABS(ans[j+1]))
	     {
	     cormax[5] = ABS(ans[j+1]);
	     Zcor[5] = j;
	     }
    if(maximum < cormax[5])
	     {
	     zvalue=Zcor[5];
	     maximum=cormax[5];
	     }

    for(j=0; j < np2; j++) /*Load ZDS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u7[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[6] < ABS(ans[j+1]))
	     {
	     cormax[6] = ABS(ans[j+1]);
	     Zcor[6] = j;
	     }
    if(maximum < cormax[6])
	     {
	     zvalue=Zcor[6];
	     maximum=cormax[6];
	     }

    for(j=0; j < np2; j++) /*Load ZDD*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u8[j];

    correl(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[7] < ABS(ans[j+1]))
	     {
	     cormax[7] = ABS(ans[j+1]);
	     Zcor[7] = j;
	     }
    if(maximum < cormax[7])
	     {
	     zvalue=Zcor[7];
	     maximum=cormax[7];
	     }


free_vector(data1,1,np2);
free_vector(data2,1,np2);
free_vector(ans,1,2*np2);

return(zvalue);

}/*END CORRELATE*/


/******************************* readhelm.c ****************************/

/* Routine to read Helmberger Format Seismograms and to return */
/* information about number of traces, number of time points,  */
/* dt, and the data vector                                     */
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#define FORM1a "%8d\n"
#define FORM1b "%8d      %s\n"
#define FORM2  "%s\n"
#define FORM3  "    %11.4e    %11.4e      0  0  0.00\n"
#define FORM4a "%8d  %8.5f %11.4e\n"
#define TRUE 1
#define FALSE 0

/*char form[32]     = "(6e12.5)";*/
/* cjh  changed to 132 to allow for more info on line */
char form[132]     = "(6e12.5)";
FILE *fopen(),*inf,*of;
int fclose();

readhelm(in,N,NT,DT,vec1)
char *in;
int *N, *NT;
float *DT;
float *vec1;
{
   int nt, orignt, nx,nxx, perline, i, j, cnt, left, f_width, ss;
   int nn, ntemp, test=0;
   float BA, dt, mul=0.0, *pv, *vec, *p, dd, tt, mxtt=0.0;
   float dummy;
   float pi=3.14159265;
   char c_form[512],c_form1[128],c_form2[128];
   char line[228];
   char out[150];
   char extra[80];

   inf=fopen(in,"r");

   /* read initial header */
   fgets(line,100,inf);

   sscanf(line,"%d",&nxx);

   *N=nxx; /*send number of traces*/
   fgets(form,100,inf);
   perline = chkform(form,c_form1,c_form2,&f_width);
   /*fprintf(stderr,"perline=%d  form=%s\n",perline,form);*/

   nx=nxx;
   while (nx) {
   fgets(line,100,inf);
   sscanf(line,"%f %f",&dd,&tt);
   fgets(line,100,inf);
/*   puts(line);  jh*/

   sscanf(line,"%d %f %f %s" ,&nt,&dt,&dummy,&extra);
   *NT++ = nt;
   *DT++ = dt;
/*   fprintf(stderr,"nt=%d dt=%f %f %s\n",nt,dt,dummy,extra); */

   if(nx == nxx)
     {
     orignt = nt;
     vec = (float *) malloc(8*nt);
     p=vec1;
     }
     /* Zero out so variable length and dt's are read
   else
     {
     if (nt != orignt) {
	fprintf (stderr, "nt is not the same throughout\n");
	exit(1);
     }
     }
     */
   cnt  = nt / perline;
   left = nt % perline;
   pv = vec;
   for (i=0;i<cnt;i++)
      {
      for (j=0;j<perline;j++)
	ss=fscanf(inf,c_form2,pv++);

      fgets(line,100,inf);
      }

if ( left != 0)
   {
   for (j=0;j<left;j++)
     fscanf(inf,c_form2,pv++);
     fgets(line,100,inf);
   }

   pv = vec;
   for (j=0;j < nt;j++)
     *p++=*pv++;

   nx--;

   } /*end while */
   fclose(inf);
/*   fprintf(stderr,"Finished Read\n");*/
   }


   /*Subroutines*/
char x_form[64];

int
chkform(f_form,c_form1,c_form2,f_width)
char *f_form, *c_form1, *c_form2;
int *f_width;
{
   int i, cnt, not_done, found_num, num, con1, con2;
   char *px_form, *pf_form, *pn;

   px_form = x_form;
   *f_width = 0;

   for(i=0;i<32;i++) if (f_form[i] == '(') break;
   if (i == 32) {
      fprintf(stderr,"readhelm:  Bad format, no '(', continuing...\n");
      *px_form++ = '(';
      i = 0;
   }
   else {
      *px_form++ = f_form[i];
      i++;
   }
   pf_form = &(f_form[i]);
   cnt = 32 - i;
   not_done = TRUE;
   for (i=0;i<cnt && not_done;i++) {
      
      if (isspace(*pf_form)) {
         pf_form++;
         fprintf(stderr,"readhelm:  Avoid blanks in format\n");
      }

      else if (isdigit(*pf_form)) {
         pn = px_form;
         *px_form++ = *pf_form++;
         while(isdigit(*pf_form)) {
            *px_form++ = *pf_form++;
            i++;
         }
         *px_form = '\0';
         if ((found_num = sscanf(pn,"%d",&num)) != 1) {
            fprintf(stderr,"readhelm:  unknown format error\n");
            exit(-1);
         }
      }

      else switch (*pf_form) {
         case 'e':
         case 'E':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
               fprintf(stderr,"readhelm:  e-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 6)
            {
               fprintf(stderr,"readhelm:  e-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 6)
            {
               fprintf(stderr,"readhelm:  e-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%de\0",con1,con2);
            sprintf(c_form2,"%%%df\0",con1);
            sprintf(px_form,"%d.%d)\0",con1,con2);
            not_done = FALSE;
            break;
         
         case 'f':
         case 'F':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
               fprintf(stderr,"readhelm:  f-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 2)
            {
               fprintf(stderr,"readhelm:  f-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 2)
            {
               fprintf(stderr,"readhelm:  f-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%df\0",con1,con2);
            sprintf(c_form2,"%%%df\0",con1);
            sprintf(px_form,"%d.%d)\0",con1,con2);
	    *f_width = con1;
            not_done = FALSE;
            break;

         case 'g':
         case 'G':
            *px_form++ = *pf_form++;
            if ((sscanf(pf_form,"%d.%d",&con1,&con2)) != 2)
            {
               fprintf(stderr,"readhelm:  g-format error\n");
               exit(-1);
            }
            if (con1 < con2 + 2)
            {
               fprintf(stderr,"readhelm:  g-format error\n");
               exit(-1);
            }
            if (con1 == con2 + 2)
            {
               fprintf(stderr,"readhelm:  g-format may be unreadable, continuing...\n");
            }
            sprintf(c_form1,"%%%d.%dg\0",con1,con2);
            sprintf(c_form2,"%%%df\0",con1);
            sprintf(px_form,"%d.%d)\0",con1,con2);
            not_done = FALSE;
            break;
         
         default:
            if (found_num) {
               fprintf(stderr,"readhelm:  unknown format error\n");
               exit(-1);
            }
            fprintf(stderr,"readhelm:  Bad characters in format continuing...\n");
            pf_form++;
            i++;
            break;
         }
   }
   not_done = FALSE;
   for(i=0;i<32;i++) {
      if (f_form[i] == ')') break;
      if (f_form[i] == '\0') not_done = TRUE;
   }
   if (i == 32 || not_done)
      fprintf(stderr,"readhelm:  Bad format, no ')', continuing...\n");
   for (i=0;i<32;i++) f_form[i] = x_form[i];
   if (found_num) return(num);
   return(1);
}

/*********************** fitcheck2.c ***************************/

/*   comment out jh
#include"tdmt_invb.h"
                          */

fitcheck(ss,gg,W,M,Mo,nsta,degree,var,vred)
 struct MOMENT M;
 struct DATA  *ss;
 struct GREEN *gg;
 int nsta,degree;
 float *W, Mo, *var, *vred;
   {
   int i,j,Zd,Zg,Np,cnt;
   float Dpower, Dtot, Etmp, E, Etot,VAR,DVAR, WSUM, Az, Mscl;

   Mscl = Mo/1.0e+20;
   WSUM=VAR=DVAR=Dtot=Etot=0.0;
   cnt=0;
   for(i=0; i < nsta; i++)
      {
      Dpower=0.0;
      Etmp  =0.0;
      E     =0.0;
      Zd=ss[i].zz;
      Zg=gg[i].zz;
      Np=ss[i].nn;
      Az=ss[i].azi;
      for(j=0; j < Np; j++)
	 {
	 Etmp = ss[i].t[Zd+j] - (M.mxx*0.5*gg[i].u1[j+Zg]*sin(2*Az)
			       - M.myy*0.5*gg[i].u1[j+Zg]*sin(2*Az)
			       - M.mxy*gg[i].u1[j+Zg]*cos(2*Az)
			       - M.mxz*gg[i].u2[j+Zg]*sin(Az)
			       + M.myz*gg[i].u2[j+Zg]*cos(Az));
	 E += Etmp*Etmp;
         Etmp = ss[i].r[Zd+j] - (M.mxx*0.5*gg[i].u5[j+Zg] - M.mxx*0.5*gg[i].u3[j+Zg]*cos(2*Az) + M.mxx*0.3333*gg[i].u9[j+Zg]
			       + M.myy*0.5*gg[i].u5[j+Zg] + M.myy*0.5*gg[i].u3[j+Zg]*cos(2*Az) + M.myy*0.3333*gg[i].u9[j+Zg]
                	       + M.mzz*0.3333*gg[i].u9[j+Zg]
			       - M.mxy*gg[i].u3[j+Zg]*sin(2*Az)
			       + M.mxz*gg[i].u4[j+Zg]*cos(Az)
			       + M.myz*gg[i].u4[j+Zg]*sin(Az));
	 E += Etmp*Etmp;
         Etmp = ss[i].z[Zd+j] - (M.mxx*0.5*gg[i].u8[j+Zg] - M.mxx*0.5*gg[i].u6[j+Zg]*cos(2*Az) +M.mxx*0.3333*gg[i].u10[j+Zg]
			       + M.myy*0.5*gg[i].u8[j+Zg] + M.myy*0.5*gg[i].u6[j+Zg]*cos(2*Az) +M.myy*0.3333*gg[i].u10[j+Zg]
		 	       + M.mzz*0.3333*gg[i].u10[j+Zg]
			       - M.mxy*gg[i].u6[j+Zg]*sin(2*Az)
			       + M.mxz*gg[i].u7[j+Zg]*cos(Az)
			       + M.myz*gg[i].u7[j+Zg]*sin(Az));
	 E += Etmp*Etmp;
	 Dpower += ss[i].t[Zd+j]*ss[i].t[Zd+j];
	 Dpower += ss[i].r[Zd+j]*ss[i].r[Zd+j];
	 Dpower += ss[i].z[Zd+j]*ss[i].z[Zd+j];
	 cnt++;
	 }
	 WSUM += W[3*cnt-1];
	 Etot += E;
	 VAR += W[3*cnt-1]*E;
	 Dtot += Dpower;
	 DVAR += W[3*cnt-1]*Dpower;
	 E /= Dpower;
	 ss[i].vr = (1.0 - E)*100.0;
         fprintf(stderr,"Station(%d): %s VR=%f  %g\n",i,ss[i].name,ss[i].vr,Dpower);
      }
     *var = Etot/(3.0*(float)cnt - (float)degree - 1.0);
      fprintf(stderr,"VAR=%g\n",*var);
      Etot /= Dtot;
      *vred = (1.0-Etot)*100.0;
      fprintf(stderr,"VR=%.2f  (UNWEIGHTED)\n",*vred);
      VAR /= WSUM;
      DVAR /= WSUM;
      VAR /= DVAR;
      VAR = (1.0-VAR)*100.0;
      fprintf(stderr,"VR=%.2f  (WEIGHTED)\n",VAR);
      *vred=VAR;


   }/*fitcheck end*/

/***********     numerical routines *****************************/

#define NRANSI
#include "nrutil.h"

void correl(float data1[], float data2[], unsigned long n, float ans[])
{
	void realft(float data[], unsigned long n, int isign);
	void twofft(float data1[], float data2[], float fft1[], float fft2[],
		unsigned long n);
	unsigned long no2,i;
	float dum,*fft;

	fft=vector(1,n<<1);
	twofft(data1,data2,fft,ans,n);
	no2=n>>1;
	for (i=2;i<=n+2;i+=2) {
		ans[i-1]=(fft[i-1]*(dum=ans[i-1])+fft[i]*ans[i])/no2;
		ans[i]=(fft[i]*dum-fft[i-1]*ans[i])/no2;
	}
	ans[2]=ans[n+1];
	realft(ans,n,-1);
	free_vector(fft,1,n<<1);
}
#undef NRANSI
#include <math.h>
/*#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr */ /* july 13 */

void four1(float data[], unsigned long nn, int isign)
{
	unsigned long n,mmax,m,j,istep,i;
	double wtemp,wr,wpr,wpi,wi,theta;
	float tempr,tempi;

	n=nn << 1;
	j=1;
	for (i=1;i<n;i+=2) {
		if (j > i) {
			SWAP(data[j],data[i]);
			SWAP(data[j+1],data[i+1]);
		}
		m=nn;
		while (m >= 2 && j > m) {
			j -= m;
			m >>= 1;
		}
		j += m;
	}
	mmax=2;
	while (n > mmax) {
		istep=mmax << 1;
		theta=isign*(6.28318530717959/mmax);
		wtemp=sin(0.5*theta);
		wpr = -2.0*wtemp*wtemp;
		wpi=sin(theta);
		wr=1.0;
		wi=0.0;
		for (m=1;m<mmax;m+=2) {
			for (i=m;i<=n;i+=istep) {
				j=i+mmax;
				tempr=wr*data[j]-wi*data[j+1];
				tempi=wr*data[j+1]+wi*data[j];
				data[j]=data[i]-tempr;
				data[j+1]=data[i+1]-tempi;
				data[i] += tempr;
				data[i+1] += tempi;
			}
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
		}
		mmax=istep;
	}
}
#undef SWAP
/* CAUTION: This is the ANSI C (only) version of the Numerical Recipes
   utility file nrutil.c.  Do not confuse this file with the same-named
   file nrutil.c that is supplied in the 'misc' subdirectory.
   *That* file is the one from the book, and contains both ANSI and
   traditional K&R versions, along with #ifdef macros to select the
   correct version.  *This* file contains only ANSI C.               */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#define NR_END 1
#define FREE_ARG char*

void nrerror(char error_text[])
/* Numerical Recipes standard error handler */
{
	fprintf(stderr,"Numerical Recipes run-time error...\n");
	fprintf(stderr,"%s\n",error_text);
	fprintf(stderr,"...now exiting to system...\n");
	exit(1);
}

float *vector(long nl, long nh)
/* allocate a float vector with subscript range v[nl..nh] */
{
	float *v;

	v=(float *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(float)*2));
	if (!v) nrerror("allocation failure in vector()");
	return v-nl+NR_END;
}

int *ivector(long nl, long nh)
/* allocate an int vector with subscript range v[nl..nh] */
{
	int *v;

	v=(int *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(int)*2));
	if (!v) nrerror("allocation failure in ivector()");
	return v-nl+NR_END;
}

unsigned char *cvector(long nl, long nh)
/* allocate an unsigned char vector with subscript range v[nl..nh] */
{
	unsigned char *v;

	v=(unsigned char *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(unsigned char)*2));
	if (!v) nrerror("allocation failure in cvector()");
	return v-nl+NR_END;
}

unsigned long *lvector(long nl, long nh)
/* allocate an unsigned long vector with subscript range v[nl..nh] */
{
	unsigned long *v;

	v=(unsigned long *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(long)*2));
	if (!v) nrerror("allocation failure in lvector()");
	return v-nl+NR_END;
}

double *dvector(long nl, long nh)
/* allocate a double vector with subscript range v[nl..nh] */
{
	double *v;

	v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)*2));
	if (!v) nrerror("allocation failure in dvector()");
	return v-nl+NR_END;
}

float **matrix(long nrl, long nrh, long ncl, long nch)
/* allocate a float matrix with subscript range m[nrl..nrh][ncl..nch] */
{
	long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
	float **m;

	/* allocate pointers to rows */
	m=(float **) malloc((size_t)((nrow+NR_END)*sizeof(float*)*2));
	if (!m) nrerror("allocation failure 1 in matrix()");
	m += NR_END;
	m -= nrl;

	/* allocate rows and set pointers to them */
	m[nrl]=(float *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(float)*2));
	if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
	m[nrl] += NR_END;
	m[nrl] -= ncl;

	for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

	/* return pointer to array of pointers to rows */
	return m;
}

double **dmatrix(long nrl, long nrh, long ncl, long nch)
/* allocate a double matrix with subscript range m[nrl..nrh][ncl..nch] */
{
	long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
	double **m;

	/* allocate pointers to rows */
	m=(double **) malloc((size_t)((nrow+NR_END)*sizeof(double*)*2));
	if (!m) nrerror("allocation failure 1 in matrix()");
	m += NR_END;
	m -= nrl;

	/* allocate rows and set pointers to them */
	m[nrl]=(double *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(double)*2));
	if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
	m[nrl] += NR_END;
	m[nrl] -= ncl;

	for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

	/* return pointer to array of pointers to rows */
	return m;
}

int **imatrix(long nrl, long nrh, long ncl, long nch)
/* allocate a int matrix with subscript range m[nrl..nrh][ncl..nch] */
{
	long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
	int **m;

	/* allocate pointers to rows */
	m=(int **) malloc((size_t)((nrow+NR_END)*sizeof(int*)*2));
	if (!m) nrerror("allocation failure 1 in matrix()");
	m += NR_END;
	m -= nrl;


	/* allocate rows and set pointers to them */
	m[nrl]=(int *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(int)*2));
	if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
	m[nrl] += NR_END;
	m[nrl] -= ncl;

	for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;

	/* return pointer to array of pointers to rows */
	return m;
}

float **submatrix(float **a, long oldrl, long oldrh, long oldcl, long oldch,
	long newrl, long newcl)
/* point a submatrix [newrl..][newcl..] to a[oldrl..oldrh][oldcl..oldch] */
{
	long i,j,nrow=oldrh-oldrl+1,ncol=oldcl-newcl;
	float **m;

	/* allocate array of pointers to rows */
	m=(float **) malloc((size_t) ((nrow+NR_END)*sizeof(float*)*2));
	if (!m) nrerror("allocation failure in submatrix()");
	m += NR_END;
	m -= newrl;

	/* set pointers to rows */
	for(i=oldrl,j=newrl;i<=oldrh;i++,j++) m[j]=a[i]+ncol;

	/* return pointer to array of pointers to rows */
	return m;
}

float **convert_matrix(float *a, long nrl, long nrh, long ncl, long nch)
/* allocate a float matrix m[nrl..nrh][ncl..nch] that points to the matrix
declared in the standard C manner as a[nrow][ncol], where nrow=nrh-nrl+1
and ncol=nch-ncl+1. The routine should be called with the address
&a[0][0] as the first argument. */
{
	long i,j,nrow=nrh-nrl+1,ncol=nch-ncl+1;
	float **m;

	/* allocate pointers to rows */
	m=(float **) malloc((size_t) ((nrow+NR_END)*sizeof(float*)*2));
	if (!m) nrerror("allocation failure in convert_matrix()");
	m += NR_END;
	m -= nrl;

	/* set pointers to rows */
	m[nrl]=a-ncl;
	for(i=1,j=nrl+1;i<nrow;i++,j++) m[j]=m[j-1]+ncol;
	/* return pointer to array of pointers to rows */
	return m;
}

float ***f3tensor(long nrl, long nrh, long ncl, long nch, long ndl, long ndh)
/* allocate a float 3tensor with range t[nrl..nrh][ncl..nch][ndl..ndh] */
{
	long i,j,nrow=nrh-nrl+1,ncol=nch-ncl+1,ndep=ndh-ndl+1;
	float ***t;

	/* allocate pointers to pointers to rows */
	t=(float ***) malloc((size_t)((nrow+NR_END)*sizeof(float**)*2));
	if (!t) nrerror("allocation failure 1 in f3tensor()");
	t += NR_END;
	t -= nrl;

	/* allocate pointers to rows and set pointers to them */
	t[nrl]=(float **) malloc((size_t)((nrow*ncol+NR_END)*sizeof(float*)*2));
	if (!t[nrl]) nrerror("allocation failure 2 in f3tensor()");
	t[nrl] += NR_END;
	t[nrl] -= ncl;

	/* allocate rows and set pointers to them */
	t[nrl][ncl]=(float *) malloc((size_t)((nrow*ncol*ndep+NR_END)*sizeof(float)*2));
	if (!t[nrl][ncl]) nrerror("allocation failure 3 in f3tensor()");
	t[nrl][ncl] += NR_END;
	t[nrl][ncl] -= ndl;

	for(j=ncl+1;j<=nch;j++) t[nrl][j]=t[nrl][j-1]+ndep;
	for(i=nrl+1;i<=nrh;i++) {
		t[i]=t[i-1]+ncol;
		t[i][ncl]=t[i-1][ncl]+ncol*ndep;
		for(j=ncl+1;j<=nch;j++) t[i][j]=t[i][j-1]+ndep;
	}

	/* return pointer to array of pointers to rows */
	return t;
}

void free_vector(float *v, long nl, long nh)
/* free a float vector allocated with vector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_ivector(int *v, long nl, long nh)
/* free an int vector allocated with ivector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_cvector(unsigned char *v, long nl, long nh)
/* free an unsigned char vector allocated with cvector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_lvector(unsigned long *v, long nl, long nh)
/* free an unsigned long vector allocated with lvector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_dvector(double *v, long nl, long nh)
/* free a double vector allocated with dvector() */
{
	free((FREE_ARG) (v+nl-NR_END));
}

void free_matrix(float **m, long nrl, long nrh, long ncl, long nch)
/* free a float matrix allocated by matrix() */
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch)
/* free a double matrix allocated by dmatrix() */
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

void free_imatrix(int **m, long nrl, long nrh, long ncl, long nch)
/* free an int matrix allocated by imatrix() */
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

void free_submatrix(float **b, long nrl, long nrh, long ncl, long nch)
/* free a submatrix allocated by submatrix() */
{
	free((FREE_ARG) (b+nrl-NR_END));
}

void free_convert_matrix(float **b, long nrl, long nrh, long ncl, long nch)
/* free a matrix allocated by convert_matrix() */
{
	free((FREE_ARG) (b+nrl-NR_END));
}

void free_f3tensor(float ***t, long nrl, long nrh, long ncl, long nch,
	long ndl, long ndh)
/* free a float f3tensor allocated by f3tensor() */
{
	free((FREE_ARG) (t[nrl][ncl]+ndl-NR_END));
	free((FREE_ARG) (t[nrl]+ncl-NR_END));
	free((FREE_ARG) (t+nrl-NR_END));
}
#include <math.h>

void realft(float data[], unsigned long n, int isign)
{
	void four1(float data[], unsigned long nn, int isign);
	unsigned long i,i1,i2,i3,i4,np3;
	float c1=0.5,c2,h1r,h1i,h2r,h2i;
	double wr,wi,wpr,wpi,wtemp,theta;

	theta=3.141592653589793/(double) (n>>1);
	if (isign == 1) {
		c2 = -0.5;
		four1(data,n>>1,1);
	} else {
		c2=0.5;
		theta = -theta;
	}
	wtemp=sin(0.5*theta);
	wpr = -2.0*wtemp*wtemp;
	wpi=sin(theta);
	wr=1.0+wpr;
	wi=wpi;
	np3=n+3;
	for (i=2;i<=(n>>2);i++) {
		i4=1+(i3=np3-(i2=1+(i1=i+i-1)));
		h1r=c1*(data[i1]+data[i3]);
		h1i=c1*(data[i2]-data[i4]);
		h2r = -c2*(data[i2]+data[i4]);
		h2i=c2*(data[i1]-data[i3]);
		data[i1]=h1r+wr*h2r-wi*h2i;
		data[i2]=h1i+wr*h2i+wi*h2r;
		data[i3]=h1r-wr*h2r+wi*h2i;
		data[i4] = -h1i+wr*h2i+wi*h2r;
		wr=(wtemp=wr)*wpr-wi*wpi+wr;
		wi=wi*wpr+wtemp*wpi+wi;
	}
	if (isign == 1) {
		data[1] = (h1r=data[1])+data[2];
		data[2] = h1r-data[2];
	} else {
		data[1]=c1*((h1r=data[1])+data[2]);
		data[2]=c1*(h1r-data[2]);
		four1(data,n>>1,-1);
	}
}
void twofft(float data1[], float data2[], float fft1[], float fft2[],
	unsigned long n)
{
	void four1(float data[], unsigned long nn, int isign);
	unsigned long nn3,nn2,jj,j;
	float rep,rem,aip,aim;

	nn3=1+(nn2=2+n+n);
	for (j=1,jj=2;j<=n;j++,jj+=2) {
		fft1[jj-1]=data1[j];
		fft1[jj]=data2[j];
	}
	four1(fft1,n,1);
	fft2[1]=fft1[2];
	fft1[2]=fft2[2]=0.0;
	for (j=3;j<=n+1;j+=2) {
		rep=0.5*(fft1[j]+fft1[nn2-j]);
		rem=0.5*(fft1[j]-fft1[nn2-j]);
		aip=0.5*(fft1[j+1]+fft1[nn3-j]);
		aim=0.5*(fft1[j+1]-fft1[nn3-j]);
		fft1[j]=rep;
		fft1[j+1]=aim;
		fft1[nn2-j]=rep;
		fft1[nn3-j] = -aim;
		fft2[j]=aip;
		fft2[j+1] = -rem;
		fft2[nn2-j]=aip;
		fft2[nn3-j]=rem;
	}
}
/************************************************************************/

/*
#include"tdmt_invb.h" */
#include<string.h>
/* routine changed to just save data in a file jh */
float A[6];
void yplot();

mt_plot(ss,gg,nsta,Strike,Rake,Dip,St2,Rk2,Dp2,M,d_mt,Pdc,Pclvd,Piso,Mo,Mw,E,VR)
int nsta;
struct MOMENT M;
struct GREEN *gg;
struct DATA  *ss;
float Strike, Rake, Dip, Pdc, Pclvd, Piso, Mo, Mw, E, VR;
float St2, Rk2, Dp2;
double d_mt[3][3];
   {
   int i,j, n, Z, Zg, Np, Zd, count;
   float xorig=1.0, yorig=7.0,xscale,yscale;
   float dt, spin, x, y, *tmp, height, Mscl,Az;
   float scalefac,scaler,scalet,scalez;
   char name[6];

/* open file for synthetics jh */

synt=fopen("synt.out","w");

   Mscl = Mo/1.0e+20;


/* loop of stations */

   for(i=0; i < nsta; i++)
      {
      Np=ss[i].nn;
      Z =ss[i].zz;
      dt=ss[i].dt;
      spin = (float)Np*ss[i].dt/2.0;
      tmp=(float *)malloc(sizeof(float)*Np*2);
/*      printf("zpar %d nsta %d\n",Z,i); */

      fprintf(synt,"%d %d\n",i,Np); /* write channel number and number of samples jh */


      for(j=0; j < Np; j++)
         {
	 tmp[j]=ss[i].t[j+Z];             /* shift according to zcor */
         if(j>(Np-Z-2)) { 
           tmp[j]=0.0;    /* make sure no old data is copied jh */
/*           printf("extra end %d \n",j) */ ;}
         if((j+Z)<0)                     /* also blank in fron if zcor negative */
         { 
         tmp[j]=0.0;
/*         printf("%f\n",j,tmp[j]);*/ }

         fprintf(synt,"%f\n",tmp[j]);   /*write seismogrem jh*/
         }

      for(j=0; j < Np; j++)
         {
	 tmp[j]=ss[i].r[j+Z];
         if(j>(Np-Z-2)) { tmp[j]=0.0; }   /* make sure no old data is copied */
         if((j+Z)<0){ tmp[j]=0.0; }
         fprintf(synt,"%f\n",tmp[j]);   /*write seismogrem jh*/
         }

      for(j=0; j < Np; j++)
         {
	 tmp[j]=ss[i].z[j+Z];
         if(j>(Np-Z-2)) { tmp[j]=0.0; }   /* make sure no old data is copied */
         if((j+Z)<0){ tmp[j]=0.0; }
         fprintf(synt,"%f\n",tmp[j]);   /*write seismogrem jh*/
         }

      Az=ss[i].azi;
      Np=gg[i].nn;
      Zg =gg[i].zz;
      dt=gg[i].dt;
      spin = (float)Np*gg[i].dt/2.0;  /* not used  ?*/

      for(j=0; j< Np; j++)
	 {
	 tmp[j]=(M.mxx*0.5*gg[i].u1[j+Zg]*sin(2*Az)
		- M.myy*0.5*gg[i].u1[j+Zg]*sin(2*Az)
	        - M.mxy*gg[i].u1[j+Zg]*cos(2*Az)
	        - M.mxz*gg[i].u2[j+Zg]*sin(Az)
	        + M.myz*gg[i].u2[j+Zg]*cos(Az));
/* write out tangential*/
                fprintf(synt,"%f\n",tmp[j]);   /*write synt seismogram jh*/
	 }

      for(j=0; j< Np; j++)
	 {
	 tmp[j]= (M.mxx*0.5*gg[i].u5[j+Zg] 
		 - M.mxx*0.5*gg[i].u3[j+Zg]*cos(2*Az) 
		 + M.mxx*0.3333*gg[i].u9[j+Zg]
	         + M.myy*0.5*gg[i].u5[j+Zg] 
		 + M.myy*0.5*gg[i].u3[j+Zg]*cos(2*Az) 
		 + M.myy*0.3333*gg[i].u9[j+Zg]
 	         + M.mzz*0.3333*gg[i].u9[j+Zg]
		 - M.mxy*gg[i].u3[j+Zg]*sin(2*Az)
		 + M.mxz*gg[i].u4[j+Zg]*cos(Az)
		 + M.myz*gg[i].u4[j+Zg]*sin(Az));
/*Radial Synth*/
                fprintf(synt,"%f\n",tmp[j]);   /*write synt seismogram jh*/
	 }

      for(j=0; j< Np; j++)
	 {
	 tmp[j]= (M.mxx*0.5*gg[i].u8[j+Zg] 
		 - M.mxx*0.5*gg[i].u6[j+Zg]*cos(2*Az) 
		 + M.mxx*0.3333*gg[i].u10[j+Zg]
                 + M.myy*0.5*gg[i].u8[j+Zg] 
		 + M.myy*0.5*gg[i].u6[j+Zg]*cos(2*Az) 
		 + M.myy*0.3333*gg[i].u10[j+Zg]
                 + M.mzz*0.3333*gg[i].u10[j+Zg]
		 - M.mxy*gg[i].u6[j+Zg]*sin(2*Az)
		 + M.mxz*gg[i].u7[j+Zg]*cos(Az)
		 + M.myz*gg[i].u7[j+Zg]*sin(Az));
/*Vertical Synth*/
                fprintf(synt,"%f\n",tmp[j]);   /*write synt seismogram jh*/
	 }
      free(tmp);
      }
   }



