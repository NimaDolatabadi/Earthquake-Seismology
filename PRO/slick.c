#include <math.h>
#include <stdio.h>
#define TORADS 57.29577951
#define MAXDATA 1000
#define MAX3 3000
/* COORDINATES ARE EAST,NORTH,UP */
/* this version does no statistics */
/* and therefore makes no plot */
/* 2014-04-14 pv: TORADS was defines two times, removed second TORADS 
                  NOTE second TORADS was wrong : TORADS 57.2577951 
                  renamed abs function to absd, it was conflicting to 
                  build in function */

main(argc,argv)  /* slickenside inversion program */
int argc;  /* argument count */
char **argv; /* argument string */
{
	double ddir[MAXDATA];  /* dip direction for data */
	double dip[MAXDATA];   /* dip of data */
	double rake[MAXDATA];  /* rake of data */
	short nobs;  /* number of observations */
	double amat[MAX3][5];  /* coefficient matrix for normal equation */
	double stress[6];  /* stress tensor in vector form, element order is: */
	/* xx,xy,xz,yy,yz,zz */
	double strten[3][3];  /* stress tensor in tensor form */
	double slick[MAX3];    /* slickenside vector elements vector */
	double n1,n2,n3;    /* normal vector elements */
	double norm[MAXDATA][3];  /* storage of n1,n2,n3 */
	short i,j,k;        /* dummy variables */
	double z,z2,z3;     /* more dummy variables */
	char name[20];      /* output file name */
	FILE *fpin;   /* input file pointer */
	FILE *fpout;  /* output file pointer */
	FILE *fplot;  /* plot file pointer */
	double a2[5][5],cc[5],sigma;  /* for use with leasq subr */
	double a2i[5][5];  /* to get covariance mtrix */
	double lam[3];  /* eigenvalues */
	double vecs[3][3];  /* eigenvectors */
	char line[80];  /* character line */
	double t[3];  /* shear stress vector */
	double iso;  /* isotropic stress mag */
	double angavg,angstd;  /* average and standard deviation of fit angle */
	double isoavg,isostd;  /* same for isotropic stress size */
	double magavg,magstd;  /* same for tangential stress size */
	double tf[3],tnorm;  /* full traction vector  */
	/* and normal traction */

	/* get file pointers */
	-- argc;  
	++argv;
	if(argc == 0){
		printf("usage: slick data_file\n");
		return;
	}
	fpin=fopen(*argv,"r");
	if(fpin==NULL){
		printf("unable to open %s.\n",*argv);
		return;
	}
	sprintf(name,"%s.oput",*argv);
	fpout=fopen(name,"w");
	if(fpout==NULL){
		printf("unable to open %s.\n",name);
		return;
	}

	/* read and write comment line from data file to output file */
	fgets(line,80,fpin);
	fputs(line,fpout);

	/* loop to get data and make up equation */
	nobs=0;
	while(fscanf(fpin,"%lf%lf%lf",&ddir[nobs],&dip[nobs],&rake[nobs])
	    != EOF )
	{
		i=nobs;
		j=3*nobs;
		++nobs;
		z=ddir[i]/TORADS;
		z2=dip[i]/TORADS;
		z3=rake[i]/TORADS;

		n1=sin(z)*sin(z2);  /* normal vector to fault plane */
		n2=cos(z)*sin(z2);
		n3=cos(z2);

		norm[i][0]=n1;
		norm[i][1]=n2;
		norm[i][2]=n3;

		/* slickenside vector calculation */
		slick[j]= -cos(z3)*cos(z)-sin(z3)*sin(z)*cos(z2);
		slick[j+1]= cos(z3)*sin(z)-sin(z3)*cos(z)*cos(z2);
		slick[j+2]= sin(z3)*sin(z2);

		/* find the matrix elements */
		amat[j][0]= n1-n1*n1*n1+n1*n3*n3;
		amat[j][1]= n2-2.*n1*n1*n2;
		amat[j][2]= n3-2.*n1*n1*n3;
		amat[j][3]= -n1*n2*n2+n1*n3*n3;
		amat[j][4]= -2.*n1*n2*n3;

		amat[j+1][0]= -n2*n1*n1+n2*n3*n3;
		amat[j+1][1]= n1-2.*n1*n2*n2;
		amat[j+1][2]= -2.*n1*n2*n3;
		amat[j+1][3]= n2-n2*n2*n2+n2*n3*n3;
		amat[j+1][4]= n3-2.*n2*n2*n3;

		amat[j+2][0]= -n3*n1*n1-n3+n3*n3*n3;
		amat[j+2][1]= -2.*n1*n2*n3;
		amat[j+2][2]= n1-2.*n1*n3*n3;
		amat[j+2][3]= -n3*n2*n2-n3+n3*n3*n3;
		amat[j+2][4]= n2-2.*n2*n3*n3;

		/* check to see if all possible data has been read */
		if(nobs==MAXDATA){
			fprintf(fpout,"NOT ALL DATA COULD BE READ.\n");
			break;
		}
	}  /* end of data read loop */

	/* solve equations via linear least squares */
	i=5;
	j= 3*nobs;
	leasq(amat,i,j,stress,slick,a2,cc,&sigma);
	/* fix zz element by using trace = 0 */
	stress[5]= -(stress[0]+stress[3]);

	/* put stress tensor into tensor form */
	strten[0][0]= stress[0];
	strten[0][1]= stress[1];
	strten[1][0]= stress[1];
	strten[0][2]= stress[2];
	strten[2][0]= stress[2];
	strten[1][1]= stress[3];
	strten[1][2]= stress[4];
	strten[2][1]= stress[4];
	strten[2][2]= stress[5];

	fprintf(fpout,"\nCOORDINATES ARE EAST,NORTH,UP.\n");
	fprintf(fpout,"stress tensor is:\n");
	for(i=0;i<3;++i){
		for(j=0;j<3;++j)fprintf(fpout,"%g  ",strten[i][j]);
		fprintf(fpout,"\n");
	}

	/* find  eigenvalues and eigenvectors */
	eigen(strten,lam,vecs);
	fprintf(fpout,"eigenvalue   vector: E,N,UP,direction,plunge\n");
	for(i=0;i<3;++i){
		dirplg(vecs[0][i],vecs[1][i],vecs[2][i],&z,&z2);
		fprintf(fpout,"%g  ",lam[i]);
		for(j=0;j<3;++j)fprintf(fpout,"%g  ",vecs[j][i]);
		fprintf(fpout,"%f  %f\n",z,z2);
	}
	fprintf(fpout,"variance= %g\n",sigma);
	/* order eigenvalues and compute phi */
	i=1;
	while(i){
		i=0;
		for(j=0;j<2;++j){
			if(lam[j]>lam[j+1]){
				z=lam[j];
				lam[j]=lam[j+1];
				lam[j+1]=z;
				i=1;
			}
		}
	}
	if(lam[0] != lam[2]){
		z=(lam[1]-lam[2])/(lam[0]-lam[2]);
		fprintf(fpout,"phi value= %g\n",z);
	}
	/* output data and fit angle */

	angavg=0.;
	angstd=0.;
	isoavg=0.;
	isostd=0.;
	iso=0.;
	fprintf(fpout,"\ndip direction, dip, rake, fit angle, mag tau\n");
	for(i=0;i<nobs;++i){

		for(j=0;j<3;++j){  /* compute shear traction */
			t[j]=0;
			tf[j]=0;
			for(k=0;k<5;++k)t[j]+= amat[3*i+j][k]*stress[k];
			for(k=0;k<3;++k)tf[j]+= strten[j][k]*norm[i][k];
		}
		tnorm=0;
		for(k=0;k<3;++k)tnorm+= tf[k]*norm[i][k];
		/* find angle between t and slickenside */
		z=0.;
		for(j=0;j<3;++j)z+= t[j]*slick[3*i+j];
		z2=0.;
		for(j=0;j<3;++j)z2+= t[j]*t[j];
		z2=sqrt(z2);
		z3=0.;
		for(j=0;j<3;++j)z3+= slick[3*i+j]*slick[3*i+j];
		z3=sqrt(z3);
		z=z/(z2*z3);
		z=acos(z)*TORADS;
		angavg+= z;
		angstd+= z*z;
		z3= (z2/(-0.8)) - tnorm;
		iso+= fabs(tnorm);
		isoavg+=z3;
		isostd+= z3*z3;
		magavg+=z2;
		magstd+= z2*z2;
		fprintf(fpout,"%7.1f  %7.1f  %7.1f  %7.1f %7.2f\n",
		ddir[i],dip[i],rake[i],z,z2);
	}
	z3=(double)nobs-1;
	angstd= angstd-(angavg*angavg/nobs);
	angstd= angstd/z3;
	angstd= sqrt(angstd);
	angavg= angavg/nobs;
	isostd= isostd-(isoavg*isoavg/nobs);
	isostd= isostd/z3;
	isostd= sqrt(isostd);
	isoavg= isoavg/nobs;
	iso/= nobs;
	isoavg/= iso;
	isostd/= iso;
	magstd= magstd-(magavg*magavg/nobs);
	magstd= magstd/z3;
	magstd= sqrt(magstd);
	magavg= magavg/nobs;
	fprintf(fpout,"fit angle mean= %f standard deviation= %f\n",angavg,angstd);
	fprintf(fpout,"for f=0.8 I= %f , std. dev.= %f D norm= %f\n",
	isoavg,isostd,iso);
	fprintf(fpout,"avg tau= %f , std. dev.= %f\n",magavg,magstd);
}




leasq(a,m,n,x,b,a2,c,psis) /* finds the least squares solution of ax=b */

double a[]; /* the coefficients matrix with n rows and m columns */
double x[]; /* the solution vector of length m */
double b[]; /* the constant vector of length n */
double a2[]; /* a square matrix of size m for internal use */
double c[]; /* vector of length m for internal use */
short m,n; /* see above */
double *psis; /*pointer to the variance */

/* steps 1 a2= a transpose a */
/*       2 c= a transpose b */
/*       3 solve a2x=c by gaussian elimination */

{
	short i,j;
	atransa(a,m,n,a2);
	atransb(a,m,n,b,c);
	gaus(a2,m,x,c);
	sigsq(a,m,n,x,b,psis);
	return;
}


gaus(a,m,x,b) /* solves ax=b for x by gaussian elimination */


#define sub(I,J) (J+I*m)
short m;
double a[];  /* a square matrix of size m */
double b[];  /* a vector of length m */
double x[];  /* a vector of length m */
{
	short i,i2,i3;
	double d,hold,fact;
	double absd();

	/* take care of special cases */
	if(m<2){
		x[1]=0.;
		if(m==1) x[1]=b[1]/a[1];
		return ;
	}

	for(i=0;i<m;++i){     /* loop for each pivot */
		for(i2=i+1;i2<m;++i2){   /* loop for each row below a pivot */

			/* see if element below pivot is 0 */
			if(a[sub(i2,i)]==0.) continue;

			/* if element below pivot > pivot flop rows  */
			if(absd(a[sub(i2,i)])>absd(a[sub(i,i)])){
				hold=b[i];
				b[i]=b[i2];
				b[i2]=hold;
				for(i3=i;i3<m;++i3){
					hold=a[sub(i,i3)];
					a[sub(i,i3)]=a[sub(i2,i3)];
					a[sub(i2,i3)]=hold;
				}
			}

			/* do the elimination */ 
			fact=a[sub(i2,i)]/a[sub(i,i)];
			a[sub(i2,i)]=0.;
			for(i3=i+1;i3<m;++i3)a[sub(i2,i3)]=a[sub(i2,i3)]-fact*a[sub(i,i3)];
			b[i2]=b[i2]-fact*b[i];


		}
	}

	/* solve the equations */
	x[m-1]=b[m-1]/a[m*m-1];
	for(i=m-2;i> -1;--i){
		d=b[i];
		for(i2=i+1;i2<m;++i2)d=d-x[i2]*a[sub(i,i2)];
		x[i]=d/a[sub(i,i)];
	}
	return;
}


double absd(cc)
double cc;
{  
	double dd;
	dd=cc;  
	if(dd<0)dd= -dd; 
	return (dd); 
}



atransa(a,m,n,b) /* computes b=a transpose*a  */


#define sub(I,J) (J+I*m)
double a[];   /* a matrix of m columns and n rows */
double b[];    /* a square matrix of size m */
short m,n;

{
	short i,j,k;
	double bb;

	for(i=0;i<m;++i){
		for(j=i;j<m;++j){
			bb=0;
			for(k=0;k<n;++k) bb=bb+a[sub(k,i)]*a[sub(k,j)];
			b[sub(i,j)]=bb;
			b[sub(j,i)]=bb;
		}
	}
	return;
}



atransb(a,m,n,b,c) /* computes c= atranspose * b */
double a[];  /* a matrix of n rows and m columns  */
double b[];  /* vector of length n */
double c[];  /* vector of length m */
short m,n;   /* see above */

{   
	short i,i2;

	for(i=0;i<m;++i){ 
		c[i]=0.;
		for(i2=0;i2<n;++i2)c[i]=c[i]+a[sub(i2,i)]*b[i2];
	}
	return;
}

sigsq(a,m,n,x,b,psis) /* computes the variance of a */
/* single observation */
double a[];    /* matrix of n rows and m columns */
double b[];    /* data vector length n*/
double x[];    /* solution vector length m */
double *psis;  /* where to put answer */
short m,n;     /* see above */
{ 
	short i,j;    /* loop variables */
	double y,z,z2;     /* sum variables */

	z=0;
	for(i=0;i<n;++i){  /* loop over rows */
		y=0;
		for(j=0;j<m;++j)y+= a[sub(i,j)]*x[j];
		z+= (b[i]-y)*(b[i]-y);
	}

	if(n!=m){
		z2=(double)n-m;
		z=z/z2;
	}
	*psis=z;
}



#include <math.h>
eigen(a,lam,q)    /* subroutine to find eigen values and */
/* and eigenvectors of a 3 by 3 matrix */
/* at end eigenvalues are in lam and   */
/* the corresponding eigenvector is in */
/* q[i,*]                              */
/* does not destroy the matrix a       */


double a[3][3];  /* the matrix */
double lam[3];  /* the eigen values */
double q[3][3]; /* used to hold eigen vectors at end */
{
	double a2[3][3];  /* the matrix again */
	double r[3][3];  /* the decomposition matrices */
	/* q also used to hold eigenvectors at end */
	short i,j,k;   /* various indices */
	double x,y,z;   /* various holders */
	double shift;   /* shift value for shifted QR */
	double xx[3],b[3];

	/* set a2 for later use */
	for(i=0;i<3;++i){
		for(j=0;j<3;++j){
			a2[i][j]=a[i][j];
			r[i][j]=0.;
		}
	}

	/* do the QR */
	/* do the shift */
omt:  
	shift=a2[2][2];
	shift=shift*.999;
	for(i=0;i<3;++i)a2[i][i]=a2[i][i]-shift;
	for(j=0;j<3;++j){
		/* find v sub j */
		for(i=0;i<3;++i)q[i][j]=a2[i][j];
		r[j][j]=1.;
		if(j!=0){
			/* must subtract from q */
			for(k=0;k<j;++k){
				/* find coefficient */
				x=0;
				y=0;
				for(i=0;i<3;++i){
					x=x+q[i][k]*a2[i][j];
					y=y+q[i][k]*q[i][k];
				}
				z=x/y;
				r[k][j]=z;
				for(i=0;i<3;++i)q[i][j]=q[i][j]-z*q[i][k];
			}
		}
	}
	/* now normalize Q and R */
	for(j=0;j<3;++j){
		z=0;
		for(i=0;i<3;++i)z=z+q[i][j]*q[i][j];
		z=sqrt(z);
		for(i=0;i<3;++i){
			q[i][j]=q[i][j]/z;
			r[j][i]=r[j][i]*z;
		}
	}

	/* form a= RQ */
	for(i=0;i<3;++i){
		for(j=0;j<3;++j){
			x=0;
			for(k=0;k<3;++k)x=x+r[i][k]*q[k][j];
			a2[i][j]=x;
		}
	}
	for(i=0;i<3;++i)a2[i][i]=a2[i][i]+shift;
	/* check to see if new iteration needed */
	if(fabs(a2[2][1]) > fabs(.001*a2[2][2]))goto omt;
	if(fabs(a2[1][0]) > fabs(.001*a2[1][1]))goto omt;
	/* store eigen values */
	for(i=0;i<3;++i)lam[i]=a2[i][i];
	/* find eigen vectors now */
	for(k=0;k<3;++k){
		for(i=0;i<3;++i){
			for(j=0;j<3;++j){
				a2[i][j]=a[i][j];
				if(i==j)a2[i][j]=a2[i][j]-lam[k];
			}
			b[i]=0;
		}
		eigvec(a2,3,xx,b);
		for(i=0;i<3;++i)q[i][k]=xx[i];
	}

}
#include <math.h>
eigvec(a,m,x,b) /* solves ax=b for x by gaussian elimination */
/* set up expecially for eigenvectors (i.e.  */
/* for singular matrices)                    */

#define sub(I,J) (J+I*m)
short m;
double a[];  /* a square matrix of size m */
double b[];  /* a vector of length m */
double x[];  /* a vector of length m */
{
	short i,i2,i3;
	double d,hold,fact;
	double mag;

	/* take care of special cases */
	if(m<2){
		x[1]=0.;
		if(m==1) x[1]=b[1]/a[1];
		return ;
	}

	for(i=0;i<m;++i){     /* loop for each pivot */
		for(i2=i+1;i2<m;++i2){   /* loop for each row below a pivot */

			/* see if element below pivot is 0 */
			if(a[sub(i2,i)]==0.) continue;

			/* if element below pivot > pivot flop rows  */
			if(fabs(a[sub(i2,i)])>fabs(a[sub(i,i)])){
				hold=b[i];
				b[i]=b[i2];
				b[i2]=hold;
				for(i3=i;i3<m;++i3){
					hold=a[sub(i,i3)];
					a[sub(i,i3)]=a[sub(i2,i3)];
					a[sub(i2,i3)]=hold;
				}
			}

			/* do the elimination */ 
			fact=a[sub(i2,i)]/a[sub(i,i)];
			a[sub(i2,i)]=0.;
			for(i3=i+1;i3<m;++i3)a[sub(i2,i3)]=a[sub(i2,i3)]-fact*a[sub(i,i3)];
			b[i2]=b[i2]-fact*b[i];


		}
	}

	/* set small values to zero */
	mag=0.;
	for(i=0;i<m;++i){
		for(i2=0;i2<m;++i2)mag=mag+a[sub(i,i2)]*a[sub(i,i2)];
	}
	mag=sqrt(mag)/sqrt(2.);
	for(i=0;i<m;++i){
		for(i2=0;i2<m;++i2) 
			if(mag*.001 > fabs(a[sub(i,i2)]) ) a[sub(i,i2)]=0.;
	}

	/* solve the equations */
	if( a[m*m-1]==0.)x[m-1]=1.;
	else x[m-1]=b[m-1]/a[m*m-1];
	for(i=m-2;i> -1;--i){
		d=b[i];
		for(i2=i+1;i2<m;++i2)d=d-x[i2]*a[sub(i,i2)];
		if( a[sub(i,i)]==0.)x[i]=1.;
		else x[i]=d/a[sub(i,i)];
	}

	/* normalize the eigenvector */
	mag=0.;
	for(i=0;i<m;++i)mag=mag+x[i]*x[i];
	mag=sqrt(mag);
	if(mag == 0.)return;
	for(i=0;i<m;++i)x[i]=x[i]/mag;

	return ;
}





#include <math.h>
// #define TORADS 57.2577951
dirplg(e,n,u,pdir,pplg) /* to find direction and plunge of a vector */
double e,n,u; /* the vector in east,north,up coordinates */
double *pdir,*pplg;  /* pointers to the direction in east of north */
/* and the plunge down the direction */
{
	double z;  /* dummy variable */
	z=e*e+n*n;
	z=sqrt(z);
	*pplg=atan2(-u,z)*TORADS;
	if(*pplg<0){
		*pplg= -*pplg;
		e= -e;
		n= -n;
	}
	*pdir=atan2(e,n)*TORADS;
}
