
/* SEISAN version of iscloc header file.						
		*/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

/* String lengths for reading instructions and configuration. */
#define LINLEN 80
#define PARLEN 20
#define VALLEN 20

/* String lengths for structure members */
#define FILENAMELEN 50
#define AGLEN 17
#define NETLEN 7
#define STALEN 7
#define PHALEN 9

/* All times in seconds relative to this date. */
#define EPOCH_YEAR 2000

#define MAXHYP 50
#define MAXPHA 5000
#define MAXAMP 10
#define PHA_PER_READ 50
#define NULLVAL 9999999

#define ELLIPT  0.99327733			/* (1 - 1/297)*(1 - 1/297) */
#define TORAD   0.01745329			/*  PI/180 */
#define PI		3.14159265
#define EARTH_RADIUS 6371
#define KM_PER_DEG 111.195

/* Parameters used starting point solution. */
#define SEARCH_START_DEPTH 33
#define SEARCH_MAX_DELTA 110
#define SEARCH_PDIFF_DELTA 102

#define ERRLEN 160

char errstr[ERRLEN];
FILE* logfp, *errfp;

struct hyp_rec {
	int hypid;
	double time;			/* Time in seconds since epoch */
	double lat;
	double lon;
	double dircos[7];		/* 6 direction cosines - don't use 0 index 
*/
	double depth;
	int nsta;
	int ndefsta;
	int nass;
	int ndef;
	double mindist;
	double maxdist;
	double azimgap;
	char etype[5];
	char agency[AGLEN];
	double sdobs;
	double stime;
	double sdepth;
	double minax;
	double majax;
	double theta;
	int depfix;
	int epifix;
	int timfix;
	int rank;			
};

struct amp_rec{
	double amp;
	double per;
	double logat;
	char comp;
};

struct pha_rec {
	int hypid;
	int phid;
	int rdid;
	int pref_rd;
	char init;
	char rep_phase[PHALEN];
	char phase[PHALEN];				/* ISC code - could change 
each iter */
	char net[NETLEN];
	char sta[STALEN];
	double sta_lat;
	double sta_lon;
	double sta_elev;	/* elevation in m. */
	double dircos[7]; 	/* 6 direction cosines - don't use 0 index */
	double time;		/* Time in seconds since epoch */
	float slow;
	float azim;			/* measured azimuth. */
	float esaz;			/* azimuth cf current solution. */
	float delta;		/* delta cf current solution. */
	int numamps;
	struct amp_rec a[MAXAMP];
	char comp;
	char sp_fm;
	char detchar;
	double weight;
	double weight_factor;
	int purged;			/* 1 for purged phase. 2 if whole reading 
purged. */
	int duplicate;		/* flag set to 1 for duplicate phase. */
	double ttime;
	double dtdd;		/* in s/deg. */
	double dtdh;		/* in s/km.  */
	double resid;
	double pP_weight;
	double pP_resid;
	double pP_dtdh;
	double pP_P_time;
	double s_min_p_time;
	double bodymag;
	double surfmag;
};

struct sol_rec {
	int iteration;
	int converged;				/* 0/1 flag for convergence 
reached. */
	int diverging;				/* 0/1 flag for worsening 
solution. */
	int number_of_unknowns;
	char weighting_type[20];
	int numphas;
	char phases_purged;
	int hypid;
	double time;				/* Time in seconds since epoch */
	double lat;
	double lon;
	double dircos[7]; 			/* 6 direction cosines - don't use 
0 index */
	double depth;				/* In km from surface. */
	double depdp;
	double depdp_error;
	double alpha;
	double prev_alpha;
	double sigma;
	double prev_sigma;
	double covar[5][5];			/* Dont use 0 indexes */
	double error[5];			/* 4 errors - don't use 0 index */
	double majax;
	double minax;
	double theta;
	double sdobs;
	double mindist;
	double maxdist;
	double azimgap;
	int ndef;
	int nass;
	int nsta;
	int ndefsta;
	double bodymag;
	double surfmag;
	int nsta_mb;
	int nsta_ms;
	int mb_id;
	int ms_id;
};

int guess_loc (struct sol_rec *sp, struct pha_rec p[]);
int init_sol (struct sol_rec *sp, struct hyp_rec *hp);
int id_pha (struct sol_rec *sp, struct pha_rec p[], int reid_phase);
int calc_resid (struct sol_rec *sp, struct pha_rec p[], char mode[]);
int read_ttime (struct sol_rec *sp, struct pha_rec *pp);
int correct_ttime (struct sol_rec *sp, struct pha_rec *pp);
double calc_geoid_corr(double lat, struct pha_rec *pp);
double calc_elev_corr(struct pha_rec *pp);
int calc_weight (struct sol_rec *sp, struct pha_rec p[]);
int huber_weight(struct sol_rec *sp, struct pha_rec p[]);
int buland_weight(struct sol_rec *sp, struct pha_rec p[]);
int no_weight(struct sol_rec *sp, struct pha_rec p[]);
int solve (struct sol_rec *sp, struct pha_rec p[]);
int calc_error (struct sol_rec *sp, struct pha_rec p[]);
int decide_iter(struct sol_rec *sp, struct pha_rec p[]);
int get_weight_factor(struct pha_rec p[], int numphas);
int purge_pha (struct sol_rec *sp,  struct pha_rec p[]);
int read_config(char*);


/* From utils.c */
void print_sol(struct sol_rec *sp);
void print_pha(int numphas, struct pha_rec p[]);
void calc_dircos (double lat, double lon, double dircos[]);
double calc_delta (double sta_dircos[], double h_dircos[]);
double calc_esaz (double sta_dircos[], double h_dircos[], double sta_lon,
				 double h_lon, double delta);
char * write_time(double time, char timestr[24]);
double read_time(char timestr[24]);
int split_time(double time, int *yyyy, int *mm, int *dd,
				int *hh, int *mi, int *ss, int *msec);
double join_time(int yyyy, int mm, int dd, int hh, int mi, int ss, int msec);
void dsort (double[], int left, int right);
void add_to_error(char *part_error);
void handle_error(void);


