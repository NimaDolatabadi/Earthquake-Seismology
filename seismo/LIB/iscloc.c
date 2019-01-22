/* 2014-04-29 pv added changes due to solaris cc warning */

#include "iscloc.h"
#include "iscloc_jb_model.h"
#include "iscloc_jb_tables.h"

#define CONFIG_FILE "DAT/iscloc.def" 	/* Appended to $SEISAN_TOP. */

/*#DOC  Title:																*/
/*#DOC    iscloc															*/
/*#DOC  Desc:																*/
/*#DOC    Calculates hypocentre locations using ISC methods.				*/
/*#DOC  Input Arguments:													*/
/*#DOC    hyp_* - single element arrays with values from top line of s-file.*/
/*#DOC    rea_* - arrays with values from phase lines of s-file.			*/
/*#DOC    char_stat  - 2D array of chars used to pass station names.		*/
/*#DOC    char_phase - 2D array of chars used to pass phase names.			*/
/*#DOC    char_isc_phase - 2D array of chars used to pass back isc codes.	*/
/*#DOC    pha_* - arrays with station details stored parallel to rea_*.		*/
/*#DOC    numpha - pointer to number of phases in rea and pha arrays.		*/
/*#DOC  Calls:																*/
/*#DOC	  Most of the iscloc_* functions from LIB.							*/

/*  This is a function designed to be called from SEISAN Fortran.  			*/
/*  It contains a subset of the functionality in the iscloc program and		*/
/*  calculates a hypocentral solution using the same algorithms as used at	*/
/*  the International Seismological Centre.									*/

/*  The methods and functions used are fully documented at:					*/
/*			 http://www.isc.ac.uk/Documents/Location/Documentation/			*/

/* Error messages from functions are passed here using the errstr variable	*/
/* declared in iscloc.h.  These messages are displayed using the function	*/
/* handle_error() from iscloc_utils.c.										*/

/******************************************************************************
*                                                                             *
*                           Function Declarations                             *
*                                                                             *
******************************************************************************/

int calc_delaz (struct sol_rec *sp, struct pha_rec p[]);
int change_weighting (struct sol_rec *sp);

/*****************************************************************************/


void iscloc_(int *hyp_year, int *hyp_month, int *hyp_day,
             int *hyp_hour, int *hyp_min, float *hyp_sec,
             char *hyp_fix_org, float *hyp_lat, float *hyp_lon,
             char *hyp_epi_flag, float *hyp_depth, char *hyp_depth_flag,
			 float *hyp_rms, float *hyp_gap, float *hyp_sec_err,
			 float *hyp_lat_err, float *hyp_lon_err, float *hyp_depth_err, 
             char char_stat[][5], char char_phase[][8],
             char char_isc_phase[][8], int *rea_hour,
			 int *rea_min, float *rea_sec, float *rea_res,
			 float *rea_dist, float *rea_az,
             float *pha_weight, float *pha_lat, float *pha_lon,
			 float *pha_elev, int *numpha)
{
	extern int max_iter;						/* From config file */
	extern int reid_phase;						/* From config file */
	extern char logfile[];						/* From config file */
	extern char errfile[];						/* From config file */

	/* Structures defined in iscloc.h */
	struct hyp_rec h;
	struct pha_rec *p;
	struct sol_rec s;

	char last_sta[STALEN];
	char namestr[80];
	char timestr[24];

	int sec,msec;
	int rdid;
	int option;
	int i,j;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Read configuration file and assign values to	*/
	/* corresponding external variables.			*/
	sprintf(namestr,"%s/%s",getenv("SEISAN_TOP"),CONFIG_FILE);
	if ( read_config(namestr) ){
		fprintf(stderr,"ISCLOC ERROR: %s\n",errstr);
		return;
	}

	/* Open file for error messages - could want stdout/stderr. */
	if (errfile[0]){
		if (strcmp(errfile,"stdout")==0)
			errfp = stdout;
		else if (strcmp(errfile,"stderr")==0)
			errfp = stderr;
		else if ((errfp = fopen(errfile,"w")) == NULL){
			fprintf(stderr,"ISCLOC ERROR: Can't open error file\n");
			return;
		}
	}
	else{
		fprintf(stderr,"ISCLOC ERROR: No error file given\n");
		return;
	}

	/* Open file for log of actions - could want stdout/stderr. */
	if (logfile[0]){
		if (strcmp(logfile,"stdout")==0)
			logfp = stdout;
		else if (strcmp(logfile,"stderr")==0)
			logfp = stderr;
		else if ((logfp = fopen(logfile,"a")) == NULL){
			sprintf(errstr,"Can't open log file.");
			handle_error();
			return;
		}
	}
	else{
		sprintf(errstr,"No log file given.");
		handle_error();
		return;
	}

	/* Announce arrival. */
	fprintf(logfp,"==============================================\n");
	fprintf(logfp,"iscloc: Starting location\n\n");

	/* Allocate memory for array of numpha phase structures. */
	p = (struct pha_rec *) calloc(*numpha, sizeof(struct pha_rec));
	if (p == NULL){
		sprintf(errstr,"Can't allocate memory for phase array.");
		handle_error();
		return;
	}

	/* Fill hypocentre structure - should have at least year/month/day.*/
	if (hyp_year[0] < 0 || hyp_year[0] > 2500){
		sprintf(errstr,"Bad hypocentre.");
		handle_error();
		free(p);
		return;
	}
	else{
		h.lat = hyp_lat[0];
		h.lon = hyp_lon[0];
		h.depth = hyp_depth[0];
		calc_dircos(hyp_lat[0],hyp_lon[0],h.dircos);

		/* Times stored as seconds since epoch. */
		sec = (int)hyp_sec[0];
		msec = 1000*(hyp_sec[0]-sec)+0.5;
		h.time = join_time(hyp_year[0],hyp_month[0],hyp_day[0],hyp_hour[0],
							hyp_min[0],sec,msec);
	}

	/* Impossible to fix time without fixing lat/lon. */
	if (hyp_fix_org[0] == 'F' && hyp_epi_flag[0] != 'F'){
		fprintf(logfp,"iscloc: Fixing lat/lon to go with fixed time\n");
		hyp_epi_flag[0] = 'F';
	}

	/* Impossible to fix lat/lon without fixing depth. */
	if (hyp_epi_flag[0] == 'F' && hyp_depth_flag[0] != 'F'){
		fprintf(logfp,"iscloc: Fixing depth to go with fixed lat/lon\n");
		hyp_depth_flag[0] = 'F';
	}

	/* Fill in array of phase structures. */
	rdid=0;
	strcpy(last_sta,"");
	s.nsta = 0;
	s.numphas = *numpha;
	for (i=0; i<s.numphas; i++){
		p[i].phid = i;

		/* Phase codes come as 2D array of characters in fortran order. */	
		j=0;
		while (j<8 && char_phase[i][j] != ' ')
			p[i].rep_phase[j] = char_phase[i][j++];
		p[i].rep_phase[j] = '\0';

		/* Station names come as 2D array of characters in fortran order. */	
		j=0;
		while (j<5 && char_stat[i][j] != ' ')
			p[i].sta[j] = char_stat[i][j++];
		p[i].sta[j] = '\0';

		/* Assume phases sorted so that readings come together. */
		if (strcmp(p[i].sta,last_sta)){
			p[i].init = 1;
			p[i].rdid = ++rdid;
			strcpy(last_sta,p[i].sta);
			s.nsta++;
		}
		else{
			p[i].init = 0;
			p[i].rdid = rdid;
		}

		/* Times stored as seconds since epoch. 				*/
		/* Year/month/day not on phase line so use hyp date.	*/
		/* Increment by one day if phase time before origin.	*/
		sec = (int)rea_sec[i];
		msec = 1000*(rea_sec[i]-sec)+0.5;
		p[i].time = join_time(hyp_year[0],hyp_month[0],hyp_day[0],rea_hour[i],
			 					rea_min[i],sec,msec);

		if (p[i].time < h.time - 3600)
			p[i].time += 86400;

		p[i].sta_lat = pha_lat[i];
		p[i].sta_lon = pha_lon[i];
		p[i].sta_elev = pha_elev[i];

		if (p[i].init)
			calc_dircos(pha_lat[i],pha_lon[i],p[i].dircos);
		else
			for (j=1; j<=6; j++)
				p[i].dircos[j] = p[i-1].dircos[j];

		p[i].s_min_p_time=NULLVAL;
		p[i].resid=NULLVAL;
	}

	/* If there is no hypocentre then try and find one. */
	if (h.lat < 0){
		if (diagnostic){
			fprintf(logfp,"iscloc: calling guess_loc %d phases:\n",s.numphas);
			print_pha(s.numphas,p);
		}

		if (guess_loc(&s, p)){
			fprintf(logfp,"iscloc: No initial solution found.\n");
			free(p);
			return;
		}
		h.time   = s.time;
		h.lat    = s.lat;
		h.lon    = s.lon;
		h.depth  = s.depth;
		for (i=1; i<=6; i++)
			h.dircos[i] = s.dircos[i];

		fprintf(logfp,"iscloc: found starting point: ");
		fprintf(logfp,"time=%s ",write_time(h.time,timestr));
		fprintf(logfp,"lat=%f lon=%f depth=%f\n",h.lat,h.lon,h.depth);
	}

	/* Location starts here. */
	/* ====================  */

	/* Option loop */
	/* option 0 is free depth  */
	/* option 1 is fixed depth */
	/* option 2 is fixed depth and location. */
	for (option=0;option<3;option++){

		/* If time is fixed then set solution as required and leave. */
		if (hyp_fix_org[0] == 'F'){
			fprintf (logfp,"iscloc: Calculating residuals only\n");
			if (diagnostic)
				fprintf (logfp,"Calling init_sol to fix all\n");
			init_sol(&s, &h);
			s.converged = 1;
			option=3;
			break;
		}

		/* No free depth pass if fixed depth instruction. */
		if (hyp_depth_flag[0] == 'F')
			option++;

		/* If location is fixed then depth will always be fixed. */
		if (hyp_epi_flag[0] == 'F')
			option++;

		if (option==0)
			s.number_of_unknowns = 4;
		else if (option==1)
			s.number_of_unknowns = 3;
		else if (option==1)
			s.number_of_unknowns = 1;

		/* Set up the solution structure using the current seed. */
		if (diagnostic)
			fprintf (logfp,"iscloc: Calling init_sol\n");
		init_sol(&s, &h);

		/* Iteration loop */
		for (s.iteration=0; s.iteration<max_iter; s.iteration++){

			fprintf(logfp,"iscloc: opt%d iter %d: ",option,s.iteration);
			print_sol(&s);

			/* Set delta and seaz for each phase. */
			if (diagnostic)
				fprintf (logfp,"iscloc: Calling calc_delaz\n");
			calc_delaz(&s,p);

			/* Set ISC phase id for each phase. */
			if (diagnostic)
				fprintf (logfp,"iscloc: Calling id_pha\n");
			id_pha(&s,p,reid_phase);

			/* Set weight_factor for each phase. */
			if (diagnostic)
				fprintf (logfp,"iscloc: Calling get_weight_factor\n");
			get_weight_factor(p,s.numphas);

			/* Set ttime, resid, dtdh, and dtdd for each phase. */
			if (diagnostic)
				fprintf (logfp,"iscloc: Calling calc_resid\n");
			if (calc_resid(&s,p,"use")){
				handle_error();
				break;
			}

			/* Set weight for each phase. */
			if (diagnostic)
				fprintf (logfp,"iscloc: Calling calc_weight\n");
			if (calc_weight(&s, p)){
				handle_error();
				break;
			}

			/* Solve equations and update solution structure. */
			if (diagnostic)
				fprintf(logfp,"iscloc: Calling solve\n");
			if (solve(&s, p)){
				handle_error();
				break;
			}

			/* Force at least 2 iterations. */
			if (s.iteration == 0)
				continue;	

			/* Check for convergence/divergence. */
			/* Update solution if needed. */
			if (diagnostic)
				fprintf(logfp,"iscloc: Calling decide_iter\n");
			if (decide_iter(&s, p)){
				handle_error();
				break;
			}

			if (s.diverging){
				fprintf(logfp,"iscloc: DIVERGING\n");
				break;
			}

			/* After first convergence remove phases with large		*/
			/* residuals and change weighting type.					*/
			/* Either of these facilities can be switched off.		*/
			if (s.converged){

				fprintf(logfp,"iscloc: CONVERGED\n");

				/* Change weighting if hasn't already been done	*/
				/* and if a second weighting_type is given.		*/
				/* Sets converged to 0 if it changes weighting. */
				if (diagnostic)
					fprintf(logfp,"iscloc: Calling change_weighting\n");
				if (change_weighting(&s)){
					handle_error();
					s.diverging = 1;
					break;
				}

				/* Remove phases with large resids from		*/
				/* solution. Only does this once.			*/
				/* Sets converged to 0 if it purges phases. */
				if (diagnostic)
					fprintf(logfp,"iscloc: Calling purge_pha\n");
				if (purge_pha(&s, p)){
					handle_error();
					s.diverging = 1;
					break;
				}

				/* Iterate again if something has been done.		*/
				/* Set iteration to -1 to make new solution start	*/
				/* at 0 (iteration++ in for) This ensures that		*/
				/* weights get re-initialised in calc_weight.		*/
				if (!s.converged){
					s.iteration=-1;
				}
			}

			/* If solution found then stop iterating. */
			if (s.converged)
				break;
		}										 /* Iteration loop */

		fprintf(logfp,"\niscloc: last  : ");
		print_sol(&s);
		fprintf(logfp,"\n");

		/* Don't need another option if converged already. */
		if (s.converged)
			break;
	}										/* Option loop */

	/* Could either be here because of success or failure. */
	if (s.converged){

		/* Calculate residuals for final solution. */

		/* Set delta and seaz for each phase. */
		if (diagnostic)
			fprintf(logfp,"iscloc: Calling calc_delaz\n");
		calc_delaz(&s,p);

		/* If no solution has been done then no ISC phase codes. */
		/* Call id_pha with reid_phase=0.						 */
		if (option==3)
			id_pha(&s,p,0);

		/* Calculate residuals. */
		if (diagnostic)
			fprintf(logfp,"iscloc: Calling calc_resid\n");
		if (calc_resid(&s,p,"all")){
			handle_error();
			free(p);
			return;
		}

		/* Print phases and residuals to log file. */
		print_pha(s.numphas,p);

		/* Overwrite phase arrays from calling routine. */
		for (i=0; i<s.numphas; i++){

			/* Put phase codes in 2D array of characters in fortran order. */	
			j=0;
			while (p[i].phase[j] != '\0')
				char_isc_phase[i][j] = p[i].phase[j++];
			char_isc_phase[i][j] = '\0';

			/* rea.for uses -999 to signify no value. */
			if (p[i].resid == NULLVAL)
				rea_res[i] = -999.0;
			else
				rea_res[i]    = p[i].resid;

			pha_weight[i] = p[i].weight;
			rea_dist[i]   = p[i].delta*KM_PER_DEG;
			rea_az[i]     = p[i].esaz;
		}

		/* No errors etc if just calculated residuals.			*/
		if (option < 3){

			/* Calculate errors etc for final solution. */
			if (diagnostic)
				fprintf(logfp,"iscloc: Calling calc_error\n");
			if (calc_error(&s,p)){
				handle_error();
				free(p);
				return;
			}

			/* Overwrite hypocentre sent from calling routine.	*/
			hyp_lat[0]   = s.lat;
			hyp_lon[0]   = s.lon;
			hyp_depth[0] = s.depth;
			hyp_rms[0]       = s.sdobs;
			hyp_gap[0]       = s.azimgap;

			/* Give lat/lon errors without scaling factors.			*/
			/* Possible that will be used to calculate ellipsoid.	*/
			/* rea.for uses -999 to signify no value. 				*/
			hyp_sec_err[0]   = s.error[1];
			if (option < 2){
				hyp_lat_err[0]   = sqrt(s.covar[2][2])*KM_PER_DEG;
				hyp_lon_err[0]   = sqrt(s.covar[3][3])*KM_PER_DEG;
			}
			else{
				hyp_lat_err[0]   = -999;
				hyp_lon_err[0]   = -999;
			}
			if (option < 1)
				hyp_depth_err[0] = sqrt(s.covar[4][4]);
			else
				hyp_depth_err[0] = -999.0;

			if (option>0)
				hyp_depth_flag[0] = 'F';
			else
				hyp_depth_flag[0] = ' ';

			if (option>1)
				hyp_epi_flag[0] = 'F';
			else
				hyp_epi_flag[0] = ' ';

			/* Print final solution to log file. */
			fprintf(logfp,"\niscloc: final : ");
			print_sol(&s);
			fprintf(logfp,"\nAzimthal gap = %.0f\n\n",s.azimgap);
		}
		/* If just calculating residuals set rms to null. */
		else{
			hyp_rms[0] = -999;
		}
	}							/* if s.converged */
	else{
		fprintf(logfp,"iscloc: FAILURE\n");
		*numpha = 0;
	}

	free(p);
	fclose(logfp);
	fclose(errfp);
	return;
}

/*#DOC  Title:																*/
/*#DOC    buland_weight														*/
/*#DOC  Desc:																*/
/*#DOC    Fills 'alpha' in solution structure and 'weight' for each phase.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    p[]  - array structures containing phases.						*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Weighting as implemented in reviser.								*/
/* Not in fact full Jeffreys weighting as sigval remains constant throughout. */

/* Designed to be run after Huber-t weighting has converged - see	*/
/* comment in huber_weighting.c.									*/

int buland_weight (struct sol_rec *sp, struct pha_rec p[])
{
	
	extern int min_phases;				/* From config file */
	extern double init_max_resid;		/* From config file */
	extern double max_resid;			/* From config file */
	extern double mu;					/* From config file */
	extern double sigma_start;			/* From config file */

	int i,ndef;
	double total_weight;
	double sigma_12;
	double sigval;
	double spread;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	sigma_12 = 12*sigma_start;
	sigval   = 1/(2*sigma_start*sigma_start);	/* Robust sigma from R.Buland.*/

	/* First iteration initialise alpha as average residual.		*/
	/* Then calculate initial weights.								*/
	/* Afterwards, calculate weights again as usual.				*/
	if (sp->iteration == 0){
		sp->alpha=0;
		ndef=0;

		for (i=0; i<sp->numphas; i++){
			if (p[i].weight_factor > 0 && fabs(p[i].resid) < init_max_resid){
				sp->alpha += p[i].resid;
				ndef++;
			}
			if (diagnostic){
				fprintf(logfp,"buland_weight: init:%s %s ",p[i].sta,p[i].phase);
				fprintf(logfp,"%f %f\n",p[i].weight_factor,p[i].resid);
			}
		}
		if (ndef < min_phases ){
			fprintf(logfp,"TOO FEW PHASES buland_weight init: ndef=%d\n",ndef);
			strcpy(errstr,"");
			return 1;
		}
		sp->alpha /= ndef;
		sp->prev_alpha = sp->alpha;

		if (diagnostic)
			fprintf(logfp,"buland_weight: init alpha=%f n=%d\n",sp->alpha,ndef);

		for (i=0; i<sp->numphas; i++){
			if (fabs(p[i].resid) < max_resid){
				spread = fabs(p[i].resid - sp->alpha);
				if (spread <= sigma_12)
					p[i].weight = 1/(1+mu*exp(spread*spread*sigval));
				else
					p[i].weight = 0;

				p[i].weight *= p[i].weight_factor;
			}
			else
				p[i].weight = 0;
		}
	}

	/* First calculate alpha. */
	sp->alpha=0;
	total_weight=0;
	ndef=0;
	for (i=0; i<sp->numphas; i++){
		if (p[i].weight_factor > 0 && fabs(p[i].resid) < max_resid){
			sp->alpha += p[i].resid*p[i].weight;
			total_weight += p[i].weight;
			ndef++;
		}
		if (diagnostic){
			fprintf(logfp,"buland_weight: %s %s ",p[i].sta,p[i].phase);
			fprintf(logfp,"%f %f ",p[i].weight_factor,p[i].weight);
			fprintf(logfp,"%f\n",p[i].resid);
		}
	}
	if (ndef < min_phases ){
		fprintf(logfp,"TOO FEW PHASES buland_weight: ndef=%d\n",ndef);
		strcpy(errstr,"");
		return 1;
	}
	sp->alpha /= total_weight;

	if (diagnostic){
		fprintf(logfp,"buland_weight: alpha=%f ",sp->alpha);
		fprintf(logfp,"total_weight=%f\n",total_weight);
	}

	/* Then calculate weights. */
	for (i=0; i<sp->numphas; i++){
		if (fabs(p[i].resid) < max_resid){
			spread = fabs(p[i].resid - sp->alpha);
			if (spread <= sigma_12)
				p[i].weight = 1/(1+mu*exp(spread*spread*sigval));
			else
				p[i].weight = 0;

			p[i].weight *= p[i].weight_factor;
		}
		else
			p[i].weight = 0;
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    calc_delaz														*/
/*#DOC  Desc:																*/
/*#DOC    Fills 'delta' and 'esaz' fields for each phase.					*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to structure containing current solution.			*/
/*#DOC    p[] - array of pointers to structures containing phases.			*/
/*#DOC  Return:	0															*/
/*#DOC  Calls:																*/
/*#DOC	  calc_delta() and calc_esaz() from utils.c.						*/
/*#DOC    print_pha() from utils.c											*/

int calc_delaz (struct sol_rec *sp, struct pha_rec p[])
{
	double delta,esaz;
	int i;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	for (i=0; i<sp->numphas; i++){

		/* Assume that readings come together with initial phase first. */
		/* Then only have to calculate delta and azimuth once per reading. */
		if (p[i].init){
			delta=calc_delta(p[i].dircos,sp->dircos);
			esaz =calc_esaz(p[i].dircos,sp->dircos,p[i].sta_lon,sp->lon,delta);
		}
		p[i].delta = delta;
		p[i].esaz  = esaz;
	}
	if (diagnostic){
		printf ("calc_delaz done:\n");
		print_pha(sp->numphas, p);
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    calc_elev_corr													*/
/*#DOC  Desc:																*/
/*#DOC    Calculates travel time correction for elevation of station.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    pp - pointer to phase structure.									*/
/*#DOC  Return:																*/
/*#DOC    Travel time correction for station elevation.						*/

/* If both an elevation correction and a crustal correction are required	*/
/* then the elevation correction is done in calc_crust_corr, not here.		*/

double calc_elev_corr(struct pha_rec *pp)
{
	double elev_corr;

	/* Not all stations have a known elevation. */
	if (pp->sta_elev == NULLVAL)
		return 0;

	/* Do P and S arrivals but not Pg or Sg. */
	if (strcmp(pp->phase,"P") ==0  || 
		strcmp(pp->phase,"PKP")==0 ||
		strcmp(pp->phase,"Pn")==0  ||
		strcmp(pp->phase,"Pb")==0){

		elev_corr  = PGVEL*(pp->dtdd/KM_PER_DEG);
		elev_corr *= elev_corr;
		elev_corr  = 1 / sqrt( 1 - elev_corr);
		elev_corr *= pp->sta_elev/(1000*PGVEL);
	}
	else if (strcmp(pp->phase,"S")==0  || 
			strcmp(pp->phase,"SKS")==0 ||
			strcmp(pp->phase,"Sn")==0  ||
			strcmp(pp->phase,"Sb")==0){

		elev_corr  = SGVEL*(pp->dtdd/KM_PER_DEG);
		elev_corr *= elev_corr;
		elev_corr  = 1 / sqrt( 1 - elev_corr);
		elev_corr *= pp->sta_elev/(1000*SGVEL);
	}
	else
		elev_corr = 0;

	return elev_corr;
}


/*#DOC  Title:																*/
/*#DOC    calc_error														*/
/*#DOC  Desc:																*/
/*#DOC    Calculates 'nass', 'ndef', 'nsta', 'ndefsta', 'sdobs', 'mindist',	*/
/*#DOC    'maxdist', 'azimgap', 'error[]','majax', 'minax', and 'theta'		*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to structure containing solution.					*/
/*#DOC    p[] - array of pointers to structures containing phases.			*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Uses:																*/
/*#DOC    get_fact() and get_fact2() from this file.						*/


/* Counts phases/stations to get ndef etc. for final solution:			*/
/* nsta is all stations except those that have had phases purged.		*/
/* (Stations with purged phases will be removed from this event.)		*/
/* nass is all phases for all nsta stations.							*/
/* ndef is number of phases with a weight_factor > 0.					*/
/* ndefsta is count of stations with at least one defining phase.		*/

/* Calculates weighted standard deviation of residuals using the		*/
/*  current value of sigma. (Only actualy sd if assume mean resid=0).	*/

/* Interprets the covarience matrix to get standard errors and an		*/
/* error ellipse. Assumes that dtdd is in sec/degree.					*/

int calc_error (struct sol_rec *sp, struct pha_rec p[])
{

	double get_fact (int num, int nunk);	/* Local subroutine */
	double get_fact2(int num, int nunk);	/* Local subroutine */

	double *esaz;
	double var_lat,var_lon,lat_lon;
	double scale_fact,km_per_deg;
	double b,c,eigen1,eigen2;
	char prev_net[NETLEN],prev_sta[STALEN];
	char prev_def_net[NETLEN],prev_def_sta[STALEN];
	int pha_for_sta, purge_for_sta, weight_for_sta;
	int i;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Assign memory for array of azimuths. */
	if ((esaz = (double*) calloc (sp->numphas, sizeof(double))) == NULL){
		sprintf(errstr,"calc_error: Memory problem.");
		return 1;
	}

	/* Count associated/defining phases/stations. 					*/
	/* Rely on phases being ordered by station in array.	 		*/
	/* Exclude a station from nsta if one or more phases purged		*/
	/* and no other phases for it have a weight.					*/
	/* nass includes all or none of the phases for a given station.	*/
	/* At same time get mindist and maxdist and fill an array of	*/
	/* azimuths for the calculation of azimgap.						*/
	sp->ndef = sp->nass = sp->nsta = sp->ndefsta = 0;
	pha_for_sta = purge_for_sta = weight_for_sta = 0;
	sp->mindist = 360;
	sp->maxdist = 0;
	for (i=0;i<sp->numphas;i++){

		/* New station. */
		if (strcmp(p[i].sta,prev_sta) || strcmp(p[i].net,prev_net)){

			/* Add numbers for previous station. */
			if (pha_for_sta > 0 && (purge_for_sta==0 || weight_for_sta > 0)){
				sp->nsta++;
				sp->nass+=pha_for_sta;
			}
			pha_for_sta = purge_for_sta = weight_for_sta = 0;
		}
		pha_for_sta++;

		/* Defining phases/stations. */
		if (p[i].weight_factor > 0 && p[i].resid != NULLVAL){

			/* New defining station. */
			if (strcmp(p[i].sta,prev_def_sta) || strcmp(p[i].net,prev_def_net)){
				if (p[i].delta < sp->mindist)
					sp->mindist = p[i].delta;
				if (p[i].delta > sp->maxdist)
					sp->maxdist = p[i].delta;

				esaz[sp->ndefsta++] = p[i].esaz;
				strcpy(prev_def_sta,p[i].sta);
				strcpy(prev_def_net,p[i].net);
			}
			sp->ndef++;
			weight_for_sta++;
		}

		if (p[i].purged)
			purge_for_sta++;

		strcpy(prev_sta,p[i].sta);
		strcpy(prev_net,p[i].net);
	}
	/* Add numbers for last station. */
	if (pha_for_sta > 0 && (purge_for_sta==0 || weight_for_sta > 0)){
		sp->nsta++;
		sp->nass+=pha_for_sta;
	}

	/* Weighted standard deviation - can use final value of sigma. */
	if (sp->ndef){
		sp->sdobs =  sp->sigma;
		sp->sdobs *= sqrt(sp->ndef/(sp->ndef - sp->number_of_unknowns));
	}
	else {
		sp->sdobs   = NULLVAL;
		sp->mindist = NULLVAL;
		sp->maxdist = NULLVAL;
	}

	/* Calculate azimuthal gap using local array of azimuths filled above. */
	if (sp->ndef){
		dsort(esaz,0,sp->ndefsta-1);
		sp->azimgap = esaz[0] + 360 - esaz[sp->ndefsta-1];

		for(i=0; i < sp->ndefsta-1; i++){
			if( (esaz[i+1]-esaz[i]) > sp->azimgap)
				sp->azimgap = esaz[i+1]-esaz[i];
		}
	}
	else {
		sp->azimgap = NULLVAL;
		sp->ndef = NULLVAL;
		sp->ndefsta = NULLVAL;
	}
	free(esaz);

	if (diagnostic){
		fprintf(logfp,"calc_error: nsta=%d nass=%d ",sp->nsta,sp->nass);
		fprintf(logfp,"ndefsta=%d ndef=%d ",sp->ndefsta,sp->ndef);
		fprintf(logfp,"gap=%f \n",sp->azimgap);
	}

	/* Calculate standard errors. */

	/* Calculate scaling factor for errors using function below. */
	if (diagnostic)
		fprintf(logfp,"calc_error: calling get_fact\n");
	scale_fact = get_fact(sp->numphas,sp->number_of_unknowns);

	/* Enter errors in solution structure. */
	for (i=1; i<=sp->number_of_unknowns; i++)
		sp->error[i] = scale_fact*sqrt(sp->covar[i][i]);
	for (i=sp->number_of_unknowns+1; i<5; i++)
		sp->error[i] = NULLVAL;

	/* No error ellipse for fixed location solution */
	if (sp->number_of_unknowns < 3){
		sp->majax    = NULLVAL;
		sp->minax    = NULLVAL;
		sp->theta    = NULLVAL;
 		return 0;
	}

	/* Express longitude error in degrees for appropriate latitude */
	sp->error[3] /= cos(sp->lat*TORAD);

	/* Calculate error ellipse. */

	/* Get partial derivatives from matrix and convert into km. */
	/* Terms for longitude are already in degrees of equatorial equivalent. */
	km_per_deg = TORAD * (EARTH_RADIUS - sp->depth);
	var_lat = km_per_deg * km_per_deg * sp->covar[2][2];
	lat_lon = km_per_deg * km_per_deg * sp->covar[2][3];
	var_lon = km_per_deg * km_per_deg * sp->covar[3][3];

	/* Solve quadratic to get the eigenvalues of the matrix:		*/
	/*		 x = (-b +/- sqrt(b^2-4ac)) / 2a  where a=1				*/
	b = var_lat+var_lon;
	c = var_lat*var_lon - lat_lon*lat_lon;

	eigen1 = 0.5 * (b + sqrt(b*b - 4*c));
	eigen2 = 0.5 * (b - sqrt(b*b - 4*c));

	/* Again need scaling factor, but a differnet one this time. */
	if (diagnostic)
		fprintf(logfp,"calc_error: calling get_fact2\n");
	scale_fact = get_fact2(sp->numphas,sp->number_of_unknowns);

	/* Calculate ellipse parameters. */
	sp->majax = sqrt(eigen1)*scale_fact;
	sp->minax = sqrt(eigen2)*scale_fact;
	sp->theta = (atan(2*lat_lon/(var_lat-var_lon)))/TORAD;

	if (sp->theta < 0)
		sp->theta += 180;

	/* Print out avergage ellipse axis for cf reviser. */
	if (diagnostic)
		printf ("calc_error: position %f\n", sqrt(sp->majax*sp->minax));

	return 0;
}

/* Subroutine to get error scaling factor.  Only used by calc_error. */
/* Comments below come from reviser function fctrn.f:			*/
/*		N is num, M is nunk										*/
/*  
c   Function fctrn returns a semi-empirical scaling factor which when
c   multiplied into the square roots of the diagonal elements of the
c   correlation matrix, yield the best current estimates of the 90%
c   marginal confidence intervals.  N is the number of observations
c   and M is the number of parameters being found (either 3 for a held
c   depth or 4 for a free depth solution).  The factor is
c   asymptotically sigma times beta times the .05 percentage point of
c   the normal distribution.  Sigma is the empirically derived robust
c   estimate of the standard deviation of teleseismic travel-time
c   residuals (about 1.145 s).  Beta is the inverse square root of the
c   efficiency of the regression method times a correction for the
c   correlation matrix which is approximately the inverse square root
c   of the maximum weight (about 1.03).  This asymptotic value is
c   multiplied by an empirical non-asymptotic correction which turns
c   out to be nearly linear in the inverse square root of the number
c   of degrees of freedom.  As expected, the empirical factor is
c   independent of the number of parameters sought.  However, it is
c   unexpectedly slightly smaller than unity for large values of N.
c   For zero degrees of freedom, apparently due to non-linearity in
c   the free depth solution, the asymptotic correction depends on M.
c   Programmed on 17 March 1987 by R. Buland.
c
c   Why the empirical relationship should be slightly different for
c   held and free depth solutions is not exactly clear.  The
c   difference is not very large and may not be significant.
c   However, as it is consistent between 1 and 60 degrees of freedom
c   and as it is the best available information at this time, it has
c   been retained.
*/
double get_fact (int num, int nunk)
{
	double factor;

	if (num > nunk){             /* Positive degrees of freedom. */
		if (nunk > 3){
			factor = 1.94*( 0.926 + 0.333/sqrt(num-nunk) );
		}
		else{
			factor = 1.94*( 0.905 + 0.306/sqrt(num-nunk) );
		}
	}
	else{						/* Zero degrees of freedom. */
		if (nunk > 3){
			factor=5.72;
		}
		else{
			factor=2.64;
		}
	}
	return factor;
}

/* Subroutine to get error scaling factor.  Only used by calc_error.*/
/* Comments below come from reviser function fctrx2.f:				*/
/*		N is num, M is nunk											*/
/*  
c   Function fctrx2 returns a semi-empirical scaling factor which when
c   multiplied into the correlation matrix, yields the best current
c   estimate of the 90% confidence ellipsoid.  N is the number of
c   observations and M is the number of parameters being found (either
c   3 for a held depth or 4 for a free depth solution).  The factor is
c   asymptotically sigma times beta times the square root of chi
c   squared with M-1 degrees of freedom (for the spatial error
c   ellipsoid).  Sigma is the empirically derived robust estimate of the
c   standard deviation of teleseismic travel-time residuals (about
c   1.145 s).  Beta is the inverse square root of the efficiency of the
c   regression method times a correction for the correlation matrix
c   which is approximately the inverse square root of the maximum weight
c   (about 1.03).  This asymptotic value is multiplied by an empirical
c   non-asymptotic correction which turns out to be nearly linear in
c   the inverse square root of the number of degrees of freedom.  As
c   expected, the empirical factor is independent of the number of
c   parameters sought.  However, it is unexpectedly slightly smaller
c   than one for large values of N.  For zero degrees of freedom,
c   apparently due to non-linearity in the free depth solution, the
c   asymptotic correction depends on M.  Programmed on 17 March 1987
c   by R. Buland.

c   Why the empirical relationship should be slightly different for
c   held and free depth solutions is not exactly clear.  The
c   difference is not very large and may not be significant.
c   However, as it is consistent between 1 and 60 degrees of freedom
c   and as it is the best available information at this time, it has
c   been retained.
*/

double get_fact2 (int num, int nunk)
{
	double factor;

	if (num > nunk){             /* Positive degrees of freedom. */
		if (nunk > 3){
			factor = 2.94*( 0.926 + 0.333/sqrt(num-nunk) );
		}
		else{
			factor = 2.52*( 0.905 + 0.306/sqrt(num-nunk) );
		}
	}
	else{						/* Zero degrees of freedom. */
		if (nunk > 3){
			factor=8.67;
		}
		else{
			factor=3.43;
		}
	}
	return factor;
}


/* Steps in delta used to get multiplier for geoid correction. */
int delta_step[] = { 9, 13, 22, 41, 63, 72, 83, 124, 140, 180 };

/*#DOC  Title:																*/
/*#DOC    calc_geoid_corr													*/
/*#DOC  Desc:																*/
/*#DOC    Calculates travel time correction for height above geoid.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    lat - source latitude.											*/
/*#DOC    pp  - pointer to  a phase structure.								*/
/*#DOC  Return:																*/
/*#DOC    Travel time correction for height of geoid.						*/
/*#DOC  Calls:																*/
/*#DOC    height_above_mean_sphere() from this file.						*/

double calc_geoid_corr(double lat, struct pha_rec *pp)
{
	double height_above_mean_sphere(double lat);
	double epi_hams,sta_hams;
	double elcor,geoid_corr;
	int i;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Apply corrections at station and epicentre together.	*/
	/* delta_step array defined at top of file.				*/
	epi_hams = height_above_mean_sphere(lat*TORAD);
	sta_hams = height_above_mean_sphere(pp->sta_lat*TORAD);

	if (strcmp(pp->phase,"PKP")==0){
		if (pp->delta < 140)
			elcor = 0.1;
		else
			elcor = 0.094;
	}
	else{
		for (i=0; (int)(pp->delta+0.5) > delta_step[i]; i++);
		elcor = 0.01 * i;
	}
	geoid_corr = elcor * (epi_hams+sta_hams);

	return geoid_corr;
}


/*#DOC  Title:																*/
/*#DOC    height_above_mean_sphere	(only called by calc_geoid_corr)		*/
/*#DOC  Desc:																*/
/*#DOC    Calculates height above mean sphere.								*/
/*#DOC  Input Arguments:													*/
/*#DOC    lat  - latitude in radians.										*/
/*#DOC  Return:																*/
/*#DOC    Height above mean sphere in km.									*/

double height_above_mean_sphere(double lat)
{
	return 10.738*cos(2*lat)-3.549-0.023*cos(4*lat);
}


/*#DOC  Title:																*/
/*#DOC    calc_resid														*/
/*#DOC  Desc:																*/
/*#DOC    Fills 'ttime','resid','dtdh', and 'dtdd' fields for each phase.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    p  - array of structures containing phases.						*/
/*#DOC    mode  - "all" if require resid for all phases.					*/
/*#DOC            "use" if only want resids that will be used in solution.	*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC	  read_ttime() from read_ttime.c									*/
/*#DOC	  add_to_error() and handle_error() from utils.c.					*/

int calc_resid (struct sol_rec *sp, struct pha_rec p[], char mode[4])
{
	extern double max_depth;		/* Set in read_ttime for tables in use.	*/
	int all,status;
	double obtime;
	char error_part[ERRLEN];
	int i,j;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Set flag to enable/disable residual calculation for unused phases. */
	if (strcmp(mode,"all")==0)
		all = 1;
	else if (strcmp(mode,"use")==0)
		all = 0;
	else{
		sprintf(errstr,"calc_resid: unrecognised mode %s.",mode);
		return 1;
	}

	/* Can't calculate residuals unless have a depth for the source. */
	if (sp->depth == NULLVAL){
		sprintf(errstr,"calc_resid: depthless hypocentre.");
		return 1;
	}

	/* Won't get residuals if source has gone too deep.		*/
	/* Want to fail without writing to stderr in this case. */
	if (sp->depth > max_depth){
		fprintf(logfp,"SOLUTION TOO DEEP\n");
		strcpy(errstr,"");
		return 1;
	}

	/* Loop through all associated phases. */
	for (i=0; i<sp->numphas; i++){

		/* Set default value of resids to NULLVAL. */
		p[i].resid = NULLVAL;

		/* To save time only calculate residuals for phases not used	*/
		/* in the solution (weight_factor=0) if requested to.			*/
		if (all==0 && p[i].weight_factor==0)
			continue;

		/* Timeless or codeless phases don't have residuals. */
		if (p[i].time == NULLVAL || !p[i].phase[0] )
			continue;

		/* pP,sP special case handled in different function.	*/
		if (strcmp(p[i].phase,"pP") == 0 || strcmp(p[i].phase,"sP") == 0)
			continue;

		/* Observed travel time for this phase. */
		obtime = p[i].time - sp->time;

		/* Update ttime, dtdd, and dtdh in phase structure. 	*/
		/* Status 1 is a normal error.							*/
		/* Status 2 is a warning message.						*/
		/* If error when getting residuals that won't be used	*/
		/* (mode="all") set phase ID to null but only warn.		*/
		status = read_ttime(sp, &p[i]);
		if (status){
			sprintf(error_part,"calc_resid: %s %s:",p[i].sta,p[i].phase);
			add_to_error(error_part);
			if (all == 1){
				fprintf(logfp,"WARN: %s\n",errstr);
				strcpy(p[i].phase,"");
			}
			else if (status == 1){
				handle_error();
			}
			else if (status == 2){
				fprintf(logfp,"WARN: %s\n",errstr);
			}
			p[i].weight_factor = 0;
			continue;
		}

		/* Calculate residual for this phase. */
		p[i].resid = obtime - p[i].ttime;

		if (diagnostic){
			fprintf(logfp,"calc_resid: %5s %2s ",p[i].sta,p[i].phase);
			fprintf(logfp,"%0.3f tt=%f ",p[i].delta,p[i].ttime);
			fprintf(logfp,"dtdd=%f dtdh=%f ",p[i].dtdd,p[i].dtdh);
			fprintf(logfp,"r=%f\n",p[i].resid);
		}
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    calc_weight														*/
/*#DOC  Desc:																*/
/*#DOC    Calls the function corresponding to current weighting_type.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    p[]  - array of structures containing phases.						*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC    function to carry out weighting.									*/
/*#DOC    print_pha() from utils.c											*/

/* This function is run each iteration. It needs to be run after:			*/
/*		get_weight_factor													*/
/*		mark_duplicates														*/
/*		calc_resid															*/

int calc_weight (struct sol_rec *sp, struct pha_rec p[])
{
	int status,i;
	double total_weight;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Alpha is calculated in weighting routine.						*/
	/* For first iteration prev_alpha also set by weighting routine.	*/
	if (sp->iteration != 0)
		sp->prev_alpha = sp->alpha;

	/* Values of weighting_type correspond to possible entries for		*/
	/* weighting1 and weighting2 in config.txt							*/
	if (strcmp(sp->weighting_type,"huber")==0){
		if (huber_weight(sp,p))
			return 1;
	}
	else if (strcmp(sp->weighting_type,"buland")==0){
		if (buland_weight(sp,p))
			return 1;
	}
	else if (strcmp(sp->weighting_type,"none")==0){
		if (no_weight(sp,p))
			return 1;
	}

	/* Update sigma, only for use by decide_iter if huber/buland weighting.	*/
	/* sigma = sqrt ( sum simulb**2 / sum weight ). 						*/
	sp->prev_sigma = sp->sigma;
	sp->sigma = 0;
	total_weight = 0;
	for (i=0; i< sp->numphas; i++){
		sp->sigma += p[i].resid*p[i].resid*p[i].weight;
		total_weight += p[i].weight;
	}
	sp->sigma = sqrt(sp->sigma/total_weight);

	if (diagnostic){
		fprintf(logfp,"calc_weight: %s:  ",sp->weighting_type);
		fprintf(logfp,"sigma=%f alpha=%f\n",sp->sigma,sp->alpha);
		print_pha(sp->numphas, p);
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    change_weighting													*/
/*#DOC  Desc:																*/
/*#DOC    Changes 'weighting_type' field in solution structure if required.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to structure containing current solution.			*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Comes here after convergence has been reached with current weighting.	*/
/* If on first weighting type and a second weighting type is given then		*/
/* change weighting to the new type.										*/
/* Also reset the converged flag so that iterating will continue.			*/

int change_weighting (struct sol_rec *sp)
{
	extern char weighting1[];		/* From config file */
	extern char weighting2[];		/* From config file */

	if (strcmp(sp->weighting_type,weighting1)==0){
		if (strcmp(weighting2,"")!=0){
			
			strcpy(sp->weighting_type,weighting2);
			sp->converged = 0;
		}
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    correct_ttime														*/
/*#DOC  Desc:																*/
/*#DOC    Potentialy changes 'ttime' for a phase.							*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    pp - pointer to  a phase structure.								*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC    calc_geoid_corr													*/
/*#DOC    calc_elev_corr if elev_corr_on = 1								*/	

int correct_ttime (struct sol_rec *sp, struct pha_rec *pp)
{
	extern int elev_corr_on;	/* From config.txt. */

	double geoid_corr,source_corr,station_corr;
	double elev;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Apply travel time correction at station and 		*/
	/* epicentre for height of geoid above mean sphere.	*/
	geoid_corr = calc_geoid_corr(sp->lat, pp);
	pp->ttime += geoid_corr;

	if (diagnostic)
		fprintf(logfp,"correct_ttime: %s geoid_corr=%.3f\n",pp->sta,geoid_corr);

	/* Elevation correction already checked if crust correction done. */
	else if (elev_corr_on){
		station_corr = calc_elev_corr(pp);
		pp->ttime += station_corr;

		if (diagnostic){
			fprintf(logfp,"correct_ttime: %s ",pp->sta);
			fprintf(logfp,"elev_corr=%.3f\n",station_corr);
		}
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    decide_iter														*/
/*#DOC  Desc:																*/
/*#DOC    Tests for convergence/divergence of current solution.				*/
/*#DOC    If found updates the corresponding flag in the solution structure.*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    p[] - array of phase structures.									*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

int decide_iter (struct sol_rec *sp, struct pha_rec p[])
{
	extern double avg_weight_thresh;	/* From config file */
	extern double alpha_thresh;			/* From config file */
	extern double dalpha_thresh;		/* From config file */
	extern double dsigma_low;			/* From config file */
	extern double dsigma_high;			/* From config file */

	int ndef,i;
	double avg_weight;
	double dsigma;
	double dalpha;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Calculate average weight */
	avg_weight =0;
	ndef=0;
	for (i=0; i<sp->numphas; i++){
		if (p[i].weight_factor > 0){
			avg_weight += p[i].weight;
			ndef++;
		}
	}
	avg_weight /= ndef;

	/* Calculate changes in alpha and sigma since last iteration */
	dsigma = fabs(sp->sigma - sp->prev_sigma);
	dalpha = fabs(sp->alpha - sp->prev_alpha);

	if (diagnostic){
		fprintf(logfp,"decide: avg=%f ndef=%d ",avg_weight,ndef);
		fprintf(logfp,"alpha=%f prev_alpha=%f\n",sp->alpha,sp->prev_alpha);
	}

	/* Check for convergence */
	if ( fabs(sp->alpha) <= alpha_thresh 	&& 
		 dsigma > dsigma_low				&&
		 dsigma < dsigma_high				&&
		 avg_weight > avg_weight_thresh ){

		sp->converged = 1;

		if (diagnostic){
			fprintf(logfp,"decide: converged: avg=%f da=%f ",avg_weight,dalpha);
			fprintf(logfp,"ds=%f depth=%f\n",dsigma,sp->depth);
		}
		return 0;
	}

	/* Check for divergence */
	if (avg_weight < avg_weight_thresh
	  || (dalpha > dalpha_thresh && dsigma < dsigma_low)){
			
		sp->diverging = 1;

		if (diagnostic){
			fprintf(logfp,"decide: diverging: avg=%f da=%f ",avg_weight,dalpha);
			fprintf(logfp,"ds=%f depth=%f\n",dsigma,sp->depth);
		}
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    get_weight_factor													*/
/*#DOC  Desc:																*/
/*#DOC    Fills 'weight_factor' field for each phase.						*/
/*#DOC  Input Arguments:													*/
/*#DOC    p[] - array structures containing phases.					 		*/
/*#DOC    numphas  - number of phases in p[].								*/
/*#DOC  Return:	0															*/
/*#DOC  Calls:																*/
/*#DOC    print_pha() from utils.c											*/

#define NUM_PHASE_WEIGHTS 12				 /* Must match structure below. */

struct phase_weight_rec {
	char phase[9];
	int delta1;
	int delta2;
	double weight_factor;
};

struct phase_weight_rec phase_weight[NUM_PHASE_WEIGHTS] = {
	{ "P",  0, 20, 1 },
	{ "P",  20, 100, 1 },
	{ "Pn", 0,   8, 1 },
	{ "Pg", 0,   8, 1 },
	{ "Pb", 0,   8, 1 },
	{ "PKP", 110, 180, 0 },
	{ "S" , 0, 10, 1 },
	{ "S" ,10,20, 1},
	{ "S" ,20,65, 0.5 },
	{ "Sn", 0,10, 1 },
	{ "Sg", 0,10, 1 },
	{ "Sb", 0,10, 1 },
};

/* The weight for each phase is multiplied by weight_factor before use.		*/
/* In addition, weight_factor is used as a switch (0 or not) for deciding	*/
/* whether to calculate phase residuals in the main iteration loop and		*/
/* whether to include a phase when calculating average weights etc.			*/
/* The factor is decided depending on phase code and delta using the		*/
/* numbers stored in the phase_weight structure. In addition phases			*/
/* without arrival times get weight_factor 0 and so do phases that have		*/
/* been purged in a previous iteration.										*/

/* This function is run each iteration. It needs to be run after:			*/
/*		calc_delaz															*/
/*		id_pha																*/

int get_weight_factor (struct pha_rec p[], int numphas)
{
	int i,j;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	for (i=0; i<numphas; i++){

		p[i].weight_factor = 0;

		/* Purged phases get a weight factor of 0.	*/
		if (p[i].purged)
			continue;

		/* No point giving a timeless phase a weight.	*/
		if (p[i].time == NULLVAL)
			continue;

		/* Use the phase_weight structure to decide weight_factor.	*/
		/* for this phases phase code and delta.					*/
		for (j=0;j<NUM_PHASE_WEIGHTS;j++){
			if (strcmp(p[i].phase,phase_weight[j].phase)==0){
				if (p[i].delta >= phase_weight[j].delta1 &&
					p[i].delta < phase_weight[j].delta2	)
						p[i].weight_factor = phase_weight[j].weight_factor;
				else
					if (strcmp(p[i].phase,phase_weight[j+1].phase)!=0)
						break;
			}
		}
	}
	if (diagnostic){
		fprintf(logfp,"get_weight_factor done:\n");
		print_pha(numphas, p);
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    guess_loc															*/
/*#DOC  Desc:																*/
/*#DOC    Makes repeated calls to guess_loc_p6 for differnt phase sets.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to solution structure.								*/
/*#DOC    p[] - array of phase structures.									*/
/*#DOC  Return:	0/1 depending on convergence being found.					*/
/*#DOC  Calls:																*/
/*#DOC    select_phase() from this file.									*/
/*#DOC    swap_phase() from this file.										*/
/*#DOC    guess_loc_p6() from this file.									*/
/*#DOC    print_pha() from utils.c											*/

int guess_loc (struct sol_rec *sp, struct pha_rec p[])
{
	int select_phase(struct pha_rec *p,struct pha_rec *p6, struct sol_rec s);
	int swap_phase (struct pha_rec p6[6], int swap);
	int guess_loc_p6 (struct sol_rec *sp, struct pha_rec p6[6]);

	struct pha_rec p6[6];
	int npha;				/* Number of phases in p */
	int np6;				/* Number of phases in p6 (5 or 6) */
	int i;

	/* Switch debug messages from this function on/off (1/0). */
	int  diagnostic = 0;

	/* Select 6 (or 5) phases to use to get initial solution. 	*/
	if ((np6=select_phase(p,p6,*sp))==1)
		return 1;

	npha = sp->numphas;
	sp->numphas=np6;

	if (diagnostic){
		fprintf(logfp,"guess_loc: %d phases for initial solution:\n",np6);
		print_pha(np6,p6);
	}

	/* Loop for different sets of 5 phases. */
	/* Use first 5 of the 6 selected above and if no convergence or the */
	/* residual sum is too big then swap in the 6th for each in turn.	*/
	for (i=0; i<5; i++){

		/* No swapping possible if only 5 phases. */
		if (np6==5)
			i=4;

		/* guess_loc_p6 returns 1 on failure - try next combination.	*/
		if (guess_loc_p6 (sp,p6))
			swap_phase(p6,i);
		else
			break;
	}
	/* If tried them all then return failure. */
	if (i==5)
		return 1;

	sp->numphas = npha;
	return 0;
}

/*#DOC  Title:																*/
/*#DOC    select_phase														*/
/*#DOC  Desc:																*/
/*#DOC    Picks 6 phases from those available to initialise a solution.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    p[]  - array of phase structures.									*/
/*#DOC    p6[] - array of phase structures - subset of p[].					*/
/*#DOC    nsta - number of unique stations in p[].							*/
/*#DOC    numphas - number of phases in p[].								*/
/*#DOC  Return: 1 on failure or number of phases found (5 or 6).			*/
/*#DOC  Calls:																*/
/*#DOC	  compatibility_check() from this file.								*/

int select_phase(struct pha_rec p[], struct pha_rec p6[6], struct sol_rec s)
{
	int compatibility_check (struct pha_rec *p1, struct pha_rec *p2);

	int *misfit;
	int ncomp;
	int most_misfits,worst;
	double step;
	int i,j,k,m;

	/* Switch debug messages from this function on/off (1/0). */
	int  diagnostic = 0;

	/* Mark subset of readings want to compare weight=1 on init phase.	*/
	for (i=0; i<s.numphas; i++)
		p[i].weight=1;

	/* Only consider readings without P init if less than 6 readings.	*/
	ncomp=s.nsta;
	for (i=s.numphas-1; i>=0; i--){
		if (ncomp<=6)
			break;
		if (p[i].init==0)
			continue;
		if (p[i].weight==0)
			continue;

		if (strcmp(p[i].rep_phase,"P")){

			/* Weight down phases in this reading. */
			for (j=i; j<s.numphas; j++){
				if (p[j].rdid != p[i].rdid)
					break;
				p[j].weight=0;
			}

			/* Don't decrement ncomp if there's still a reading for this sta. */
			for (j=0; j<s.numphas; j++)
				if (strcmp(p[j].sta,p[i].sta)==0 && p[j].weight>0)
					break;
			if (j==s.numphas)
				ncomp--;

			if (diagnostic){
				fprintf(logfp,"select_phase: not looking at ");
				fprintf(logfp,"%s: init=%s ",p[i].sta,p[i].rep_phase);
				fprintf(logfp,"ncomp=%d\n",ncomp);
			}
		}
	}
	if (diagnostic)
			fprintf(logfp,"select_phase: ncomp=%d after loosing non P\n",ncomp);

	/* Only compare more than 10 stations if more than 10 have S.		*/
	for (i=s.numphas-1; i>=0; i--){
		if (ncomp<11)
			break;
		if (p[i].init==0)
			continue;
		if (p[i].weight==0)
			continue;

		if (p[i].s_min_p_time == NULLVAL){

			/* Weight down phases in this reading. */
			for (j=i; j<s.numphas; j++){
				if (p[j].rdid != p[i].rdid)
					break;
				p[j].weight=0;
			}

			/* Don't decrement ncomp if there's still a reading for this sta. */
			for (j=0; j<s.numphas; j++)
				if (strcmp(p[j].sta,p[i].sta)==0 && p[j].weight>0)
					break;
			if (j==s.numphas)
				ncomp--;

			if (diagnostic){
				fprintf(logfp,"select_phase: not looking at ");
				fprintf(logfp,"%s: more than 11 others\n",p[i].sta);
			}
		}
	}
	if (diagnostic)
			fprintf(logfp,"select_phase: ncomp=%d after loosing non S\n",ncomp);

	/* Array to count how many other readings each reading can't be with. */
	if ((misfit = calloc (s.numphas, sizeof(int))) == NULL){
		sprintf(errstr,"select_phase: memory problem: misfit.");
		return 1;
	}
	for (i=0; i<s.numphas; i++)
		misfit[i]=0;

	/* Compare every initial phase with every other initial phase. */
	most_misfits=0;
	while(1){

		/* If less than 6 left try an unused one. */
		i=0;
		while (ncomp<6 && i<s.numphas){
			if (p[i].init==1 && p[i].weight==0){

				/* Only reinstate if sta not already represented. */
				for (j=0; j<s.numphas; j++)
					if (strcmp(p[j].sta,p[i].sta)==0 && p[j].weight>0)
						break;

				if (j==s.numphas){
					for (k=i; k<s.numphas; k++){
						if (p[k].rdid != p[i].rdid)
							break;
						p[k].weight=1;
					}
					ncomp++;

					if (diagnostic)
						fprintf(logfp,"select_phase: reinstate %s\n",p[i].sta);
				}
			}
			i++;
		}

		/* If less than 5 left then give up. */
		if (ncomp<5)
			break;

		for (i=0; i<s.numphas; i++){

			if (p[i].init==0)
				continue;
			if (p[i].weight<1)
				continue;

			for (j=i+1; j<s.numphas; j++){

				if (p[j].init==0)
					continue;
				if (p[j].weight<1)
					continue;

				/* Check returns 1 if phases can't be in same event. */
				if (compatibility_check(&p[i],&p[j])){

					if (++misfit[i] > most_misfits){
						most_misfits = misfit[i];
						worst = i;
					}
					if (++misfit[j] > most_misfits){
						most_misfits = misfit[j];
						worst = j;
					}						
				}
			}
		}
		if (most_misfits==0)
			break;

		if (diagnostic){
			fprintf(logfp,"select_phase: discarding %s ",p[worst].sta);
			fprintf(logfp,"didn't fit with %d others\n",most_misfits);
		}

		/* Mark this reading and any duplicates as unwanted in location. */
		for (i=worst; i<s.numphas; i++){
			if (p[i].rdid != p[worst].rdid)
				break;
			p[i].weight=-1;
		}

		/* Don't decrement ncomp if there's still a reading for this sta. */
		for (i=0; i<s.numphas; i++)
			if (strcmp(p[i].sta,p[worst].sta)==0 && p[i].weight>0)
				break;
		if (i==s.numphas)
			ncomp--;

		/* Reset misfit array for next trawl. */
		most_misfits = 0;
		for (i=0; i<s.numphas; i++)
			misfit[i]=0;

	}
	free(misfit);

	if (diagnostic){
		fprintf(logfp,"select_phase: ncomp=%d after compatibility\n",ncomp);
		print_pha(s.numphas,p);
	}

	/* If less than 5 left then give up. */
	if (ncomp<5)
		return 1;

	/* Want only 6 initial phases (or 5 if that's all there is).	*/
	/* Could do this in one clever loop but simpler this way.		*/
	k=0;					/* Index of new array of 5/6 phases.	*/
	if (ncomp==6 || ncomp==5){
		for (i=0; i<s.numphas; i++){
			if (p[i].init==0)
				continue;
			if (p[i].weight<1)
				continue;

			/* Check whether this station already included. */
			for (m=0; m<k; m++)
				if (strcmp(p[i].sta,p6[m].sta)==0)
					break;
			if (m!=k)
				continue;

			p6[k++] = p[i];
			p[i].weight = 2;
		}
	}
	else{
		step = (ncomp-1)*0.2;
		j=0;						/* Counts unique station codes.	*/
		for (i=0; i<s.numphas; i++){

			if (p[i].init==0)
				continue;
			if (p[i].weight<1)
				continue;

			/* Check whether this station already included. */
			for (m=0; m<k; m++)
				if (strcmp(p[i].sta,p6[m].sta)==0)
					break;
			if (m!=k)
				continue;

			/* If step=1 array might be overfilled - need k<6 check. */
			if (j==(int)(step*k+0.5) && k<6){
				p6[k++] = p[i];
				p[i].weight = 2;
			}
			j++;
		}
		ncomp=6;
	}

	/* Set phase = P for all p6 phases. */
	for (i=0; i<ncomp; i++){
		strcpy(p6[i].rep_phase,"");
		strcpy(p6[i].phase,"P");
	}
	return ncomp;
}

/*#DOC  Title:																*/
/*#DOC    compatibility_check												*/
/*#DOC  Desc:																*/
/*#DOC    Checks arrival time difference between two initial phases.		*/ 
/*#DOC  Input Arguments:													*/
/*#DOC    p1 - phase structure.												*/
/*#DOC    p2 - phase structure.												*/
/*#DOC  Return:																*/
/*#DOC    0 if possible that phases come from same event.					*/
/*#DOC    1 if impossible that phases come from same event.					*/

/* Maximum possible difference between two travel times to an unknown		*/
/* source would be when one station was at the source ie - the diffence in	*/
/* arrival times at two stations must be less than the travel time between	*/
/* one station and a dummy source at the other.								*/
/* If have S minus P times for either or both stations than delta and		*/
/* ttime fields of the P phase will have been set in get_unassoc to			*/ 
/* correspond, use these to narrow down the maximum time difference.		*/

int compatibility_check (struct pha_rec *p1, struct pha_rec *p2)
{
	struct sol_rec dumsol;
	double ttime,time_diff,min_diff,max_diff;
	double delta_min;

	/* Status 1 means readings not in same event. */
	int status = 0;

	/* Switch debug messages from this function on/off (1/0). */
	int  diagnostic = 0;

	/* Dummy solution for finding travel times given delta. */
	dumsol.depth = SEARCH_START_DEPTH;
	dumsol.lat   = NULLVAL;			/* To switch off geoid correction. */

	if (diagnostic){
		fprintf(logfp,"compatibility_check: ");
		fprintf(logfp,"\t%d %s cf %d %s\n",p1->rdid,p1->sta,p2->rdid,p2->sta);
	}

	/* Arrival time difference between stations. */
	time_diff = p2->time - p1->time;

	/* Delta between stas is minimum sum of deltas between source and stas.	*/
	delta_min = calc_delta(p1->dircos,p2->dircos);

	/* If have S minus P for both simply check sum of deltas. */
	if (p1->s_min_p_time != NULLVAL && p2->s_min_p_time != NULLVAL){

		if (p1->delta + p2->delta < delta_min)
			status = 1;

		if (diagnostic){
			fprintf(logfp,"\tboth: del1=%.1f del2=%.1f ",p1->delta,p2->delta);
			fprintf(logfp," delta_min=%.1f status=%d\n",delta_min,status);
		}
	}

	/* Only have S minus P for the first arriving phase p1.	*/
	else if ( p1->s_min_p_time != NULLVAL ){

		/* Second station must be close enough to have P recorded.	*/
		if ( delta_min > p1->delta + SEARCH_PDIFF_DELTA ){
			status = 1;

			if (diagnostic){
				fprintf(logfp,"\tp1 only: del1=%.1f ",p1->delta);
				fprintf(logfp,"delta_min=%.1f status=%d\n",delta_min,status);
			}
		}
		else{
			/* Check that source could be between stations. */
			if (p1->delta*2 <= delta_min){

				/* Calculate minimum possible travel time for p2 */			
				p2->delta = delta_min - p1->delta;
				strcpy(p2->phase,"P");
				read_ttime(&dumsol,p2);

				/* Minimum p2 travel time minus p1 S-P travel time 	*/
				/* is minimum arrival time diference. 				*/
				min_diff = p2->ttime - p1->ttime;

				if ( time_diff < min_diff)
					status = 1;
			}
			/* Fall back on checking arrival times as below for no p-s */
			else{
				/* Treat second station as source to get travel time */
				p2->delta = delta_min;
				strcpy(p2->phase,"P");
				read_ttime(&dumsol,p2);
				max_diff = p2->ttime;

				/* max_diff==0 for same station twice	*/
				/* - don't want to loose those here.	*/
				if (max_diff && time_diff > max_diff )
					status = 1;
			}
			if (diagnostic){
				fprintf(logfp,"\tp1 only: del1=%.1f ",p1->delta);
				fprintf(logfp,"delta_min=%.1f ",delta_min);
				fprintf(logfp,"tt1=%.1f max_diff=%.1f ",p1->ttime,max_diff);
				fprintf(logfp,"time_diff=%.1f status=%d\n",time_diff,status);
			}
		}
	}
	/* If only have S minus P for the second arriving phase then use		*/
	/* criteria that p1 recorded closer than p2 and that arrival time		*/
	/* difference must be less than the maximum travel time difference.		*/
	else if ( p2->s_min_p_time != NULLVAL ){

		/* Minimum p1 delta must be less than p2 delta. */
		if ( fabs(delta_min - p2->delta) > p2->delta ){
			status = 1;

			if (diagnostic){
				fprintf(logfp,"\tp2 only: del2=%.1f ",p2->delta);
				fprintf(logfp,"delta_min=%.1f status=%d\n",delta_min,status);
			}
		}
		else{

			/* Calculate minimum possible ttime for p1. */
			strcpy(p1->phase,"P");
			p1->delta = fabs(delta_min - p2->delta);
			read_ttime(&dumsol,p1);

			/* p2 S-P travel time minus p1 minumum travel time	*/
			/* is maximum arrival time diference. 				*/
			max_diff = p2->ttime - p1->ttime;

			if ( time_diff > max_diff )
				status = 1;

			if (diagnostic){
				fprintf(logfp,"\tp2 only: max_diff=%.1f ",max_diff);
				fprintf(logfp,"tt1=%.1f tt2=%.1f ",p1->ttime,p2->ttime);
				fprintf(logfp,"time_diff=%.1f status=%d\n",time_diff,status);
			}
		}
	}
	/* No S minus P - check maximum arrival time difference. */
	else{

		/* Treat first station as source to get travel time between stations. */
		p2->delta = delta_min;
		strcpy(p2->phase,"P");

		/* If stations too far apart need to split distance into two.	*/
		if (delta_min > 100){
			p2->delta -= 100;
			read_ttime(&dumsol,p2);
			ttime = p2->ttime;
			p2->delta = 100;
			read_ttime(&dumsol,p2);
			p2->ttime += ttime;
		}
		else 
			read_ttime(&dumsol,p2);
		max_diff = p2->ttime;

		/* max_diff==0 for same station twice	*/
		/* - don't want to loose those here.	*/
		if (max_diff && time_diff > max_diff)
			status = 1;

		if (diagnostic){
			fprintf(logfp,"\tnone: max_diff=%.1f ",max_diff);
			fprintf(logfp," time_diff=%.1f status=%d\n",time_diff,status);
		}
	}
	return status;
}

/*#DOC  Title:																*/
/*#DOC    swap_phase														*/
/*#DOC  Desc:																*/
/*#DOC    Swaps phases within p6 so that a different 5 are tried.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    p6[] - array of phase structures.									*/
/*#DOC    swap - number of swaps already done.								*/
/*#DOC  Return:	0															*/

/* Keep spare phase here for simplicity. */
struct pha_rec extra;

int swap_phase(struct pha_rec p6[6], int swap)
{

	/* Swap p6[5] with each in turn - but need to shift 2nd to 1st	*/
	/* when swapping 1st so that 1st phase always 1st arrival.		*/
	if (swap==0){
		extra = p6[0];
		p6[0] = p6[1];
		p6[1] = p6[5];
	}
	else if (swap==1){
		p6[1] = p6[0];
		p6[0] = extra;
		extra = p6[1];
		p6[1] = p6[5];
	}					
	else if (swap==4){
		return 0;
	}
	else{
		p6[swap-1] = extra;
		extra   = p6[swap];
		p6[swap]   = p6[5];
	}
	return 0;
}

/*#DOC  Title:																*/
/*#DOC    guess_loc_p6														*/
/*#DOC  Desc:																*/
/*#DOC    Estimates time/lat/lon using overlapping spheres.					*/
/*#DOC    Then estimates depth by choosing one with smallest residuals.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to solution structure.								*/
/*#DOC    p6[] - array of phase structures.									*/
/*#DOC  Return:	0/1 depending on convergence being found.					*/
/*#DOC  Calls:																*/
/*#DOC    look_up_delta() from this file.									*/
/*#DOC    get_coeff() from this file.										*/
/*#DOC    solve_guess() from this file.										*/
/*#DOC    read_ttime() from read_ttime.c									*/
/*#DOC    calc_delta() from utils.c											*/

int guess_loc_p6 (struct sol_rec *sp, struct pha_rec p6[])
{
	int look_up_delta(struct pha_rec *pp, double ttime);
	int get_coeff(struct pha_rec *pp, double coeff[]);
	int solve_guess(double coeff[5][5], struct sol_rec *sp);

	struct sol_rec s;
	double coeff[5][5];		/* Maximum of 4 unkowns - don't use index 0. */
	double ttime,resid;
	double prev_delta0,delta0;
	double residsum,abs_residsum;
	double prev_depth,prev_residsum,prev_abs_residsum;
	double t1 = 60.5;
	double ddelta_thresh = 0.1;
	int max_iter = 6;
	int i,j,k;

	/* Switch debug messages from this function on/off (1/0). */
	int  diagnostic = 0;

	/* Initialise solution structure. */
	sp->converged = 0;
	sp->numphas   = 5;
	sp->depth     = SEARCH_START_DEPTH;

	/* Loop to solve for lat and lon */
	for (i=0; i<max_iter; i++){

		if (diagnostic)
			fprintf(logfp,"guess_loc_p6: lat/lon loop iter=%d t1=%f\n",i,t1);

		/* Loop over first 5 phases in p6. */
		for (j=0; j<5; j++){

			/* Set travel time for arrival. */
			ttime = t1 + p6[j].time - p6[0].time;

			/* Use travel time to set delta. */
			look_up_delta (&p6[j], ttime);

			if (diagnostic){
				fprintf(logfp,"\t%s %s ",p6[j].sta,p6[j].phase);
				fprintf(logfp,"ttime=%.1f delta=%.1f\n",ttime,p6[j].delta);
			}

			if (p6[j].delta == NULLVAL)
				return 1;

			if (j==0 /*&& i==0*/)
				prev_delta0 = p6[j].delta;

			/* Use delta to form sphere equation for phase */
			get_coeff(&p6[j],coeff[j]);

			if (j){
				for (k=1; k<=4; k++)
					coeff[j][k] -= coeff[0][k];
				coeff[j][4] *= -0.5;
			}
			if (diagnostic){
				fprintf(logfp,"\tcoeff: %.1f ",coeff[j][1]);
				fprintf(logfp,"%.1f %.1f ",coeff[j][2],coeff[j][3]);
				fprintf(logfp,"%.1f\n",coeff[j][4]);
			}
		}

		/* Solve equations representing sphere per phase */
		if (solve_guess(coeff,sp)){
			add_to_error("guess_loc:");
			handle_error();
			return 1;
		}

		/* Check for convergence and increment ttime for nearest station. */
		/* Loop over first 5 phases in p6. */
		residsum=0;
		for (j=0; j<5; j++){

			p6[j].delta = calc_delta(p6[j].dircos, sp->dircos);

			if (p6[j].delta< SEARCH_PDIFF_DELTA)
				strcpy(p6[j].phase,"P");
			else if (p6[j].delta < SEARCH_MAX_DELTA)
				strcpy(p6[j].phase,"Pdiff");
			else
				return 1;

			if (diagnostic){
				fprintf(logfp,"\tconvergence check: %s ",p6[j].phase);
				fprintf(logfp,"%s delta=%.1f\n",p6[j].sta,p6[j].delta);
			}

			if (j==0){
				if (abs(prev_delta0 - p6[0].delta) < ddelta_thresh){
					s.converged = 1;
					break;
				}
				prev_delta0 = p6[0].delta;
			}

			else{
				if (read_ttime(sp,&p6[j])){
					add_to_error("guess_loc_p6:");
					handle_error();
					return 1;
				}
			}
			residsum += (p6[j].ttime-p6[0].ttime) - (p6[j].time-p6[0].time);
		}
		if (sp->converged)
			break;

		t1 = p6[0].ttime + residsum/4;

	} /* lat and lon iterations. */

	/* Keep going if converged or went through max_iter iterations. */
	if (diagnostic)
		fprintf(logfp,"\tconvergence = %d\n",sp->converged);

	/* Get origin time and depth by minimising residuals between	*/
	/* A - A0 and T - T0 where A is arrival time and T travel time.	*/
	prev_residsum = NULLVAL;	/* So that residsum smaller on 1st iter. */
	for (i=0; i<8; i++){

		if (i==0)
			sp->depth = 0;
		else if (i==1)
			sp->depth = 33;
		else if (i==2)
			sp->depth = 50;
		else
			sp->depth = 100 + (i-3)*100;

		calc_delaz(sp,p6);
		id_pha(sp,p6,1);

		if (diagnostic)
			fprintf(logfp,"guess_loc_p6: depth loop: depth=%f\n",sp->depth);

		residsum = abs_residsum = 0;
		for(j=0; j<5; j++){

			if (diagnostic){
				fprintf(logfp,"\t\t %s %s ",p6[j].sta,p6[j].phase);
				fprintf(logfp,"delta=%.1f ",p6[j].delta);
			}

			/* id_pha sets phase to "" for dodgy Pdiff delta phases. */
			if (p6[j].delta >= SEARCH_PDIFF_DELTA){
				if (p6[j].delta > SEARCH_MAX_DELTA)
					return 1;
				strcpy(p6[j].phase,"Pdiff");
			}

			if (read_ttime(sp,&p6[j])){
				add_to_error("guess_loc_p6:");
				handle_error();
				return 1;
			}

			resid = (p6[j].time - p6[0].time) - (p6[j].ttime - p6[0].ttime);
			residsum += resid;
			abs_residsum += fabs(resid);

			if (diagnostic)
				fprintf(logfp,"resid=%.1f\n",resid);
		}
		if (diagnostic)
			fprintf(logfp,"\tresidsum=%f prev=%f\n",residsum,prev_residsum);

		if (fabs(residsum) > fabs(prev_residsum))
			break;

		prev_depth = sp->depth;
		prev_residsum = residsum;
		prev_abs_residsum = abs_residsum;
	}

	if (diagnostic){
		fprintf(logfp,"guess_loc_p6: i=%d ",i);
		fprintf(logfp,"prev_abs_residsum=%f\n",prev_abs_residsum);
	}
	/* Pretty arbitrary check from Engdahl + Gunst. */
	if (prev_abs_residsum > 30)
		return 1;

	if (i==8)
		sp->depth = 33;
	else
		sp->depth = prev_depth;

	sp->time = p6[0].time - p6[0].ttime - prev_residsum *0.2;

	return 0;
}

/*#DOC  Title:																*/
/*#DOC    look_up_delta														*/
/*#DOC  Desc:																*/
/*#DOC    Adds delta correspoding to a given P ttime to a phase structure.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    pp - pointer to a phase structure.								*/
/*#DOC    ttime - P wave travel time.										*/
/*#DOC  Return:	0															*/
/*#DOC  Calls:																*/
/*#DOC    read_ttime() from read_ttime.c.									*/

int look_up_delta(struct pha_rec *pp, double ttime)
{
	struct sol_rec dumsol;
	double ttime1, ttime2;
	int i;

	dumsol.depth = SEARCH_START_DEPTH;
	dumsol.lat   = NULLVAL;			/* To switch off geoid correction. */

	for (i=0; i<SEARCH_PDIFF_DELTA; i++){

		pp->delta=i;
		read_ttime(&dumsol,pp);

		if (pp->ttime < ttime)
			ttime1 = pp->ttime;
		else{
			ttime2 = pp->ttime;
			break;
		}
	}
	if (i==SEARCH_PDIFF_DELTA){
		strcpy(pp->phase,"Pdiff");
		for (i=SEARCH_PDIFF_DELTA; i<SEARCH_MAX_DELTA; i++){

			pp->delta=i;
			read_ttime(&dumsol,pp);

			if (pp->ttime < ttime)
				ttime1 = pp->ttime;
			else{
				ttime2 = pp->ttime;
				break;
			}
		}
		strcpy(pp->phase,"P");
	}

	if (i==0)
		pp->delta = 0;
	else if (i==SEARCH_MAX_DELTA)
		pp->delta = NULLVAL;
	else
		pp->delta = i - (ttime2 - ttime)/(ttime2 - ttime1);

	return 0;
}

/*#DOC  Title:																*/
/*#DOC    get_coeff															*/
/*#DOC  Desc:																*/
/*#DOC    	*/
/*#DOC  Input Arguments:													*/
/*#DOC    pp - pointer to a phase structure.								*/
/*#DOC    coeff - array of equation coeeficients for a single equation.		*/
/*#DOC  Return:	0															*/

int get_coeff(struct pha_rec *pp, double coeff[])
{
	double gcolat,coscolat,sincolat,coslon,sinlon;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic=0;

	gcolat = PI/2 - atan(ELLIPT * tan(TORAD*pp->sta_lat));
	coscolat = cos(gcolat);
	sincolat = sin(gcolat);
	coslon = cos(TORAD*pp->sta_lon);
	sinlon = sin(TORAD*pp->sta_lon);

	coeff[1]  = EARTH_RADIUS * sincolat *coslon;
	coeff[2]  = EARTH_RADIUS * sincolat *sinlon;
	coeff[3]  = EARTH_RADIUS * coscolat;
	coeff[4]  = 4 * EARTH_RADIUS * ( EARTH_RADIUS - 33);
	coeff[4] *= sin(TORAD*pp->delta*0.5) * sin(TORAD*pp->delta*0.5);
	coeff[4] += 1089;				/* 1089 is 33*33 */

	if (diagnostic){
		fprintf(logfp,"get_coeff: %s: %.1f ",pp->sta,coeff[1]);
		fprintf(logfp,"%.1f %.1f %.1f\n",coeff[2],coeff[3],coeff[4]);
	}
	return 0;
}

/*#DOC  Title:																*/
/*#DOC    solve_guess														*/
/*#DOC  Desc:																*/
/*#DOC    	*/
/*#DOC  Input Arguments:													*/
/*#DOC    coeff - array of equation coeeficients for a 5 equations.			*/
/*#DOC    sp - pointer to solution structure.								*/
/*#DOC  Return:	0/1 dependin on whether equations are soluble or not.		*/
/*#DOC  Calls:																*/
/*#DOC	  calc_delta() from utils.c.										*/

int solve_guess(double coeff[5][5], struct sol_rec *sp)
{
	double a[5][5], b[5];
	double ff,trma,trmb,trmc,trmd,trme,trmf,trmg;
	double x[5];
	double rr;
	double coscolat,sincolat,sinlon ,coslon,colat;

	int i,j,k,jj,jk;
	int nunk =3;

	/* Switch debug messages from this function on/off (1/0). */
	int  diagnostic = 0;

	/* Nullify normal equations */
	for (i=1; i<=4; i++){
		b[i] = 0;
		for (k=1; k<=4; k++)
			a[k][i] = 0;
	}

	/* Fill normal equations */
	/* Don't use nearest station. */
	for (i=1; i<5; i++){

		for (j=1; j<=nunk; j++){
			b[j] += coeff[i][j]*coeff[i][4];
			for (k=1; k<=j; k++)
				a[k][j] += coeff[i][j]*coeff[i][k];
		}
	}

	if (diagnostic){
		fprintf(logfp,"solve_guess:\n");
		fprintf(logfp,"a1=%.1f %.1f %.1f %f\n",a[1][1],a[1][2],a[1][3],a[1][4]);
		fprintf(logfp,"a2=%.1f %.1f %.1f %f\n",a[2][1],a[2][2],a[2][3],a[2][4]);
		fprintf(logfp,"a3=%.1f %.1f %.1f %f\n",a[3][1],a[3][2],a[3][3],a[3][4]);
		fprintf(logfp,"a4=%.1f %.1f %.1f %f\n",a[4][1],a[4][2],a[4][3],a[4][4]);
		fprintf(logfp," b=%.1f %.1f %.1f %.1f\n\n",b[1],b[2],b[3],b[4]);
	}

	if (a[1][2]*a[1][3] == 0){
		sprintf(errstr,"solve_guess: multiple is zero.");
		return 1;
	}

	/* Solve */

	ff = a[1][2]/a[1][1];
	trma = a[2][3]/a[1][3] - ff;
	trmb = a[2][2]/a[1][2] - ff;
	if (!trmb){
		sprintf(errstr,"solve_guess: trmb = 0");
		return 1;
	}
	ff = a[1][3]/a[1][1];
	trmc = a[2][3]/a[1][2] - ff;
	trmf = a[3][3]/a[1][3] - ff;
	ff = b[1]/a[1][1];
	trmd = b[2]/a[1][2] - ff;
	trme = b[3]/a[1][3] - ff;
	trmg = trmb*trmf - trma*trmc;
	if (!trmg){
		sprintf(errstr,"solve_guess: trmg = 0");
		return 1;
	}
	x[3] = (trmb*trme-trma*trmd)/trmg;
	x[2] = (trmd - trmc*x[3])/trmb;
	x[1] = (b[1] - a[1][3]*x[3] - a[1][2]*x[2])/a[1][1];

	if (x[1]==0 && x[2]==0){
		sprintf(errstr,"solve_guess: x[1] and x[2] = 0");
		return 1;
	}
	rr = sqrt(x[1]*x[1] + x[2]*x[2] + x[3]*x[3]);
	ff = sqrt(x[1]*x[1] + x[2]*x[2]);

	coscolat = x[3]/rr;
	sincolat = ff/rr;
	sinlon = x[2]/ff;
	coslon = x[1]/ff;

	colat   = acos(coscolat) * (sincolat/fabs(sincolat));
	sp->lat = atan(tan(PI*0.5-colat)/ELLIPT)/TORAD;
	sp->lon = (acos(coslon)/TORAD) * (sinlon/fabs(sinlon));

	calc_dircos(sp->lat,sp->lon,sp->dircos);

	if (diagnostic)
		fprintf(logfp,"solve_guess: lat=%.1f lon=%.1f\n",sp->lat,sp->lon);

	return 0;
}


/*#DOC  Title:																*/
/*#DOC    huber_weight														*/
/*#DOC  Desc:																*/
/*#DOC    Fills 'alpha' in solution structure and 'weight' for each phase.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    p  - array of structures containing phases.						*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Simple weighting designed to get basic solution before using more	*/
/* complex but less stable 'Buland' weighting to get final solution.	*/

/* From Fortran code (nwtfcn used to switch between Huber and Jeffreys):

c   When nwtfcn < 0, function wght returns the Huber-t weight for
c   residual r in seconds.  The cutoff, s, between least squares
c   behavior and one-norm behavior has been determined empirically to
c   optimize efficiency against the distribution of teleseismic
c   travel-time residuals.  For this case, the efficiency is about
c   .905 which is quite close to the optimal efficiency achievable
c   with a non-decreasing influence function.  Except for the
c   non-linearity inherent in the earthquake location problem, this
c   weighting would always produce unique results.  When nwtfcn >= 0,
c   function wght returns the Jeffreys weight using mu of .05 and
c   sigma of 1.145.  This should result in efficiency in excess of .99.
c   However, the result is not unique unless started from the converged
c   result of the first weighting.  Programmed on 17 March 1987 by
c   R. Buland.

*/

#define CUT_OFF 0.8275	/* From Buland's Fortran */

int huber_weight (struct sol_rec *sp, struct pha_rec p[])
{
	extern int    min_phases;			/* From config file */
	extern double init_max_resid;		/* From config file */
	extern double max_resid;			/* From config file */

	int i,ndef;
	double spread;
	double total_weight;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* First iteration:												*/
	/* Initialise alpha as average residual.						*/
	/* Then calculate initial weights.								*/
	/* Afterwards, calculate weights again as usual.				*/
	if (sp->iteration == 0){
		sp->alpha=0;
		ndef=0;

		for (i=0; i<sp->numphas; i++){
			if (p[i].weight_factor > 0 && fabs(p[i].resid) < init_max_resid){
				sp->alpha += p[i].resid;
				ndef++;

			}
			if (diagnostic){
				fprintf(logfp,"huber_weight: init: %s %s ",p[i].sta,p[i].phase);
				fprintf(logfp,"%f %f\n",p[i].weight_factor,p[i].resid);
			}
		}
		if (ndef < min_phases ){
			fprintf(logfp,"TOO FEW PHASES huber_weight init: ndef=%d\n",ndef);
			strcpy(errstr,"");
			return 1;
		}
		sp->alpha /= ndef;
		sp->prev_alpha = sp->alpha;

		if (diagnostic)
			fprintf(logfp,"huber_weight: init alpha=%f n=%d ",sp->alpha,ndef);

		for (i=0; i<sp->numphas; i++){
			if (fabs(p[i].resid) < max_resid){
				spread = fabs(p[i].resid - sp->alpha);
				if (spread <= CUT_OFF)
					p[i].weight = 1;
				else
					p[i].weight = CUT_OFF/spread;

				p[i].weight *= p[i].weight_factor;
			}
			else
				p[i].weight = 0;
		}
	}

	/* First calculate alpha. */
	sp->alpha=0;
	total_weight=0;
	ndef=0;
	for (i=0; i<sp->numphas; i++){
		if (p[i].weight_factor > 0 && fabs(p[i].resid) < max_resid){
			sp->alpha += p[i].resid*p[i].weight;
			total_weight += p[i].weight;
			ndef++;
		}
		if (diagnostic){
			fprintf(logfp,"huber_weight: %s %s ",p[i].sta,p[i].phase);
			fprintf(logfp,"%f %f ",p[i].weight_factor,p[i].weight);
			fprintf(logfp,"%f\n",p[i].resid);
		}
	}
	if (ndef < min_phases ){
		fprintf(logfp,"TOO FEW PHASES huber_weight: ndef=%d\n",ndef);
		strcpy(errstr,"");
		return 1;
	}
	sp->alpha /= total_weight;

	if (diagnostic){
		fprintf(logfp,"huber_weight: alpha=%f ndef=%d ",sp->alpha,ndef);
		fprintf(logfp,"total_weight=%f\n",total_weight);
	}

	/* Then calculate weights. */
	for (i=0; i<sp->numphas; i++){
		if (fabs(p[i].resid) < max_resid){
			spread = fabs(p[i].resid - sp->alpha);
			if (spread <= CUT_OFF)
				p[i].weight = 1;
			else
				p[i].weight = CUT_OFF/spread;

			p[i].weight *= p[i].weight_factor;
		}
		else
			p[i].weight = 0;
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    id_jb																*/
/*#DOC  Desc:																*/
/*#DOC    Alters the 'phase' field of a phase if it does not fit JB tables.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    pp - pointer to a phase structure.								*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Uses:																*/
/*#DOC    read_ttime() from read_ttime.c									*/
/*#DOC    add_to_error() and handle_error() from utils.c					*/

int id_jb (struct sol_rec *sp, struct pha_rec *pp)
{
	extern double max_resid;			/* From config file */
	char error_part[ERRLEN];

	double g_time,b_time,n_time;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* If source is too deep any call to read_ttime will give error. */
	if (sp->depth > EARTH_RADIUS*MAX_DEPTH)
		return 0;

	/* Check for PKP that's realy a P or P diff. */
	if (strcmp(pp->phase,"PKP") ==0)
		if (pp->delta < PKP_MIN_DELTA)
				strcpy(pp->phase,"P");

	/* Check for Pdiff that's realy P. */
	if (strcmp(pp->phase,"Pdiff") ==0)
		if (pp->delta < PDIFF_MIN_DELTA)
			strcpy(pp->phase,"P");
	
	if (strcmp(pp->phase,"P") ==0 || strcmp(pp->phase,"Pg")==0 ||
		strcmp(pp->phase,"Pb")==0 || strcmp(pp->phase,"Pn")==0 ){

		/* Devide into crustal and non-crustal */
		if (pp->delta > CRUSTAL_P_DELTA || sp->depth > MOHO){
			strcpy(pp->phase,"P");

			/* Check for Pdiff. If 102<delta<106 and residual 		*/
			/* huge then probably PKiKP - leave phase code blank.	*/
			if (pp->delta >= PDIFF_MIN_DELTA){
			strcpy(pp->phase,"Pdiff");
			if (read_ttime(sp, pp) )
				strcpy(pp->phase,"");
			else if (fabs(pp->time-sp->time-pp->ttime) > max_resid)
				strcpy(pp->phase,"");
			}
			/* Check for PKP. If Pdiff residual small then not PKP.	*/
			if (pp->delta>PKP_MIN_DELTA && strcmp(pp->phase,"Pdiff"))
				strcpy(pp->phase,"PKP");
		}
		/* P crustal phases. */
		else{
			/* In upper crust - could be Pg, Pb, or Pn. */
			/* If reported as P then assume its first arriving phase.	*/
			/* Times for impossible phases are set to -NULLVAL so that	*/
			/* when times are compared these will be the biggest.		*/
			if (sp->depth < CONRAD){
				if (strcmp(pp->phase,"P")==0){

					/* Pg */
					strcpy(pp->phase,"Pg");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					g_time = pp->ttime;

					/* Pb */
					strcpy(pp->phase,"Pb");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					b_time = pp->ttime;

					/* Pn */
					strcpy(pp->phase,"Pn");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					n_time = pp->ttime;

					if (g_time == b_time == n_time == -NULLVAL)
						strcpy(pp->phase,"");
					else if (g_time <= b_time && g_time < n_time)
						strcpy(pp->phase,"Pg");
					else if (b_time < g_time && b_time < n_time)
						strcpy(pp->phase,"Pb");
					else
						strcpy(pp->phase,"Pn");

					if (diagnostic){
						fprintf(logfp,"id_jb: del=%f ",pp->delta);
						fprintf(logfp,"z=%f g=%f ",sp->depth,g_time);
						fprintf(logfp,"b=%f n=%f\n",b_time,n_time);
					}
				}
			}
			/* In lower crust - could be Pb or Pn. */
			/* If reported P or Pg assume it's first arriving phase.	*/ 
			/* Times for impossible phases are set to -NULLVAL so that	*/
			/* when times are compared these will be the biggest.		*/
			else{
				if(strcmp(pp->phase,"P")==0 || strcmp(pp->phase,"Pg")==0){

					/* Pb */
					strcpy(pp->phase,"Pb");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					b_time = pp->ttime;

					/* Pn */
					strcpy(pp->phase,"Pn");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					n_time = pp->ttime;

					if (b_time == n_time == -NULLVAL)
						strcpy(pp->phase,"");
					else if (b_time < n_time)
						strcpy(pp->phase,"Pb");
					else
						strcpy(pp->phase,"Pn");

					if (diagnostic){
						fprintf(logfp,"id_jb: del=%f ",pp->delta);
						fprintf(logfp,"z=%f ",sp->depth);
						fprintf(logfp,"b=%f n=%f\n",b_time,n_time);
					}
				}
			}
		}		/* above moho, delta < CRUSTAL_P_DELTA */
	}		/* P phase */

	/* Check for SKS that's realy an S. */
	if (strcmp(pp->phase,"SKS") ==0)
		if (pp->delta < SKS_MIN_DELTA)
			strcpy(pp->phase,"S");

	if (strcmp(pp->phase,"S") ==0 || strcmp(pp->phase,"Sg")==0 ||
		strcmp(pp->phase,"Sb")==0 || strcmp(pp->phase,"Sn")==0 ){

		/* Devide into crustal and non-crustal */
		if (pp->delta > CRUSTAL_S_DELTA || sp->depth > MOHO){
			strcpy(pp->phase,"S");

			/* Check for wrongly identified SKS */
			if (pp->delta > S_MAX_DELTA)
				strcpy(pp->phase,"SKS");
		}
		else{
			/* In upper crust - could be Sg, Sb, Sn. */
			/* If reported as S then assume it's first arriving phase.	*/ 
			/* Times for impossible phases are set to -NULLVAL so that	*/
			/* when times are compared these will be the biggest.		*/
			if (sp->depth < CONRAD){
				if (strcmp(pp->phase,"S")==0){

					/* Sg */
					strcpy(pp->phase,"Sg");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime =- NULLVAL;
					}
					g_time = pp->ttime;

					/* Sb */
					strcpy(pp->phase,"Sb");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					b_time = pp->ttime;

					/* Sn */
					strcpy(pp->phase,"Sn");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					n_time = pp->ttime;

					if (g_time == b_time == n_time == -NULLVAL)
						strcpy(pp->phase,"");
					else if (g_time <= b_time && g_time < n_time)
						strcpy(pp->phase,"Sg");
					else if (b_time < g_time && b_time < n_time)
						strcpy(pp->phase,"Sb");
					else
						strcpy(pp->phase,"Sn");
					if (diagnostic){
						fprintf(logfp,"id_jb: del=%f ",pp->delta);
						fprintf(logfp,"z=%f g=%f ",sp->depth,g_time);
						fprintf(logfp,"b=%f n=%f\n",b_time,n_time);
					}
				}
			}
			/* In lower crust - could be Sb or Sn. */
			/* If reported S or Sg assume it's first arriving phase.	*/ 
			/* Times for impossible phases are set to -NULLVAL so that	*/
			/* when times are compared these will be the biggest.		*/
			else{
				if(strcmp(pp->phase,"S")==0 || strcmp(pp->phase,"Sg")==0){

					/* Sb */
					strcpy(pp->phase,"Sb");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					b_time = pp->ttime;

					/* Sn */
					strcpy(pp->phase,"Sn");
					if( read_ttime(sp, pp) ){
						sprintf(error_part,"id_jb: %d:",pp->phid);
						add_to_error(error_part);
						handle_error();
						pp->ttime = -NULLVAL;
					}
					n_time = pp->ttime;

					if (b_time == n_time == -NULLVAL)
						strcpy(pp->phase,"");
					else if (b_time < n_time)
						strcpy(pp->phase,"Sb");
					else
						strcpy(pp->phase,"Sn");

					if (diagnostic){
						fprintf(logfp,"id_jb: del=%f ",pp->delta);
						fprintf(logfp,"z=%f ",sp->depth);
						fprintf(logfp,"b=%f n=%f\n",b_time,n_time);
					}
				}
			}
		}		/* above moho delta < CRUSTAL_S_DELTA */
	}		/* s_phase true */

	return 0;
}


#define NUM_PHASE_MAP 40				 /* Must match structure below. */

struct phase_map_rec {
	char rep_phase[PHALEN];
	char phase[PHALEN];
};

struct phase_map_rec phase_map[NUM_PHASE_MAP] = {
	{"p" , "P" },
	{"P" , "P" },
	{"pP", "pP"},
	{"AP", "pP"},
	{"Pg", "Pg"},
	{"P*", "Pb"},
	{"Pb", "Pb"},
	{"Pn", "Pn"},
	{"PG", "Pg"},
	{"PB", "Pb"},
	{"PN", "Pn"},
	{"PKP","PKP"},
	{"P/PKP","P"},
	{"Pdiff","Pdiff"},
	{"PDIF","Pdiff"},
	{"s" , "S" },
	{"S" , "S" },
	{"Sg", "Sg"},
	{"S*", "Sb"},
	{"Sb", "Sb"},
	{"Sn", "Sn"},
	{"SG", "Sg"},
	{"SB", "Sb"},
	{"SN", "Sn"},
	{"SKS","SKS"},
	{"S/(SKS)","S"},
	{"PcP","PcP"},
	{"ScS","ScS"},
	{"PP" ,"PP"},
	{"PPP","PPP"},
	{"PS" ,"PS"},
	{"SP" ,"SP"},
	{"PPS","PPS"},
	{"SPP","SPP"},
	{"PSP","PSP"},
	{"SS" ,"SS"},
	{"SSP","SSP"},
	{"PSS","PSS"},
	{"SPS","SPS"},
	{"SSS","SSS"},
};

/*#DOC  Title:																*/
/*#DOC    id_pha															*/
/*#DOC  Desc:																*/
/*#DOC    Fills the 'phase' field of each phase structure with an ISC code.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to structure containing current solution.			*/
/*#DOC    p[] - array of phase structures.									*/
/*#DOC    reid_phase -	flag from event structure.							*/
/*#DOC    				Will be 1 unless set to 0 on instruction line.		*/
/*#DOC  Return:	0															*/
/*#DOC  Calls:																*/
/*#DOC    id_jb() from id_jb.c												*/
/*#DOC    handle_error() from utils.c												*/

/* Uses 'rep_phase' field in phase structure to set the 'phase' field.		*/
/* Ensures that the codes used conform to ISC standard in case etc.			*/
/* The phase_map structure is used to recognise and possibly change codes.	*/
/* Sets phase to null if rep_phase is not recognised.						*/
/* Assumes unidentified first phases are 'P'.								*/
/* Gives non-initial 'P' phases a null isc_code.							*/
/* Reidentifies phases if given code is impossible, given the current		*/
/* solution's depth and delta values.										*/

/* This function is run each iteration. It needs to be run after:			*/
/*		calc_delaz															*/

int id_pha (struct sol_rec *sp, struct pha_rec p[], int reid_phase)
{
	int i,j;

	/* This function would need changing if the model were changed. */
	int id_jb (struct sol_rec *sp, struct pha_rec p[]);

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	for (i=0; i<sp->numphas; i++){

		if (diagnostic)
			fprintf(logfp,"id_pha:%s rep_phase=%s\n",p[i].sta,p[i].rep_phase);

		/* Set ISC phase equal to null. */
		strcpy(p[i].phase,"");

		/* Don't give phases without a time an ISC code. */
		if (p[i].time == NULLVAL)
			continue;

		/* Assume unnamed initial phases are P. */
		if (strcmp(p[i].rep_phase,"")==0 && p[i].init)
			strcpy(p[i].phase,"P");

		/* Map alternate spellings/codes to ISC standard */
		for (j=0;j<NUM_PHASE_MAP;j++){
			if (strcmp(p[i].rep_phase,phase_map[j].rep_phase)==0){
				strcpy(p[i].phase,phase_map[j].phase);
				break;
			}
		}
		if (diagnostic)
			fprintf (logfp,"id_pha:%s mapped phase=%s\n",p[i].sta,p[i].phase);

		/* Check for non-initial P phases. */
		/* If an agency gives two Ps in the same reading it causes problems. */
		if (strcmp(p[i].phase,"P")==0 && !p[i].init)
			strcpy(p[i].phase,"");

		/* Unless option was chosen not to reconsider phase codes check */
		/* that reporters phase is possible for current depth/delta and	*/
		/* rename if necessary. Function used is model dependent. 		*/
		if (reid_phase)
			if (id_jb(sp,&p[i]))
				handle_error();
			
		if (diagnostic)
			fprintf (logfp,"id_pha:%s phase=%s\n",p[i].sta,p[i].phase);

	}		/* phase loop */
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    init_sol	(SEISAN version)										*/
/*#DOC  Desc:																*/
/*#DOC    Sets up solution structure with starting point for 1st iteration.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to solution structure to be initialised.			*/
/*#DOC    hp  - pointer to structure containing seed hypocentre.			*/
/*#DOC  Calls:																*/
/*#DOC    calc_dircos() from utils.c										*/
/*#DOC  Return:	0															*/

int init_sol(struct sol_rec *sp, struct hyp_rec *hp)
{
	extern double sigma_start;		/* From config file */
	extern char weighting1[];		/* From config file */
	extern double default_depth;	/* From config file */
	extern int purge_phase;			/* From config file */
	int i;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Mark as not converged yet. */
	sp->converged = 0;
	sp->diverging = 0;

	/* Set starting point for location to that of chosen hypocentre */
	sp->time = hp->time;
	sp->lat  = hp->lat;
	sp->lon  = hp->lon;

	for (i=1; i<=6; i++)
		sp->dircos[i] = hp->dircos[i];

	/* Depth either from hypocentre or from config file. */
	if (hp->depth != NULLVAL)
		sp->depth = hp->depth;
	else
		sp->depth = default_depth;

	/* Reset errors and sdobs. */
	/* Only for diagnostics - they get overwritten anyway. */
	for (i=0;i<5;i++)
		sp->error[i] = NULLVAL;
	sp->sdobs = NULLVAL;
	sp->mindist = sp->maxdist = NULLVAL;
	sp->majax = sp->minax = sp->theta = NULLVAL;

	/* Set starting point for weighting variables to those in config file. */
	strcpy(sp->weighting_type,weighting1);
	if (sp->weighting_type[0] == '\0'){
		sprintf(errstr,"init_sol: weighting1 not set");
		return 1;
	}
	if (sigma_start)
		sp->sigma = sigma_start;
	else{
		sprintf(errstr,"init_sol: sigma_start not set");
		return 1;
	}
	if (diagnostic){
		fprintf(logfp,"init_sol: weight=%s ",sp->weighting_type);
		fprintf(logfp,"sigma=%.3f\n",sp->sigma);
	}

	/* If phase purging has been disabled in config file then set	 	*/
	/* phases_purged flag as if purging has already been done.			*/
	if (purge_phase)
		sp->phases_purged = 0;
	else
		sp->phases_purged = 1;

	return 0;
}


/*#DOC  Title:																*/
/*#DOC    no_weight															*/
/*#DOC  Desc:																*/
/*#DOC    Fills 'alpha' in solution structure and 'weight' for each phase.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    p[]  - array of pointers to structures containing phases.			*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/


int no_weight (struct sol_rec *sp, struct pha_rec p[])
{
	extern int    min_phases;			/* From config file */
	extern double max_resid;			/* From config file */

	int i,ndef;
	double total_weight;

	sp->alpha=0;
	total_weight=0;
	ndef=0;
	for (i=0; i<sp->numphas; i++){
		if (p[i].weight_factor > 0 && fabs(p[i].resid) < max_resid){
			p[i].weight   = p[i].weight_factor;
			total_weight += p[i].weight;
			sp->alpha    += p[i].resid*p[i].weight;
			ndef++;
		}
		else{
			p[i].weight = 0;
		}
	}
	if (ndef < min_phases){
		fprintf(logfp,"TOO FEW PHASES no_weight: ndef=%d\n",ndef);
		strcpy(errstr,"");
		return 1;
	}
	sp->alpha /= total_weight;

	if (sp->iteration == 0)
		sp->prev_alpha = sp->alpha;

	return 0;
}


/*#DOC  Title:																*/
/*#DOC    purge_pha		(SEISAN version)									*/
/*#DOC  Desc:																*/
/*#DOC    Sets 'purged' field of a phase if |resid| > purge_resid.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    ep  - pointer to structure containing event information.			*/
/*#DOC    sp  - pointer to structure containing current solution.			*/
/*#DOC    p[] - array of phase structures.									*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Uses:																*/
/*#DOC    print_pha() from utils.c											*/

/* Checks if phases have been purged yet and if not then does so.			*/
/* Threshold used is in config.txt.											*/
/* If purging is switched off on instruction line then init_sol sets		*/
/* phases_purged=1 at the outset so that this function never runs.			*/
/* Resets the converged flag so that iterating will continue. 				*/

/* Purged flag in phase structure can have three values:					*/
/*   0 Still available.														*/
/*   1 Phase purged but other good phases in same reading.					*/
/*   2 Phase purged along with all other phases in this reading.			*/

int purge_pha (struct sol_rec *sp,  struct pha_rec p[])
{
	extern double purge_resid;		/* From config file */

	int prev_rdid;
	int reading[PHA_PER_READ];
	int keep,purged;
	int i,j,k;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* If phases have already been purged then don't do it again. 			*/
	/* This is also how purging is switched off - phases_purged starts as 1.*/
	if (sp->phases_purged)
		return 0;

	/* Purge large resids but not those set to NULLVAL. */
	/* If no other phases in this reading have small residuals then 	*/
	/* purge all the phases (to stop some being used for magnitudes.) 	*/
	prev_rdid = 0;
	j = 0;					/* Number of phases for a reading. 		*/
	keep = 1;				/* Default is don't purge whole reading.*/
	purged = 0;				/* Count of purges made.				*/
	for (i=0; i<sp->numphas; i++){

		/* Deal with previous reading in case been marked bad. */
		if (p[i].rdid != prev_rdid){
			if (!keep){
				/* Check through bad reading for good phases. */
				for (k=reading[0]; k<=reading[j-1]; k++)
					if (fabs(p[k].resid) <= purge_resid)
						keep = 1;

				/* Reading still bad. */
				if (!keep)
					for (k=reading[0]; k<=reading[j-1]; k++)
						p[k].purged = 2;

				keep = 1;
			}
			j=0;
			prev_rdid = p[i].rdid;
		}

		/* Check current phase. */
		if (fabs(p[i].resid) > purge_resid && p[i].resid != NULLVAL){
			p[i].purged = 1;
			keep = 0;
			purged++;
		}
		reading[j++] = i;

		/* Check not going past end of reading array. */
		if (j>PHA_PER_READ){
			sprintf(errstr,"purge_pha: %s: too many phases",p[i].sta);
			return 1;
		}
	}

	if (diagnostic){
		fprintf(logfp,"\nphases with |resid| > %f purged:\n",purge_resid);
		fprintf(logfp,"(weights not reset yet)\n");
		print_pha(sp->numphas, p);
	}

	/* Set flag in solution structure so that purging only happens once. */
	sp->phases_purged = 1;

	/* Reset the converged flag so that iterating will continue.	*/
	/* Only do this if at least one phase has been purged.			*/
	if (purged > 0)
		sp->converged = 0;

	return 0;
}


/*#DOC  Title:																*/
/*#DOC    read_config	(SEISAN version)									*/
/*#DOC  Desc:																*/
/*#DOC    Sets external variables to values read from configuration file.	*/
/*#DOC  Input Arguments:													*/
/*#DOC    filename  - string with path/name of configuration file.			*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Each line can have one configuration variable and its value, eg:			*/
/*	max_iter = 16															*/
/* Empty lines are ignored.													*/
/* Anything after # is considered a comment and not read.					*/

/* Diagnostics from this function get written to STDERR as logfile is not	*/
/* open yet (its name is read in here).										*/

/* These external variables correspond to recognised entries in the file.	*/ 
char weighting1[VALLEN];
char weighting2[VALLEN];
char ttime_table[VALLEN];
char out_agency[VALLEN];
int min_phases;
int reid_phase;
double init_max_resid;
double max_resid;
int purge_phase;
double purge_resid;
double sigma_start;
double mu;
int max_iter;
double default_depth;
double max_err_factor;
double avg_weight_thresh;
double alpha_thresh;
double dalpha_thresh;
double dsigma_low;
double dsigma_high;
int crust_corr_on;
int elev_corr_on;
int mantle_time_on;
int tele_time_on;
int repid;
char isf_stafile[VALLEN];
char isf_outfile[VALLEN];
char instructfile[VALLEN];
char logfile[VALLEN];
char errfile[VALLEN];
double update_db;
double body_mag_min_dist;
double body_mag_max_dist;
double surf_mag_min_dist;
double surf_mag_max_dist;
double body_mag_min_per;
double body_mag_max_per;
double surf_mag_min_per;
double surf_mag_max_per;
double mag_range_warn_thresh;
double depdp_range_warn_thresh;

int read_config(char *filename)
{
	FILE *fp;
	char line[LINLEN];
	char par[PARLEN];
	char value[VALLEN];

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Open configuration file or return an error. */
	if ((fp = fopen(filename,"r")) == NULL){
		sprintf(errstr,"read_config: can't open %s",filename);
		return 1;
	}

	/* Step through file a line at a time identifying parameters. */
	while ( fgets(line,LINLEN,fp) ){

		/* blank line */
		if ( sscanf(line,"%s = %s", par, value) < 2)
			continue;

		/* comment */
		if (strncmp(par,"#",1)==0)
			continue;

		else if (strcmp(par,"out_agency")==0){
			strncpy(out_agency,value,VALLEN);
			out_agency[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"weighting1")==0){
			strncpy(weighting1,value,VALLEN);
			weighting1[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"weighting2")==0){
			strncpy(weighting2,value,VALLEN);
			weighting2[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"ttime_table")==0){
			strncpy(ttime_table,value,VALLEN);
			ttime_table[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"min_phases")==0)
			min_phases = atoi(value);

		else if (strcmp(par,"reid_phase")==0)
			reid_phase = atoi(value);

		else if (strcmp(par,"init_max_resid")==0)
			init_max_resid = atof(value);

		else if (strcmp(par,"max_resid")==0)
			max_resid = atof(value);

		else if (strcmp(par,"purge_phase")==0)
			purge_phase = atoi(value);

		else if (strcmp(par,"purge_resid")==0)
			purge_resid = atof(value);

		else if (strcmp(par,"sigma_start")==0)
			sigma_start = atof(value);

		else if (strcmp(par,"mu")==0)
			mu = atof(value);

		else if (strcmp(par,"max_iter")==0)
			max_iter = atoi(value);

		else if (strcmp(par,"default_depth")==0)
			default_depth = atof(value);

		else if (strcmp(par,"max_err_factor")==0)
			max_err_factor = atof(value);

		else if (strcmp(par,"avg_weight_thresh")==0)
			avg_weight_thresh = atof(value);

		else if (strcmp(par,"alpha_thresh")==0)
			alpha_thresh = atof(value);

		else if (strcmp(par,"dalpha_thresh")==0)
			dalpha_thresh = atof(value);

		else if (strcmp(par,"dsigma_low")==0)
			dsigma_low = atof(value);

		else if (strcmp(par,"dsigma_high")==0)
			dsigma_high = atof(value);

		else if (strcmp(par,"crust_corr_on")==0)
			crust_corr_on = atoi(value);

		else if (strcmp(par,"elev_corr_on")==0)
			elev_corr_on = atoi(value);

		else if (strcmp(par,"mantle_time_on")==0)
			mantle_time_on = atoi(value);

		else if (strcmp(par,"tele_time_on")==0)
			tele_time_on = atoi(value);

		else if (strcmp(par,"repid")==0)
			repid = atoi(value);

		else if (strcmp(par,"isf_stafile")==0){
			strncpy(isf_stafile,value,VALLEN);
			isf_stafile[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"isf_outfile")==0){
			strncpy(isf_outfile,value,VALLEN);
			isf_outfile[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"instructfile")==0){
			strncpy(instructfile,value,VALLEN);
			instructfile[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"logfile")==0){
			strncpy(logfile,value,VALLEN);
			logfile[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"errfile")==0){
			strncpy(errfile,value,VALLEN);
			errfile[VALLEN-1] = '\0';
		}
		else if (strcmp(par,"update_db")==0)
			update_db = atoi(value);

		else if (strcmp(par,"body_mag_min_dist")==0)
			body_mag_min_dist = atof(value);

		else if (strcmp(par,"body_mag_max_dist")==0)
			body_mag_max_dist = atof(value);

		else if (strcmp(par,"surf_mag_min_dist")==0)
			surf_mag_min_dist = atof(value);

		else if (strcmp(par,"surf_mag_max_dist")==0)
			surf_mag_max_dist = atof(value);

		else if (strcmp(par,"body_mag_min_per")==0)
			body_mag_min_per = atof(value);

		else if (strcmp(par,"body_mag_max_per")==0)
			body_mag_max_per = atof(value);

		else if (strcmp(par,"surf_mag_min_per")==0)
			surf_mag_min_per = atof(value);

		else if (strcmp(par,"surf_mag_max_per")==0)
			surf_mag_max_per = atof(value);

		else if (strcmp(par,"mag_range_warn_thresh")==0)
			mag_range_warn_thresh = atof(value);

		else if (strcmp(par,"depdp_range_warn_thresh")==0)
			depdp_range_warn_thresh = atof(value);

		else{
			sprintf(errstr,"read_config: unrecognised parameter %s",par);
			return 1;
		}

		/* logfile not open yet. */
		if (diagnostic)
			fprintf(stderr,"%-*s = %-*s\n",PARLEN,par,VALLEN,value);
	}
	fclose(fp);
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    read_jb.c															*/
/*#DOC  Desc:																*/
/*#DOC   File with functions that need to read the JB tables.				*/
/*#DOC   The two functions called from outside this file are:				*/
/*#DOC    read_jb															*/
/*#DOC    jb_pP_P															*/


/*#DOC  Title:																*/
/*#DOC    read_jb															*/
/*#DOC  Desc:																*/
/*#DOC    Fills 'ttime, 'dtdh', and 'dtdd' fields for a phase.				*/
/*#DOC  Input Arguments:													*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    pp    - pointer to structure containing a phase.					*/
/*#DOC  Return:																*/
/*#DOC    0/1/2 for success/failure/warn.									*/
/*#DOC  Calls:																*/
/*#DOC    Functions from this file for reading tables.						*/

/* For P/PKP or S/SKS 3 possible cases: 								*/
/* If edep < 0.016 earth radii and delta < 1.3 							*/
/*		then use 'Short Epicentral Distances' table.					*/
/* If edep < 0 earth radii (delta > 8 or would be assuming Pg, Pn etc.) */
/* 		then use first column of standard tables but interpolate and 	*/
/* 		calculate derivatives using velocities.							*/
/* Otherwise															*/
/*		use standard tables												*/

/* Save dtdh in s/km rather than s/e.r.*100.							*/

int read_jb (double depth, struct pha_rec *pp)
{
	int status;
	double ttime;
	double dtdd;
	double dtdh;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Call subroutine relevant for phase code. */
	status = 0;

	/* Pg */
	if (strcmp(pp->phase,"Pg") == 0)
		status = ps_g('P',pp->delta,depth,&ttime,&dtdh,&dtdd);
	/* Pb */
	else if (strcmp(pp->phase,"Pb") == 0)
		status = ps_star('P',pp->delta,depth,&ttime,&dtdh,&dtdd);
	/* Pn */
	else if (strcmp(pp->phase,"Pn") == 0)
		status = ps_crust('P',pp->delta,depth,&ttime,&dtdh,&dtdd);
	/* Sg */
	else if (strcmp(pp->phase,"Sg") == 0)
		status = ps_g('S',pp->delta,depth,&ttime,&dtdh,&dtdd);
	/* Sb */
	else if (strcmp(pp->phase,"Sb") == 0)
		status = ps_star('S',pp->delta,depth,&ttime,&dtdh,&dtdd);
	/* Sn */
	else if (strcmp(pp->phase,"Sn") == 0)
		status = ps_crust('S',pp->delta,depth,&ttime,&dtdh,&dtdd);

	/* P - subroutine chosen according to delta and source depth. */
	else if (strcmp(pp->phase,"P") == 0){

		if (diagnostic)
			fprintf(logfp,"read_jb: P delta=%f depth=%f\n",pp->delta,depth);

		/* Station very near to source, depth between MOHO and SHORT_DEPTH. */
		if (pp->delta < SHORT_DELTA && depth < SHORT_DEPTH)
			status = ps_short(&p_short,pp->delta,depth,&ttime,&dtdh,&dtdd);

		/* Shallow source with station beyond crustal phase distances. */
		else if (depth < MOHO)
			status = ps_crust('P',pp->delta,depth,&ttime,&dtdh,&dtdd);

		/* P default. */
		else
			status = ps(&p_tab,pp->delta,depth,&ttime,&dtdh,&dtdd);
	}
	/* PKP - same subroutine as default P but different table. */
	else if (strcmp(pp->phase,"PKP") == 0)
		status = ps(&pkp_tab,pp->delta,depth,&ttime,&dtdh,&dtdd);

	/* Pdiff - no derivatives returned, no use in solution. */
	else if (strcmp(pp->phase,"Pdiff") == 0){
		status = pdiff(&p_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}

	/* S - subroutine chosen according to delta and source depth. */
	else if (strcmp(pp->phase,"S") == 0){

		if (diagnostic)
			fprintf(logfp,"read_jb: S delta=%f depth=%f\n",pp->delta,depth);

		/* Station very near to source, depth between MOHO and SHORT_DEPTH. */
		if (pp->delta < SHORT_DELTA && depth < SHORT_DEPTH)
			status = ps_short(&s_short,pp->delta,depth,&ttime,&dtdh,&dtdd);

		/* Shallow source with station beyond crustal phase distances. */
		else if (depth < MOHO)
			status = ps_crust('S',pp->delta,depth,&ttime,&dtdh,&dtdd);

		/* S default. */
		else
			status = ps(&s_tab,pp->delta,depth,&ttime,&dtdh,&dtdd);
	}
	/* SKS - same subroutine as default S but different table. */
	else if (strcmp(pp->phase,"SKS") == 0)
		status = ps(&sks_tab,pp->delta,depth,&ttime,&dtdh,&dtdd);

	/* From here phases are dealt with in other_phase,	*/
	/* which does not return derivatives.				*/
	/* PcP */
	else if (strcmp(pp->phase,"PcP") == 0){
		status = other_phase(&pcp_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* ScS */
	else if (strcmp(pp->phase,"ScS") == 0){
		status = other_phase(&scs_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* PP */
	else if (strcmp(pp->phase,"PP") == 0){
		status = other_phase(&pp_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* PPP */
	else if (strcmp(pp->phase,"PPP") == 0){
		status = other_phase(&ppp_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* PS */
	else if (strcmp(pp->phase,"PS") == 0){
		status = other_phase(&ps_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* SP */
	else if (strcmp(pp->phase,"SP") == 0){
		status = other_phase(&sp_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* SPP */
	else if (strcmp(pp->phase,"SPP") == 0){
		status = other_phase(&spp_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* PSP / PPS*/
	else if (strcmp(pp->phase,"PSP") == 0 || strcmp(pp->phase,"PPS") == 0){
		status = other_phase(&pps_psp_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* SS */
	else if (strcmp(pp->phase,"SS") == 0){
		status = other_phase(&ss_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* PSS */
	else if (strcmp(pp->phase,"PSS") == 0){
		status = other_phase(&pss_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* SSP / SPS*/
	else if (strcmp(pp->phase,"SSP") == 0 || strcmp(pp->phase,"SPS") == 0){
		status = other_phase(&ssp_sps_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* SSS */
	else if (strcmp(pp->phase,"SSS") == 0){
		status = other_phase(&sss_surf_tab,pp->delta,depth,&ttime);
		dtdh = NULLVAL;
		dtdd = NULLVAL;
	}
	/* Unknown phase - should have been taken care of in id_pha. */
	else {
		sprintf(errstr,"%d: bad code: %s",pp->phid,pp->phase);
		return 1;
	}

	/* Record and return actual return status from function */
	/* as could be a warning or a serious error.			*/					
	if (status)
		return status;

	pp->ttime = ttime;
	pp->dtdh  = dtdh;
	pp->dtdd  = dtdd;

	return 0;
}

/*#DOC  Title:																*/
/*#DOC    ps																*/
/*#DOC  Desc:																*/
/*#DOC    Looks up given JB table and supplies ttime, dtdd, and dtdh.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    tp    - pointer to structure with travel-time table.				*/
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    ttime - pointer to variable for travel time.						*/
/*#DOC    dtdh  - pointer to variable for dt/dh.							*/
/*#DOC    dtdd  - pointer to variable for dt/dd.							*/
/*#DOC  Return:																*/
/*#DOC    0/1/2 for success/failure/warn.									*/
/*#DOC  Calls:																*/
/*#DOC    fill_matrix() from this file.										*/
/*#DOC    interpol() and int_diff() from this file.							*/

/* Read the standard Jeffrey's and Bullen tables for P, PKP, S, and SKS.	*/
/* P and PKP are allowed below 105 and above 106 degrees respectively.		*/
/* (although in practice use Pdiff above 102)								*/
/* S and SKS overlap between 62 and 107 degrees.							*/

int ps( struct tab_rec *tp, double delta, double depth,
				double *ttime, double *dtdh, double *dtdd)
{                                              
	double depth_frac,delta_frac;
	double depth_erad;
	int matrix[3][3];
	double t[3],d[3];
	int col1,row1,depth_dir,delta_dir;
	int numval;
	int i;

	int fill_matrix (struct tab_rec*, int col1, int row1,
				 	 int depth_dir, int delta_dir, int matrix[3][3]);

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Get column by using depth in earth radii * 100 */
	/* One out of step because 1st column is surface, 2nd column is 0.00 */
	depth_erad = (depth - MOHO)/MOHO_RADIUS;
	if (depth_erad < tp->min_depth || depth_erad > tp->max_depth){
		sprintf(errstr,"ps: depth out of range: %f",depth);
		return 1;
	}
	col1  = depth_erad*100 + 1; 			
	depth_frac = depth_erad*100 - (int)(depth_erad*100);

	/* Get row by using delta */
	if (delta < tp->min_delta || delta > tp->max_delta){
		sprintf(errstr,"ps: delta out of range: %f",delta);
		return 1;
	}
	row1 = delta - tp->min_delta;
	delta_frac = delta - (int)delta;

	if (diagnostic){
		fprintf(logfp,"ps: depth=%f col1=%d ",depth_erad,col1);
		fprintf(logfp,"depth_frac=%f\n",depth_frac);
		fprintf(logfp,"ps: delta=%f row1=%d ",delta,row1);
		fprintf(logfp,"delta_frac=%f\n",delta_frac);
	}

	/* Interpolate forwards in first half of table and backwards in second */
	/* half of table to avoid the ends. */
	if (delta > tp->switch_dir_delta){
		delta_dir = -1;
		row1++;
		delta_frac = 1 - delta_frac;
	}
	else
		delta_dir = 1;

	/* Fill a matrix with 9 values from the table */
	depth_dir = 1;
	numval = fill_matrix(tp,col1,row1,depth_dir,delta_dir,matrix);

	if (diagnostic){
		fprintf(logfp,"ps: %d %d %d\n",matrix[0][0],matrix[1][0],matrix[2][0]);
		fprintf(logfp,"    %d %d %d\n",matrix[0][1],matrix[1][1],matrix[2][1]);
		fprintf(logfp,"    %d %d %d\n",matrix[0][2],matrix[1][2],matrix[2][2]);
	}

	/* If at bottom of table fill in the missing value(s) */
	if ( numval==8 && matrix[2][0]==0){

		/* If at left hand end of table estimate value for matrix[2][0]. */
		if (col1 == 1){
			matrix[2][0]  = 2*(matrix[1][0] + matrix[2][1] + matrix[1][1]);
			matrix[2][0] -=  matrix[0][0] + matrix [2][2] + matrix[0][2];
			matrix[2][0] /= 3;
			numval = 9;
		}
		/* Otherwise invert depth order and try again */
		else {
			depth_dir = -1;
			col1 += 2;
			depth_frac = 1 - depth_frac;
			numval = fill_matrix(tp,col1,row1,depth_dir,delta_dir,matrix);
		}
	}
	else if (numval==7 && matrix[1][0]==0 && matrix[2][0]==0){

		if ( delta_dir== 1 && (depth_frac+delta_frac) < 1 ||
			 delta_dir==-1 && depth_frac < delta_frac ){

			matrix[1][0] = 2*matrix[1][1] - matrix[1][2];
			matrix[2][0] = 2*matrix[2][1] - matrix[2][2];
			numval = 9;
		}
	}

	/* Use the matrix to interpolate the required time. */
	if ( numval==9 ){
		/* Derive travel time for each delta by interpolating over depth */
		for (i=0; i<3; i++){
			t[i] = interpol(depth_frac,matrix[0][i],matrix[1][i],matrix[2][i]);
			d[i] = int_diff(depth_frac,matrix[0][i],matrix[1][i],matrix[2][i]);
		}
		if (diagnostic){
			fprintf(logfp,"ps: t=%f %f %f\n",t[0],t[1],t[2]);
			fprintf(logfp,"ps: d=%f %f %f\n",d[0],d[1],d[2]);
		}

		/* Derive required travel time by interpolating over delta */
		*ttime = 0.1 * interpol(delta_frac,t[0],t[1],t[2]);
		*dtdd = delta_dir * 0.1 * int_diff(delta_frac,t[0],t[1],t[2]);
		*dtdh = depth_dir* 10 * interpol(delta_frac,d[0],d[1],d[2])/MOHO_RADIUS;
	}
	else {
		sprintf(errstr,"del=%f,dep=%f,numval=%d",delta,depth_erad,numval);
		return 2;
	}

	if (diagnostic)
		fprintf(logfp,"ps: ttime=%f dtdd=%f dtdh%f\n",*ttime,*dtdd,*dtdh);

	return 0;
}

/*#DOC  Title:																*/
/*#DOC    fill_matrix ( only called by ps)									*/
/*#DOC  Desc:																*/
/*#DOC    Fill a 3x3 matrix with values from the given table.				*/
/*#DOC  Input Arguments:													*/
/*#DOC    tp        - pointer to structure with travel-time table.			*/
/*#DOC    col1      - first column reuired.									*/
/*#DOC    row1      - first row required.									*/
/*#DOC    depth_dir - whether to go up or down the table.					*/
/*#DOC    delta_dir - whether to go left or right across the table.			*/
/*#DOC    matrix    - 2D array for numbers from table.						*/
/*#DOC  Return:																*/
/*#DOC    number of values found (9 for success).							*/

int fill_matrix (struct tab_rec *tp, int col1, int row1,
				 int depth_dir, int delta_dir, int matrix[3][3])
{
	int row,col,numval;
	int i,j;

	numval = 0;
	for (i=0; i<3; i++){
		row = row1 + i*delta_dir;

		for (j=0; j<3; j++){
			col = col1 + j*depth_dir;

			/* If off the end column use last columns to extrapolate depth */
			if (col>13)
				matrix[j][i] = col*(tp->tab[13][row]) - tp->tab[12][row];
			else
				matrix[j][i] = tp->tab[col][row];

			if (matrix[j][i] != 0)
				numval++;
		}
	}
	return numval;
}

/*#DOC  Title:																*/
/*#DOC    ps_crust															*/
/*#DOC  Desc:																*/
/*#DOC    Calculates P/S ttime, dtdd, and dtdh when source above MOHO.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    ps    - either 'P' or 'S'.										*/
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    ttime - pointer to variable for travel time.						*/
/*#DOC    dtdh  - pointer to variable for dt/dh.							*/
/*#DOC    dtdd  - pointer to variable for dt/dd.							*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC    interpol() and int_diff() from this file.							*/

/* Subroutine to calculate travel times when depth < MOHO.					*/
/* Delta will be > 8 or would have been identified as a crustal phase.		*/
/* (assuming using complementary id_pha.)									*/
/* Taken straight from reviser:												*/
/*   ! For method see														*/
/*   !       Arnold,E.P.													*/
/*   !              1965   Revision of the seismological tables				*/
/*   !       PH.D. Thesis Cantab.											*/
/*   !       see  also														*/
/*   !       Arnold,E.P.													*/
/*   !           1967   P and S times in Japan								*/
/*   !       Pubs. of B.C.I.S., Series A   Vol.24, pp.103-135				*/
/*   !       see equation 10 on page 114									*/

int ps_crust(char ps, double delta, double depth,
									double *ttime, double *dtdh, double *dtdd)
{
	double g_slow,b_slow,hfn,ray;
	int times[3];
	int row1,row;
	int delta_dir;
	double delta_frac;
	struct tab_rec* tp;
	int i;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	if (ps == 'P'){
		g_slow = PGSLOW;
		b_slow = PBSLOW;
		tp = &p_tab;
	}
	else if (ps == 'S'){
		g_slow = SGSLOW;
		b_slow = SBSLOW;
		tp = &s_tab;
	}
	else {
		sprintf(errstr,"ps_crust: wrong ps value %c",ps);
		return 1;
	}		

	/* Check that depth is sensible */
	if (depth < 0 || depth > MOHO ){
		sprintf(errstr,"ps_crust: depth out of range: %f",depth);
		return 1;
	}

	/* Get row by using delta */
	if (delta < tp->min_delta || delta > tp->max_delta){
		sprintf(errstr,"ps_crust: delta out of range: %f",delta);
		return 1;
	}
	row1 = delta - tp->min_delta;
	delta_frac = delta - (int)delta;

	/* Interpolate forwards in first half of table and backwards in second */
	/* half of table to avoid the ends. */
	if (delta > tp->switch_dir_delta){
		delta_dir = -1;
		row1++;
		delta_frac = 1 - delta_frac;
	}
	else
		delta_dir = 1;

	if (diagnostic){
		fprintf(logfp,"ps_crust: delta=%f row1=%d ",delta,row1);
		fprintf(logfp,"delta_frac=%f delta_dir=%d\n",delta_frac,delta_dir);
	}

	/* Store the 3 values surrounding the delta of interest. */
	for (i=0; i<3; i++){
		row = row1 + i*delta_dir;

		if (tp->tab[0][row] == 0){
			sprintf(errstr,"no value: del=%d, dep=%f",row1,depth);
			return 1;
		}			
		times[i]=tp->tab[0][row];
	}
	if (diagnostic){
		fprintf(logfp,"ps_crust: t[0]=%d ",times[0]);
		fprintf(logfp,"t[1]=%d t[2]=%d\n",times[1],times[2]);
	}

	/* Calculate time from surface focus. */
	*ttime = 0.1 * interpol(delta_frac,times[0],times[1],times[2]);
	*dtdd  = 0.1 * int_diff(delta_frac,times[0],times[1],times[2])*delta_dir;

	if (diagnostic)
		fprintf(logfp,"ps_crust: surface ttime=%f dtdd=%f\n",*ttime,*dtdd);
	

	/* Correct for actual depth. */
	hfn = (EARTH_RADIUS - depth) * TORAD;
	ray = sqrt((g_slow - *dtdd) * (g_slow + *dtdd));

	if (depth <= CONRAD)
		*ttime -= (ray * depth) / hfn;
	else{
		*ttime -= (ray * CONRAD) / hfn;
		ray = sqrt((b_slow - *dtdd) * (b_slow + *dtdd));
		*ttime -= (ray * (depth - CONRAD)) / hfn;
	}
	*dtdh = -0.57 * ray * 100/MOHO_RADIUS;

	if (diagnostic)
		fprintf(logfp,"ps_crust: ttime=%f dtdh=%f\n",*ttime,*dtdh);

	return 0;
}

/*#DOC  Title:																*/
/*#DOC    ps_short															*/
/*#DOC  Desc:																*/
/*#DOC    Looks up P/S ttime, dtdd, and dtdh when delta < 1.3.				*/
/*#DOC  Input Arguments:													*/
/*#DOC    tp    - pointer to structure with travel-time table.				*/
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    ttime - pointer to variable for travel time.						*/
/*#DOC    dtdh  - pointer to variable for dt/dh.							*/
/*#DOC    dtdd  - pointer to variable for dt/dd.							*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC    interpol() and int_diff() from this file.							*/

/* Subroutine to calculate travel times for P and S when delta < 1.3.		*/
/* Depth must be < 0.016 earth radii and will be > MOHO or would have been 	*/
/* identified as a crustal phase (assuming using complementary id_pha.)		*/
/* Calculate time and derivatives using 'Short Epicentral Distances' table.	*/

int ps_short( struct tab_rec* tp, double delta, double depth,
					double* ttime, double* dtdh, double* dtdd)
{
	double depth_frac,delta_frac;
	double depth_erad;
	int times[3];
	int col1,row1,col,row;
	double t[3],d[3];
	int i,j;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Get column by using depth in earth radii * 500 */
	depth_erad = (depth - MOHO)/(MOHO_RADIUS);
	if (depth_erad < tp->min_depth || depth_erad > tp->max_depth){
		sprintf(errstr,"ps_short:depth out of range: %f",depth);
		return 1;
	}
	col1  = depth_erad*500; 			
	depth_frac = depth_erad*500 - col1;

	/* Get row by using delta * 10 */
	if (delta < tp->min_delta || delta > tp->max_delta){
		sprintf(errstr,"ps_short:delta out of range: %f",delta);
		return 1;
	}
	row1 = delta*10;
	delta_frac = delta*10 - row1;

	if (diagnostic){
		fprintf(logfp,"ps_short: depth=%f col1=%d ",depth_erad,col1);
		fprintf(logfp,"depth_frac=%f\n",depth_frac);
		fprintf(logfp,"ps_short: delta=%f row1=%d ",delta,row1);
		fprintf(logfp,"delta_frac=%f\n",delta_frac);
	}

	for (i=0; i<3; i++){
		row = row1 + i;
		for (j=0; j<3; j++){
			col = col1 + j;
			times[j]=tp->tab[col][row];
		}
		t[i] = interpol(depth_frac,times[0],times[1],times[2]);
		d[i] = int_diff(depth_frac,times[0],times[1],times[2]);
	}
	*ttime = 0.1 * interpol(delta_frac,t[0],t[1],t[2]);
	*dtdd  = int_diff(delta_frac,t[0],t[1],t[2]);
	*dtdh  = 50 * interpol(delta_frac,d[0],d[1],d[2]) / MOHO_RADIUS;

	if (diagnostic)
		fprintf(logfp,"ps_short: tt=%f dtdd=%f dtdh=%f\n",*ttime,*dtdd,*dtdh);

	return 0;
}

/*#DOC  Title:																*/
/*#DOC    ps_g																*/
/*#DOC  Desc:																*/
/*#DOC    Calculates Pg/Sg ttime, dtdd, and dtdh.							*/
/*#DOC  Input Arguments:													*/
/*#DOC    ps    - either 'P' or 'S'.										*/
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    ttime - pointer to variable for travel time.						*/
/*#DOC    dtdh  - pointer to variable for dt/dh.							*/
/*#DOC    dtdd  - pointer to variable for dt/dd.							*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Subroutine to calculate travel times for Pg and Sg.						*/
/* Calculate travel time directly using velocities in header file.			*/

int ps_g(char ps, double delta, double depth,
					double *ttime, double *dtdh, double *dtdd)
{
	double g_vel,g_slow;
	double distance;
	double g_ray;

	/* Need velocity in km/s so calculate them from slowness.				*/
	if (ps == 'P'){
		g_slow = PGSLOW;
		g_vel  = PGVEL;
	}
	else if (ps == 'S'){
		g_slow = SGSLOW;
		g_vel  = SGVEL;
	}
	else {
		sprintf(errstr,"ps_g: wrong ps value %c",ps);
		return 1;
	}
	distance = delta * KM_PER_DEG;

	/* Check that depth is sensible */
	if (depth < 0 || depth > CONRAD ){
		sprintf(errstr,"ps_g: depth out of range: %f",depth);
		return 1;
	}

	/* Calculate travel time. */
	g_ray = sqrt(distance*distance + depth*depth);
	*ttime = g_ray/g_vel;

	/* Calculate derivatives dtdh in km. */
	if (*ttime == 0){
		*dtdd = g_slow;
		*dtdh = 0;
	}
	else{
		*dtdd = g_slow*g_slow*delta/(*ttime);
		*dtdh = depth/(*ttime*g_vel*g_vel);
	}
	return 0;
}

/*#DOC  Title:																*/
/*#DOC    ps_star															*/
/*#DOC  Desc:																*/
/*#DOC    Calculates P* / S* ttime, dtdd, and dtdh.							*/
/*#DOC  Input Arguments:													*/
/*#DOC    ps    - either 'P' or 'S'.										*/
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    ttime - pointer to variable for travel time.						*/
/*#DOC    dtdh  - pointer to variable for dt/dh.							*/
/*#DOC    dtdd  - pointer to variable for dt/dd.							*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Subroutine to calculate travel times for P* and S*.						*/
/* When source above Conrad calculate travel time directly. 				*/
/* When source below Conrad use converted star subroutine from reviser to	*/
/* iterate for the point where the ray crosses the Conrad and is refracted.	*/
/* Following is Fortran comment from star.f:

c subroutine star for finding travel-times of the direct wave
c with the focus in the middle layer. Uses Newton's method of
c approximation to solve the distance to the refraction point:
c
c      x(n+1)=x(n)-f(x(n))/f'(x(n))
c where f'(x(n))=(f(x(n)+D)-f(x(n)-d))/(2*d)
c   (with acknowlegment to DSIR, New Zealand for a copy of their
c     pstar program)
*/

int ps_star(char ps, double delta, double depth,
									double *ttime, double *dtdh, double *dtdd)
{
	double g_vel,b_vel;
	double distance,above_conrad,below_conrad;
	double conrad_radius,source_radius;
	double frac1,frac2;
	double g_ray,b_ray;
	double prev_crossing,prev_time,incr;
	double crossing,sin_inc,sin_ref,critical,new_dist;
	double cos_inc;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Need velocities in km/s so calculate them from slownesses.			*/
	if (ps == 'P'){
		g_vel  = PGVEL;
		b_vel  = PBVEL;
	}
	else if (ps == 'S'){
		g_vel  = SGVEL;
		b_vel  = SBVEL;
	}
	else {
		sprintf(errstr,"ps_star: wrong ps value %c",ps);
		return 1;
	}
	distance = delta * KM_PER_DEG;

	/* Check that depth is sensible */
	if (depth < 0 || depth > MOHO ){
		sprintf(errstr,"ps_star: depth out of range: %f",depth);
		return 1;
	}

	if (diagnostic)
		fprintf(logfp,"ps_star: distance=%f,depth=%f\n",distance,depth);

	/* Case when source above Conrad. 	*/
	if (depth < CONRAD ){
		above_conrad = CONRAD - depth;
		critical = asin(g_vel/b_vel);
		*ttime  = (above_conrad+CONRAD)/(g_vel*cos(critical));
		*ttime += (distance - (above_conrad+CONRAD)*tan(critical))/b_vel;

		if (diagnostic)
			fprintf(logfp,"ps_star: depth=%f,tt=%f\n",depth,*ttime);

		/* dt/dh =  (MOHO_RADIUS/vel*vel) * (h/t)		*/
		/* h/t = r.cos(critical)/t = vel.cos(critical)  */
		*dtdh = -cos(critical)/g_vel;
		*dtdd = KM_PER_DEG/b_vel;

		return 0;
	}

	/* Case where source below Conrad.	*/
	below_conrad  = depth - CONRAD;

	/* Special case of station over source. */
	if (distance < 0.01){
		*ttime = below_conrad/b_vel;
		*dtdh  = b_vel;
		*dtdd  = 0;
		return 0;
	}
	conrad_radius = EARTH_RADIUS - CONRAD;
	source_radius = EARTH_RADIUS - depth;

	frac1 = conrad_radius/EARTH_RADIUS;
	frac2 = source_radius/EARTH_RADIUS;

	/* Search for Conrad crossing point that gives right distance. */
	prev_crossing = 0;
	prev_time = 1000000;
	incr = distance/2;
	for(;;){
		crossing = prev_crossing + incr;
		b_ray = sqrt(below_conrad*below_conrad + frac1*frac2*crossing*crossing);
		*ttime   = b_ray/b_vel;
		sin_inc  = frac2*crossing/b_ray;
		sin_ref  = sin_inc*g_vel/b_vel;
		critical = sqrt(1-sin_ref*sin_ref);
		g_ray    = CONRAD/critical;
		*ttime += g_ray/g_vel;

		/* Check to see if close enough yet. */
		new_dist = crossing + g_ray*sin_ref;
		if (fabs(new_dist-distance) < 0.1)
			break;
		if (fabs(new_dist-distance) < 0.5 && fabs(*ttime-prev_time) < 0.01)
			break;

		/* Check to see if gone too far. */
		if (new_dist > distance){
			incr /= 2;
			if (incr < 0.01)
				break;
			continue;
		}
		prev_time = *ttime;
		prev_crossing = crossing;
	}

	/* Calculate derivatives dtdh in km. */
	*dtdd = (sin_inc/b_vel)*KM_PER_DEG;
	cos_inc = 1 - sin_inc*sin_inc;
	if (cos_inc > 0)
		*dtdh = (sqrt(cos_inc)/b_vel);
	else
		*dtdh = 0;

	return 0;
}

/*#DOC  Title:																*/
/*#DOC    pdiff																*/
/*#DOC  Desc:																*/
/*#DOC    Calculates travel time for diffracted P.							*/
/*#DOC  Input Arguments:													*/
/*#DOC    tp   - pointer to structure with travel-time table.				*/
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    ttime - pointer to variable for travel time.						*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Calculate diffracted P travel time using the final row of the P			*/
/* table as a starting point and adding 4.4 seconds per degree over that.	*/
/* Taken straight from reviser.												*/
/* Limited use because does not calculate the derivatives.					*/

int pdiff( struct tab_rec* tp, double delta, double depth, double* ttime)
{
	double depth_frac,depth_erad,dtdh;
	int col;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Only get Pdiff once P has stopped */
	if (delta < PDIFF_MIN_DELTA || delta > 180){
		sprintf(errstr,"pdiff: delta out of range: %f",delta);
		return 1;
	}

	/* Get column by using depth in earth radii * 100 */
	/* One out of step because 1st column is surface, 2nd column is 0.00 */
	depth_erad = (depth - MOHO)/MOHO_RADIUS;
	if (depth_erad < SURFACE || depth_erad > tp->max_depth){
		sprintf(errstr,"pdiff:depth out of range: %f",depth);
		return 1;
	}
	col  = depth_erad*100+1;

	if (col>0)
		depth_frac = depth_erad*100 - (int)(depth_erad*100);
	else if (col==0)
		depth_frac = 1 + depth_erad*MOHO_RADIUS/MOHO;
	else{
		col=0;
		depth_frac = 0;
	}
	if (col==13)
		col=12;

	dtdh= 0.1*(tp->tab[col+1][PDIFF_MIN_DELTA] - tp->tab[col][PDIFF_MIN_DELTA]);
	*ttime = 0.1 * tp->tab[col][PDIFF_MIN_DELTA] + depth_frac*dtdh;
	*ttime = *ttime + PDIFF_SEC_PER_DEG * (delta-PDIFF_MIN_DELTA);

	if (diagnostic){
		fprintf(logfp,"pdiff: delta=%f depth=%f col=%d ",delta,depth,col);
		fprintf(logfp,"depth_frac=%f ttime=%f\n",depth_frac,*ttime);
	}
	return 0;
}

/*#DOC  Title:																*/
/*#DOC    other_phase														*/
/*#DOC  Desc:																*/
/*#DOC    Looks up travel times in JB tables.								*/
/*#DOC  Input Arguments:													*/
/*#DOC    tp   - pointer to structure with surface focus travel-time table. */
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    ttime - pointer to variable for travel time.						*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* First looks up surface focus table then corrects for depth using depth	*/
/* allowance table.															*/
/* Does not return any derivatives.				 							*/

int other_phase(struct surf_tab_rec* tp,double delta,double depth,double* ttime)
{
	double depth_frac,delta_frac;
	double depth_erad;
	double surf_time,depth_corr;
	double c[4];
	int col,row;
	struct dep_allow_tab_rec* tp_allow;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* First get surface travel time from surface table. */

	/* Get row by using delta */
	if (delta < tp->min_delta || delta > tp->max_delta){
		sprintf(errstr,"other_phase: delta out of range: %f",delta);
		return 1;
	}
	row = delta - tp->min_delta;
	delta_frac = delta - (int)delta;

	surf_time  = 0.1 * tp->tab[row];
	surf_time += 0.1 * (tp->tab[row+1]-tp->tab[row]) * delta_frac;

	if (diagnostic){
		fprintf(logfp,"other_phase: delta=%f delta_frac=%f ",delta,delta_frac);
		fprintf(logfp,"surf_time=%f\n",surf_time);
	}

	/* Then get depth correction. */
	tp_allow = tp->dep_allow_tp;

	/* Rows are unevenly spaced - delta of each row given in row_label array. */
	for (row=0; row < tp_allow->numrows; row++)
		if (delta < tp_allow->row_label[row+1])
			break;

	delta_frac = (delta - tp_allow->row_label[row]) / 
	             (tp_allow->row_label[row+1] - tp_allow->row_label[row]);

	if (diagnostic)
		fprintf(logfp,"other_phase: row=%d delta_frac=%f\n",row,delta_frac);

	/* Get column by using depth in earth radii * 100 */
	/* Reject if exactly at end - won't be able to extrapolate. */
	depth_erad = (depth - MOHO)/MOHO_RADIUS;
	if (depth_erad < SURFACE || depth_erad >= tp_allow->max_depth){
		sprintf(errstr,"other_phase: depth out of range: %f",depth);
		return 1;
	}

	/* Extrapolate for depths above Moho.*/
	if (depth_erad <= 0){
		col = -1;
		depth_frac = 1 - depth_erad/SURFACE;
		c[0] = 0;
		c[1] = 0;
	}
	/* Otherwise interpolate. */
	else{
		col  = depth_erad*100; 			
		depth_frac = depth_erad*100 - col;
		c[0] = 0.1 * tp_allow->tab[row][col];
		c[1] = 0.1 * tp_allow->tab[row+1][col];
	}
	c[2] = 0.1 * tp_allow->tab[row][col+1];
	c[3] = 0.1 * tp_allow->tab[row+1][col+1];

	if (diagnostic){
		fprintf(logfp,"other_phase: depth=%f depth_erad=%f ",depth,depth_erad);
		fprintf(logfp,"depth_frac=%f ",depth_frac);
		fprintf(logfp,"c= %f %f %f %f\n",c[0],c[1],c[2],c[3]);
	}

	depth_corr  = c[0] + delta_frac*(c[1]-c[0]);
	depth_corr += depth_frac*(c[2]-c[0]);
	depth_corr += depth_frac*delta_frac*(c[0]-c[1]+c[3]-c[2]);

	if (diagnostic)
		fprintf(logfp,"other_phase:depth_corr=%f\n",depth_corr);

	*ttime = surf_time - depth_corr;
	return 0;
}

/*#DOC  Title:																*/
/*#DOC    jb_pP_P															*/
/*#DOC  Desc:																*/
/*#DOC    Looks up pP - P time and dtdh in JB table.						*/
/*#DOC  Input Arguments:													*/
/*#DOC    delta - distance between phase and source in degrees.				*/
/*#DOC    depth - depth of current solution.								*/
/*#DOC    timediff - pointer to variable for difference in travel times.	*/
/*#DOC    dtdh  - pointer to variable for dt/dh.							*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC    interpol() and int_diff() from this file.							*/

/* This is the only function here not called by read_jb(). Rather it is		*/
/* called from read_pP_P(), which provides a table independent wrapper for	*/
/* calculating P-pP times.													*/

int jb_pP_P( double delta, double depth, double* timediff, double* dtdh)
{
	double depth_erad,moho;
	double depth_frac,delta_frac;
	int times[3];
	int col1,row1,col,row;
	double t[3],d[3];
	int delta_dir;
	int i,j;
	struct tab_rec* tp;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	tp = &pP_P_tab;

	/* Get column by using depth in earth radii * 100 */
	/* One out of step because 1st column is surface, 2nd column is 0.00 */
	depth_erad = (depth - MOHO)/MOHO_RADIUS;
	if (depth_erad < tp->min_depth || depth_erad > tp->max_depth){
		sprintf(errstr,"jb_pP_P: depth out of range: %f",depth);
		return 1;
	}
	col1  = depth_erad*100 + 1;
	depth_frac = depth_erad*100 - (int)(depth_erad*100);

	/* Get row by using delta / 5 */
	if (delta < tp->min_delta || delta > tp->max_delta){
		sprintf(errstr,"jb_pP_P: delta out of range: %f",delta);
		return 1;
	}
	row1 = delta*0.2;
	delta_frac = delta*0.2 - row1;

	if (diagnostic){
		fprintf(logfp,"jb_pP_P: depth=%f depth_erad=%f ",depth,depth_erad);
		fprintf(logfp,"col1=%d depth_frac=%f ",col1,depth_frac);
		fprintf(logfp,"delta=%f,row1=%d,frac=%f\n",delta,row1,delta_frac);
	}

	/* Depth above MOHO - extrapolate off edge of table. */
	if (col1 == 0){
		row = row1 + 1;

		t[0]=tp->tab[1][row];
		t[1]=tp->tab[1][row+1];

		if (t[0]==0 || t[1]==0)
			return 0;

		moho = 100*MOHO/MOHO_RADIUS;

		*timediff = 0.1 * (1 + depth_frac/moho)*(delta_frac*(t[1]-t[0]) + t[0]);

		/* dtdh in km. */
		*dtdh     = 0.1 * (t[0]/MOHO);
	}
	/* Depth below MOHO. */
	else {

		/* Interpolate forwards in first half of table and backwards */
		/* in second half of table to avoid the ends. */
		if (delta > tp->switch_dir_delta){
			delta_dir = -1;
			row1++;
			delta_frac = 1 - delta_frac;
		}
		else
			delta_dir = 1;

		/* Store the 3 values surrounding the delta of interest. */
		for (i=0; i<3; i++){
			row = row1 + i*delta_dir;
			for (j=0; j<3; j++){
				col = col1 + j;

				/* If on the table. */
				if (col<14){
					times[j] = tp->tab[col][row];
					if (times[j]==0)
						return 0;
				}
				/* If off the bottom of the table. */
				else{
					times[j]  = tp->tab[13][row];
					times[j] += (col-13)*(tp->tab[14][row]-tp->tab[13][row]);
				}
			}

			if (diagnostic)
				fprintf(logfp,"t[%d]=%d %d %d\n",i,times[0],times[1],times[2]);

			t[i] = interpol(depth_frac,times[0],times[1],times[2]);
			d[i] = int_diff(depth_frac,times[0],times[1],times[2]);
		}
		*timediff = 0.1 * interpol(delta_frac,t[0],t[1],t[2]);

		/* Convert dtdh from e.r.*100 to km. 0.1*100=10 */
		*dtdh = 10*interpol(delta_frac,d[0],d[1],d[2])/MOHO_RADIUS;
	}
	return 0;
}

/*#DOC  Title:																*/
/*#DOC    interpol															*/
/*#DOC  Desc:																*/
/*#DOC    Newton Gregory interpolation from three points.					*/
/*#DOC  Input Arguments:													*/
/*#DOC    frac  - fraction of table interval.								*/
/*#DOC    v1, v2, v3 - table values to either side.							*/
/*#DOC  Return:																*/
/*#DOC    Interpolated value.												*/

double interpol ( double frac, double v1, double v2, double v3 )
{
	double val;

	val  = v1 * ( 1 - 0.5 * frac * (3-frac) );
	val += v2 * frac * (2-frac);
	val += v3 * 0.5 * frac * (frac-1);

	return val;
}

/*#DOC  Title:																*/
/*#DOC    int_diff															*/
/*#DOC  Desc:																*/
/*#DOC    Newton Gregory difference interpolation from three points.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    frac  - fraction of table interval.								*/
/*#DOC    v1, v2, v3 - table values to either side.							*/
/*#DOC  Return:																*/
/*#DOC    Interpolated difference.											*/

double int_diff ( double frac, double v1, double v2, double v3 )
{
	double val;

	val  = v1 * (frac-1.5);
	val += v3 * (frac-0.5);
	val -= v2 * 2 * (frac-1);

	return val;
}

/*#DOC    read_ttime														*/
/*#DOC  Desc:																*/
/*#DOC    Wrapper for whatever travel time function is in use.				*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing current solution.			*/
/*#DOC    pp - pointer to a phase structure.								*/
/*#DOC  Return:																*/
/*#DOC    0/1/2/3 for success/failure/warning/wrong phase.					*/
/*#DOC  Calls:																*/
/*#DOC	  read_jb() from read_jb.c.											*/
/*#DOC	  correct_ttime() from correct_ttime.c.								*/
/*#DOC	  add_to_error() from utils.c.										*/

/* This function will need to be rewritten to use a different earth model,	*/
/* even if just to change the called function's name.						*/

/* Set external max_depth to greatest depth that occurs in tables (in km).	*/
/* This is required by calc_resid to weed out solutions that travel times	*/
/* can't be calculated for.													*/
double max_depth = EARTH_RADIUS*MAX_DEPTH;

int read_ttime (struct sol_rec *sp, struct pha_rec *pp)
{
	int status;
	int read_jb (double depth, struct pha_rec *pp);

	status = read_jb (sp->depth,pp);
	if (status){
		add_to_error("read_jb:");
		return status;
	}

	if (correct_ttime (sp,pp)){
		return 1;
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    solve																*/
/*#DOC  Desc:																*/
/*#DOC    Solves equations and updates solution structure.					*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp - pointer to structure containing solution.					*/
/*#DOC    p  - array of pointers to structures containing phases.			*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/

/* Does not calculate errors - the covariance matrix is stored so that	*/
/* another function can do this.										*/

int solve (struct sol_rec *sp, struct pha_rec p[])
{
	extern double max_err_factor;		/* From config file */

	double *simula[5];		/* Maximum of 4 unkowns - don't use index 0. */
	double *simulb;
	double c[5][5];
	double d[5];
	double e[5];
	double f[10][5];
	double ermax[5];
	double y;
	double sqrt_weight;
	int dir;
	int nunk,num,npp;
	int i,j,k;

	/* Switch debug messages from this function on/off (1/0). */
	int diagnostic = 0;

	/* Local variables with nice short names for use in for statements etc. */
	nunk = sp->number_of_unknowns;
	num  = sp->numphas;
	npp  = nunk*2 +1;

	/* If everything fixed than nothing to do. */
	if (nunk == 0){
		return 0;
	}
	else if (nunk==2 || nunk > 4){
		sprintf(errstr,"solve: number of unknowns wrong %d",nunk);
		return 1;
	}

	/* Assign memory for arrays of simultaneous equation coefficients. */
	if ((simulb = (double*) calloc (num, sizeof(double))) == NULL){
		sprintf(errstr,"solve: Memory problem, simulb.");
		return 1;
	}
	for (i=0;i<=5;i++){
		if ((simula[i] = (double*) calloc (num, sizeof(double))) == NULL){
			sprintf(errstr,"solve: Memory problem, simula[%d].",i);
			free(simulb);
			for(j=0;j<i;j++)
				free(simula[j]);
			return 1;
		}
	}

	/* Fill in equation for each phase. */
	/* And apply weights. */
	for (i=0; i<num; i++){
		if (p[i].weight_factor==0)
			continue;

		simula[1][i] = 1;
		simulb[i] = p[i].resid;

		if (nunk > 2){
			simula[2][i] = -p[i].dtdd * cos(p[i].esaz*TORAD);
			simula[3][i] = -p[i].dtdd * sin(p[i].esaz*TORAD);
		}
		if (nunk> 3)
			simula[4][i] = p[i].dtdh;

		sqrt_weight = sqrt(p[i].weight);
		simulb[i] *= sqrt_weight;

		if (diagnostic){
			fprintf(logfp,"solve: %s %s ",p[i].sta,p[i].phase);
			fprintf(logfp,"simulb=%f\n",simulb[i]);
		}
		for (j=1; j<=nunk; j++)
			simula[j][i] *= sqrt_weight;
	}

	/* Fill matrix C and vector D */
	for (i=1; i<=nunk; i++){
		for (j=1; j<=nunk; j++){
			c[j][i] = 0;
			for (k=0; k<num; k++)
				if (p[k].weight_factor > 0)
					c[j][i] += simula[i][k] * simula[j][k];
		}
		d[i] = 0;
		for (k=0; k<num; k++)
				if (p[k].weight_factor > 0){
					d[i] += simula[i][k] * simulb[k];
				}
	}

	/* Free memory used for simultaneous equation coefficients. */
	free(simulb);
	for (i=0;i<=5;i++)
		free(simula[i]);

	if (diagnostic){
		fprintf(logfp,"solve:c1=%f %f %f %f\n",c[1][1],c[2][1],c[3][1],c[4][1]);
		fprintf(logfp,"      c2=%f %f %f %f\n",c[1][2],c[2][2],c[3][2],c[4][2]);
		fprintf(logfp,"      c3=%f %f %f %f\n",c[1][3],c[2][3],c[3][3],c[4][3]);
		fprintf(logfp,"      c4=%f %f %f %f\n",c[1][4],c[2][4],c[3][4],c[4][4]);
		fprintf(logfp,"       d=%f %f %f %f\n\n",d[1],d[2],d[3],d[4]);
	}

	/* Augment C with D and unitary matrix to get F. */
	for (i=1; i<=nunk; i++){

		for (j=1; j<=nunk; j++)
			f[j][i] = c[j][i];

		f[nunk+1][i] = d[i];

		for (j=nunk+2; j<=npp; j++)
			f[j][i] = 0;
		
		f[nunk+1+i][i] = 1;
	}

	/* Set minimum values for diagonal elements. */
	/* Depth minimum adjusted to match dtdh in s/km. */
	ermax[1] = nunk + 0.5 * max_err_factor;
	ermax[2] = nunk * max_err_factor;
	ermax[3] = nunk * max_err_factor;
	ermax[4] = 0.0001 * max_err_factor;

	/* Triangulate F */
	for (i=1; i<=nunk; i++){

		/* Check for diagonals getting too small. */
		if (f[i][i] < ermax[i]){
			fprintf(logfp,"SOLUTION INDETERMINATE, row %d\n",i);
			if (diagnostic)
				fprintf(logfp,"ermax=%f value=%f\n",ermax[i],f[i][i]);
			strcpy(errstr,"");
			return 1;
		}

		y = 1/f[i][i];
		for (j=i; j<=npp; j++)
			f[j][i] = f[j][i]*y;

		if (i<nunk){
			for(j=i+1; j<=nunk; j++){
				y = f[i][j];
				for (k=i; k<=npp; k++)
					f[k][j] -= y * f[k][i];
			}
		}
	}

	/* Back-substitute for solution. */
	e[nunk] = f[nunk+1][nunk];
	if (nunk>1){
		for (i=nunk-1; i>0; i--){
			e[i] = f[nunk+1][i];
			for (j=i+1; j<=nunk; j++)
				e[i] -= e[j]*f[j][i];
		}
		/* Back-substitute for standard errors. */
		for (i=2*nunk; i>nunk+1; i--)
			for (j=nunk-1; j>0; j--)
				for (k=j+1; k<=nunk; k++)
					f[i][j] -= f[k][j]*f[i][k];
	}

	if (diagnostic)
		fprintf(logfp,"solve: e=%f %f %f %f\n\n",e[1],e[2],e[3],e[4]);

	/* Store covariance matrix in solution structure. */
	for (i=1; i<=nunk; i++)
		for (j=1; j<=nunk; j++)
			sp->covar[j][i] = f[nunk+1+j][i];

	/* Update solution. */
	sp->time += e[1];

	if (nunk>1){
		sp->lat += e[2];
		if (fabs(sp->lat) >= 90){
			sp->lat = sp->lat/fabs(sp->lat) * 180 - sp->lat;
			sp->lon = sp->lon/fabs(sp->lon) * 180 - sp->lon;
			dir = -1;
		}
		else
			dir = 1;

		sp->lon += dir * e[3]/cos(sp->lat*TORAD);
		if (fabs(sp->lon) >= 180)
			sp->lon -= sp->lon/fabs(sp->lon) * 360;

		calc_dircos(sp->lat,sp->lon,sp->dircos);

		if (nunk==4){
			sp->depth += e[4];

			/* Check for negative depth. */
			if (sp->depth < 0){
				fprintf(logfp,"NEGATIVE DEPTH\n");
				strcpy(errstr,"");
				return 1;
			}
		}
	}
	return 0;
}


/*#DOC  Title:																*/
/*#DOC    utils.c															*/
/*#DOC  Desc:																*/
/*#DOC   File with the following utility functions used by location:		*/
/*#DOC    print_sol															*/
/*#DOC    print_pha															*/
/*#DOC    calc_delta														*/
/*#DOC    calc_esaz															*/
/*#DOC    calc_dircos														*/
/*#DOC    dsort																*/
/*#DOC    read_time															*/
/*#DOC    write_time														*/
/*#DOC    split_time														*/
/*#DOC    join_time															*/
/*#DOC    add_to_error														*/
/*#DOC    handle_error														*/


/*#DOC  Title:																*/
/*#DOC    print_sol															*/
/*#DOC  Desc:																*/
/*#DOC   Prints one line with the current solution as a diagnostic.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to structure containing current solution.			*/
/*#DOC  Return:																*/
/*#DOC    None.																*/
/*#DOC  Uses:																*/
/*#DOC    write_time() from this file.										*/

void print_sol(struct sol_rec *sp)
{
	char timestr[24];

	fprintf(logfp,"%s ",write_time(sp->time,timestr));
	if (sp->error[2] != NULLVAL)
		fprintf(logfp,"%6.3f+%6.3f ",sp->lat,sp->error[2]);
	else
		fprintf(logfp,"%6.3f+%6s ",sp->lat,"");
	if (sp->error[3] != NULLVAL)
		fprintf(logfp,"%6.3f+%6.3f ",sp->lon,sp->error[3]);
	else
		fprintf(logfp,"%6.3f+%6s ",sp->lon,"");
	if (sp->error[4] != NULLVAL)
		fprintf(logfp,"%.0f+%4.1f ",sp->depth,sp->error[4]);
	else
		fprintf(logfp,"%.0f+%4s ",sp->depth,"");
	if (sp->sdobs != NULLVAL)
		fprintf(logfp,"sd=%3.1f\n",sp->sdobs);
	else
		fprintf(logfp,"sd=%3s\n","");

	return;
}

/*#DOC  Title:																*/
/*#DOC    print_pha															*/
/*#DOC  Desc:																*/
/*#DOC   Prints a table with all the phases for one event.					*/
/*#DOC  Input Arguments:													*/
/*#DOC    numphas - number of phases in p[].								*/
/*#DOC    p[] - array of phase structures.									*/
/*#DOC  Return:																*/
/*#DOC    None.																*/
/*#DOC  Uses:																*/
/*#DOC    write_time() from this file.										*/

void print_pha(int numphas, struct pha_rec p[])
{
	int i;
	int ndef;
	char timestr[24];
	struct pha_rec temp_pha;

	fprintf(logfp,"RDID      NET   STA     REP   ISC   TIME                  ");
	fprintf(logfp,"    DELTA FACT  WT       RESID       AMP  PER  MAG\n");

	ndef=0;
	for (i=0; i< numphas; i++){
		fprintf(logfp,"%-9d ",p[i].rdid);
		if (strcmp(p[i].net,""))
			fprintf(logfp,"%-5s ",p[i].net);
		else
			fprintf(logfp,"%5s ","");
		fprintf(logfp,"%-5s ",p[i].sta);
		if (p[i].comp)
			fprintf(logfp,"%c ",p[i].comp);
		else
			fprintf(logfp,"  ");
		fprintf(logfp,"%-5s ",p[i].rep_phase);
		fprintf(logfp,"%-5s ",p[i].phase);
		fprintf(logfp,"%23s ",write_time(p[i].time,timestr));
		fprintf(logfp,"%7.3f ",p[i].delta);
		if (p[i].purged) 
			fprintf(logfp,"PURGE ");
		else if (p[i].duplicate == 1) 
			fprintf(logfp,"DUPLI ");
		else
			fprintf(logfp,"%0.3f ",p[i].weight_factor);
		fprintf(logfp,"%0.4f ",p[i].weight);
		if (p[i].resid != NULLVAL)
			fprintf(logfp,"%+8.4f ",p[i].resid);
		else
			fprintf(logfp,"%8s ","");
		if (p[i].numamps>0){
			if (p[i].a[0].amp!=NULLVAL) 
				fprintf(logfp,"%8.1f ",p[i].a[0].amp);
			else
				fprintf(logfp,"%8s ","");
			if (p[i].a[0].per!=NULLVAL)
				fprintf(logfp,"%4.1f ",p[i].a[0].per);
			else
				fprintf(logfp,"%4s ","");
		}
		else
			fprintf(logfp,"%8s %4s ","","");
		if (p[i].bodymag)
			fprintf(logfp,"%4.2fb ",p[i].bodymag);
		if (p[i].surfmag)
			fprintf(logfp,"%4.2fS ",p[i].surfmag);

		fprintf(logfp,"\n");
		if (p[i].weight > 0) ndef++;
	}
	fprintf(logfp,"ndef=%d\n",ndef);

	return;
}

/*#DOC  Title:																*/
/*#DOC    calc_delta														*/
/*#DOC  Desc:																*/
/*#DOC    Calculates delta between two points given thier direction cosines.*/
/*#DOC  Input Arguments:													*/
/*#DOC    sta_dircos[] - array of 6 direction cosines for station.			*/
/*#DOC    h_dircos[] - array of 6 direction cosines for hypocentre.			*/
/*#DOC  Return:																*/
/*#DOC    delta as a double.												*/

/*	Calculate distance in radians between station and epicentre.	*/
/*	Reference: Bullen K.E., 1963. An Introduction to the Theory		*/
/*	of Seismology, pp 154-155.										*/

/*	Expects arrays of 3 direction cosines for station and location.	*/
/*	Returns delta in degrees.										*/

double calc_delta(double sta_dircos[], double h_dircos[])
{
	double dotprod;
	double sq1,sq2,sq3;

	dotprod  = sta_dircos[1] * h_dircos[1];
	dotprod += sta_dircos[2] * h_dircos[2];
	dotprod += sta_dircos[3] * h_dircos[3];

	/* Corresponds to delta > 178 deg */
	if (dotprod < -0.9994){
		sq1 = sta_dircos[1] + h_dircos[1];
		sq2 = sta_dircos[2] + h_dircos[2];
		sq3 = sta_dircos[3] + h_dircos[3];

		dotprod = 0.5*(sq1*sq1 + sq2*sq2 + sq3*sq3) - 1;

		if (dotprod < -1) dotprod = -1;
	}
	/* Corresponds to delta < 2 deg */
	else if (dotprod > 0.9994){
		sq1 = sta_dircos[1] - h_dircos[1];
		sq2 = sta_dircos[2] - h_dircos[2];
		sq3 = sta_dircos[3] - h_dircos[3];

		dotprod = 1 - 0.5*(sq1*sq1 + sq2*sq2 + sq3*sq3);

		if (dotprod > 1) dotprod = 1;
	}

	return acos(dotprod)/TORAD;
}

/*#DOC  Title:																*/
/*#DOC    calc_esaz															*/
/*#DOC  Desc:																*/
/*#DOC    Calculates back azimuth between epicentre and station.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    sta_dircos[] - array of 6 direction cosines for station.			*/
/*#DOC    h_dircos[] - array of 6 direction cosines for hypocentre.			*/
/*#DOC    sta_lon - station longitude.										*/
/*#DOC    h_lon   - hypocentre longitude.									*/
/*#DOC    delta   - distance between the two in degrees.					*/
/*#DOC  Return:																*/
/*#DOC    azimuth as a double.												*/

double calc_esaz(double sta_dircos[], double h_dircos[], double sta_lon,
				 double h_lon, double delta)
{
	double da,dellon,azim;

	da  = (sta_dircos[1]-h_dircos[4]) * (sta_dircos[1]-h_dircos[4]);
	da += (sta_dircos[2]-h_dircos[5]) * (sta_dircos[2]-h_dircos[5]);
	da += (sta_dircos[3]-h_dircos[6]) * (sta_dircos[3]-h_dircos[6]);
	da -= 2;
	da /= 2*sin(TORAD*delta);

	if (da > 1) da = 1;
	if (da < -1) da = -1;

	azim =acos(da);

	dellon = sta_lon - h_lon;

	if ( dellon>180 || (dellon>-180 && dellon<0) )
		azim = 2*PI - azim;

	/* Rounding can put value just over 360. */
	azim /= TORAD;
	if (azim >= 360) azim -= 360;

	return azim;
}

/*#DOC  Title:																*/
/*#DOC    calc_dircos														*/
/*#DOC  Desc:																*/
/*#DOC    Calculates direction cosines given latitude and longitude.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    lat - latitude.													*/
/*#DOC    lon - longitude.													*/
/*#DOC    dircos[] - array for 6 direction cosines.							*/
/*#DOC  Return:																*/
/*#DOC    nothing															*/

void calc_dircos (double lat, double lon, double dircos[])
{
	double glat;						/* geocentric latitude */
	double coslat,sinlat,coslon,sinlon;

	lat = TORAD*lat;
	lon = TORAD*lon;

	glat = atan(ELLIPT * tan(lat));
	coslat = cos(glat);
	sinlat = sin(glat);
	coslon = cos(lon);
	sinlon = sin(lon);

	dircos[1] = coslat * coslon;
	dircos[2] = coslat * sinlon;
	dircos[3] = sinlat;
	dircos[4] = sinlat * coslon;
	dircos[5] = sinlat * sinlon;
	dircos[6] = -coslat;
} 

/*#DOC  Title:																*/
/*#DOC    dsort																*/
/*#DOC  Desc:																*/
/*#DOC    Sorts an array of doubles into descending order.					*/
/*#DOC  Input Arguments:													*/
/*#DOC    a[]   - array to be sorted.										*/
/*#DOC    left  - index of first value.										*/
/*#DOC    right - index of last value.										*/
/*#DOC  Return:																*/
/*#DOC    nothing															*/
/*#DOC  Calls:																*/
/*#DOC  	dswap() from this file and itself recursively.					*/

void dsort (double a[], int left, int right)
{
	int i,last;

	void dswap (double[], int, int);

	if(left>=right)
		return;

	dswap(a, left, (left+right)/2);
	last = left;
	for (i=left+1; i<=right; i++)
		if (a[i] < a[left])
			dswap(a, ++last, i);
	dswap(a, left, last);
	dsort(a,left,last);
	dsort(a,last+1,right);
}

/*#DOC  Title:																*/
/*#DOC    dswap ( called only from dsort(). )								*/
/*#DOC  Desc:																*/
/*#DOC    Part of the function dsort().										*/
/*#DOC  Input Arguments:													*/
/*#DOC    a[]  - array to be sorted.										*/
/*#DOC    i,j  - indexes on this array.										*/
/*#DOC  Return:																*/
/*#DOC    nothing															*/

void dswap(double a[], int i, int j)
{
	double temp;

	temp=a[i];
	a[i]=a[j];
	a[j]=temp;
}

/* Local functions called only by time routines below. */
int days_per_month (int mm, int yyyy);
int days_per_year (int yyyy);

/*#DOC  Title:																*/
/*#DOC    read_time															*/
/*#DOC  Desc:																*/
/*#DOC    Converts time from ISF format string to seconds since epoch.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    date/time as a string.											*/
/*#DOC  Return:																*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC    0 on error.														*/
/*#DOC  Calls:																*/
/*#DOC    join_time() from this file.										*/

double read_time(char *timestr)
{
	int yyyy,mm,dd,hh,mi,ss,msec;
	double sec;
	char cutstr[24];
	char *remnant;

	strcpy(cutstr,timestr);

	cutstr[4] = '\0';
	yyyy = strtol(&cutstr[0],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[7] = '\0';
	mm = strtol(&cutstr[5],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[10] = '\0';
	dd = strtol(&cutstr[8],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[13] = '\0';
	hh = strtol(&cutstr[11],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[16] = '\0';
	mi = strtol(&cutstr[14],&remnant,10);
	if (remnant[0]) return 0;

	sec = strtod(&cutstr[17],&remnant);
	if (remnant[0]) return 0;

	ss = (int)sec;
	sec -= ss;
	msec = sec*1000+0.5;

	return join_time(yyyy,mm,dd,hh,mi,ss,msec);
}

/*#DOC  Title:																*/
/*#DOC    write_time														*/
/*#DOC  Desc:																*/
/*#DOC    Converts times from seconds since epoch to ISF date format.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC    out - pointer to memory put aside for 24 chars.					*/
/*#DOC  Return:																*/
/*#DOC   date/time as a string.												*/
/*#DOC  Calls:																*/
/*#DOC  	split_time() from this file.									*/

char* write_time(double time, char* out)
{
	int yyyy,mm,dd,hh,mi,ss,msec;

	/* Get constituent parts of date and time. */
	if (split_time(time,&yyyy,&mm,&dd,&hh,&mi,&ss,&msec))
		return "";

	/* Create and return the ISF string*/
	sprintf(out,"%02d-%02d-%d %02d:%02d:%02d.%03d",dd,mm,yyyy,hh,mi,ss,msec);

	return out;
}

/*#DOC  Title:																*/
/*#DOC    split_time														*/
/*#DOC  Desc:																*/
/*#DOC    Converts times from seconds since epoch to seperate ints.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC    yyyy,mm,dd,hh,mi,ss,msec - pointers for parts of date/time.		*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC  	days_per_month() and days_per_year() from this file.			*/

int split_time(double time, int *yyyy, int *mm, int *dd,int *hh, int *mi, 
															int *ss, int *msec)
{
	int year,month,hour,min;
	double day,sec;

	/* Switch debug messages from main on/off (1/0). */
	int diagnostic = 0;

	if (time == NULLVAL)
		return 1;

	if (diagnostic)
		fprintf(logfp,"split_time: time=%f ",time);

	/* Split into days before/after epoch and seconds since start of day. */

	/* Get date from day. */
	day = (int)(time/86400);
	year= EPOCH_YEAR;
	
	if (day >= 0){
		sec  = time - day*86400;
		day++;
		while (day > days_per_year(year)){
			day -= days_per_year(year);
			year++;
		}
	}
	else{
		sec  = time - day*86400;
		if (sec != 0){
			day--;
			sec+=86400;
		}
		while (day < 0 ){
			day += days_per_year(year);
			year--;
		}
	}

	for (month=1; month<13; month++){
		if (day <= days_per_month(month,year)){ break; }
		day -= days_per_month(month,year);
	}

	if (diagnostic)
		fprintf(logfp,"split_time: year=%d month=%d day=%f ",year,month,day);

	/* Get time from sec */
	min  = sec/60;
	sec -= min*60;
	hour = min/60;
	min -= hour*60;

	if (diagnostic)
		fprintf(logfp,"split_time: hour=%d min=%d sec=%f\n",hour,min,sec);

	*yyyy = year;
	*mm = month;
	*dd = day;
	*hh = hour;
	*mi = min;
	*ss = (int)sec;
	*msec = (sec-*ss)*1000;
	*msec = (int)(*msec+0.5);

	return 0;
}


/*#DOC  Title:																*/
/*#DOC    join_time															*/
/*#DOC  Desc:																*/
/*#DOC    Converts times from seperate ints to seconds since epoch.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    yyyy,mm,dd,hh,mi,ss,msec - parts of date/time.					*/
/*#DOC  Return:																*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC  Calls:																*/
/*#DOC  	days_per_month() and days_per_year() from this file.			*/

double join_time(int yyyy, int mm, int dd, int hh, int mi, int ss, int msec)
{
	int year,month;
	double day,time;

	/* Switch debug messages from main on/off (1/0). */
	int diagnostic = 0;

	if (yyyy == NULLVAL)
		return NULLVAL;

	if (diagnostic){
		fprintf(logfp,"join_time: %d %d %d ",yyyy,mm,dd);
		fprintf(logfp,"%d %d %d %d\n",hh,mi,ss,msec);
	}

	day = 0;
	if (yyyy > EPOCH_YEAR)
		for (year=EPOCH_YEAR; year<yyyy; year++)
			day += days_per_year(year);
	else
		for (year=yyyy; year<EPOCH_YEAR; year++)
			day -= days_per_year(year);

	for (month=1; month<mm; month++)
		day += days_per_month(month,yyyy);
	day += dd-1;

	time  = day*86400;
	time += hh*3600;
	time += mi*60;
	time += ss;
	if (msec != NULLVAL)
		time += msec*0.001;

	if (diagnostic)
		fprintf(logfp,"join_time: %f\n",time);

	return time;
}

/*#DOC  Title:																*/
/*#DOC    days_per_month													*/
/*#DOC  Desc:																*/
/*#DOC    Returns the number of days in a given month.						*/
/*#DOC  Input Arguments:													*/
/*#DOC    mm - month number (1-12).											*/
/*#DOC    yyyy - year.														*/
/*#DOC  Return:																*/
/*#DOC   number of days in the month.										*/

int days_per_month (int mm, int yyyy)
{
	int days_per_month[] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };

	/* Case of typical year or not February */
	if (mm !=2 || yyyy%4 != 0){ return days_per_month[mm]; }

	/* Case of February in a typical leap year */
	if (yyyy%100 != 0){ return 29;}

	/* Case of February in last year of a typical century */
	if (yyyy%400 != 0){ return 28; }

	/* Case of February in last year of a special century */
	return 29;
}

/*#DOC  Title:																*/
/*#DOC    days_per_year ( called only from day_string(). )					*/
/*#DOC  Desc:																*/
/*#DOC    Returns the number of days in a given year.						*/
/*#DOC  Input Arguments:													*/
/*#DOC    yyyy - year.														*/
/*#DOC  Return:																*/
/*#DOC   number of days in the year.										*/

int days_per_year (int yyyy)
{
	/* Case of typical year */
	if (yyyy%4 != 0){ return 365; }

	/* Case of a typical leap year */
	if (yyyy%100 != 0){ return 366;}

	/* Case of last year of a typical century */
	if (yyyy%400 != 0){ return 365; }

	/* Case of February in last year of a special century */
	return 366;
}


/*#DOC  Title:																*/
/*#DOC    add_to_error														*/
/*#DOC  Desc:																*/
/*#DOC    Adds a string to the end of global string 'errstr'.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    part_error - characters to be added.								*/
/*#DOC  Return:																*/
/*#DOC   nothing.															*/

void add_to_error(char *part_error){

	char tempstr[ERRLEN];

	strcpy(tempstr,errstr);

	if (strlen(tempstr) + strlen(part_error) > ERRLEN)
		sprintf(errstr,"%s .......",part_error);
	else
		sprintf(errstr,"%s %s",part_error,tempstr);

	return;
}

/*#DOC  Title:																*/
/*#DOC    handle_error														*/
/*#DOC  Desc:																*/
/*#DOC    Prints global string 'errstr' and resets it to null.				*/
/*#DOC  Input Arguments:													*/
/*#DOC    none.																*/
/*#DOC  Return:																*/
/*#DOC   nothing.															*/

/* Can return control to calling function without printing error by		*/
/* setting errstr to "".												*/

void handle_error(void){

	if (strcmp(errstr,"")){

		fprintf(errfp,"ISCLOC ERROR: %s\n",errstr);
		if (errfp != logfp)
			fprintf(logfp,"ISCLOC ERROR: %s\n",errstr);
	}
	return;
}
