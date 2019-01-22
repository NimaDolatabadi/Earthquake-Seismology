#include "iscloc.h"
#include "iscloc_jb_model.h"

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
