/* Need these parameters in a seperate file from the tables so that 	*/
/* subroutines other than read_jb can access them.					
	*/

/* Reference given: Jeffreys 1939 M.N.R.A.S. Geoph. Supp. */
#define PGVEL 5.57				/* Pg velocity km/s */
#define SGVEL 3.34				/* Sg velocity km/s */
#define PBVEL 6.50				/* P* velocity km/s */
#define SBVEL 3.83				/* S* velocity km/s */
#define PNVEL 7.88				/* P velocity top of mantle km/s 
*/
#define SNVEL 4.38				/* S velocity top of mantle km/s 
*/

#define PGSLOW 19.95				/* Pg slowness s/deg */
#define SGSLOW 33.3					/* Sg slowness s/deg */
#define PBSLOW 17.1					/* P* slowness s/deg */
#define SBSLOW 29.0					/* S* slowness s/deg */

/* Densities from Bullen, 1963, An Introduction to the Theory of Seismology. */
#define UPPER_RHO 2.65				/* Upper crust. */
#define LOWER_RHO 2.87				/* Lower crust. */
#define BELOW_RHO 3.32				/* Just below Moho. */

#define MOHO   33.0
#define MOHO_RADIUS 6338
#define CONRAD 15.0
#define SURFACE -0.005207			/* 0 km depth in earth radii. */
#define MAX_DEPTH 0.12				/* Right hand edge of most JB 
tables. */

/* Delta and depth below which 'Short Epicentral Distance' tables are used. */
#define SHORT_DELTA	1.3
#define SHORT_DEPTH 134.408				/* 0.016 in earth radii */

/* Delta boundaries between phases. */
#define CRUSTAL_P_DELTA 8
#define CRUSTAL_S_DELTA 10
#define PKP_MIN_DELTA   106
#define S_MAX_DELTA     105
#define SKS_MIN_DELTA   62

/* Calculate Pdiff ttimes by adding this number of sec per degree over 102 */
#define PDIFF_MIN_DELTA 102
#define PDIFF_SEC_PER_DEG 4.4


/* Tables themselves are stored in jb_tables.h. */
/* This is the declaration for them. */
#define MAXROW 230
#define MAXCOL 30

struct tab_rec {
	double min_delta;
	double switch_dir_delta;		/* Needed to be consistent with 
reviser */
	double max_delta;
	double min_depth;
	double max_depth;
	double tab[MAXCOL][MAXROW];
};

struct surf_tab_rec {
	double min_delta;
	double max_delta;
	struct dep_allow_tab_rec *dep_allow_tp;
	double tab[MAXROW];
};

struct dep_allow_tab_rec {
	double min_depth;
	double max_depth;
	int    numrows;
	double row_label[MAXROW];
	double tab[MAXCOL][MAXROW];
};

extern struct tab_rec p_tab;
extern struct tab_rec pkp_tab;
extern struct tab_rec s_tab;
extern struct tab_rec sks_tab;
extern struct dep_allow_tab_rec pcp_dep_allow_tab;
extern struct surf_tab_rec pcp_surf_tab;
extern struct dep_allow_tab_rec scs_dep_allow_tab;
extern struct surf_tab_rec scs_surf_tab;
extern struct dep_allow_tab_rec pp_dep_allow_tab;
extern struct surf_tab_rec pp_surf_tab;
extern struct dep_allow_tab_rec ppp_dep_allow_tab;
extern struct surf_tab_rec ppp_surf_tab;
extern struct dep_allow_tab_rec ps_dep_allow_tab;
extern struct surf_tab_rec ps_surf_tab;
extern struct dep_allow_tab_rec sp_dep_allow_tab;
extern struct surf_tab_rec sp_surf_tab;
extern struct dep_allow_tab_rec pps_psp_dep_allow_tab;
extern struct surf_tab_rec pps_psp_surf_tab;
extern struct dep_allow_tab_rec spp_dep_allow_tab;
extern struct surf_tab_rec spp_surf_tab;
extern struct dep_allow_tab_rec ss_dep_allow_tab;
extern struct surf_tab_rec ss_surf_tab;
extern struct dep_allow_tab_rec ssp_sps_dep_allow_tab;
extern struct surf_tab_rec ssp_sps_surf_tab;
extern struct dep_allow_tab_rec pss_dep_allow_tab;
extern struct surf_tab_rec pss_surf_tab;
extern struct dep_allow_tab_rec sss_dep_allow_tab;
extern struct surf_tab_rec sss_surf_tab;

#define pP_LOW_DELTA 25
#define pP_HIGH_DELTA 100

int ps_star( char, double delta, double depth,
             double* ttime, double* dtdh, double* dtdd);

int ps_g( char, double delta, double depth,
          double* ttime, double* dtdh, double* dtdd);

int ps_crust( char, double delta, double depth,
              double* ttime, double* dtdh, double* dtdd);

int ps( struct tab_rec*, double delta, double depth,
        double* ttime, double* dtdh, double* dtdd);

int ps_short( struct tab_rec*, double delta, double depth,
              double* ttime, double* dtdh, double* dtdd);

int pdiff( struct tab_rec*, double delta, double depth, double* ttime);

int jb_pP_P( double delta, double depth, double* timediff, double* dtdh);

int other_phase( struct surf_tab_rec*,double delta,double depth,double* ttime);

double height_above_mean_sphere(double lat);

