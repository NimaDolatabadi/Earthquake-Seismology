#ifndef REA_C_H
#define REA_C_H

#include <stdlib.h>
#include <stdio.h>

#define MAX_DATA 10000
#define MAX_SFILE_LINES 10000   // Max number of lines in an S-file.

typedef unsigned int uint;
typedef signed int sint;


//
// Declaration of fortran common block structures for C language.
//
extern "C" {
   // External fortran function prototypes.
   void rea_event_in_(sint*, sint*, char*, sint*, uint);
   void rea_event_out_(sint*, sint*, char*, sint*, uint);

   // External fortran common blocks.
   extern struct comblck_hyp1 {
      char hyp_model[100], hyp_dist_id[100], hyp_type[100], hyp_fix_org[100];
      char hyp_depth_flag[100], hyp_epi_flag[100], hyp_mag_type[100][6];
      int hyp_high_accuracy[100], hyp_error[100];
      char hyp_mag_type_all[200], mt_coor[100];
   }  hyp1_ ;

   extern struct comblck_hyp4 {
      int hyp_year[100], hyp_month[100], hyp_day[100], hyp_hour[100], hyp_min[100];
      float hyp_sec[100], hyp_lat[100], hyp_lon[100], hyp_depth[100];
      int hyp_nstat[100];
      float hyp_rms[100], hyp_mag[100][6], hyp_mag_all[200], hyp_gap[100];
      float hyp_sec_err[100], hyp_lat_err[100], hyp_lon_err[100];
      float hyp_depth_err[100], hyp_cov[100][3];
      char hyp_auto[100][20];
   } hyp4_ ;

   extern struct comblck_hyp5 {
      char hyp_agency[100][5], hyp_mag_agency[100][6][5], hyp_mag_agency_all[200][5];
   } hyp5_ ;

   extern struct comblck_rea1 {
      char rea_weight_in[MAX_DATA/2], rea_onset[MAX_DATA/2], rea_polarity[MAX_DATA/2];
   } rea1_ ;

   extern struct comblck_rea2 {
      char rea_co[MAX_DATA/2][2], rea_weight_out[MAX_DATA/2][2];
   } rea2_ ;

   extern struct comblck_rea3 {
      char rea_action[3];
   } rea3_ ;

   extern struct comblck_rea4 {
      char rea_comp[MAX_DATA/2][4];
      int rea_hour[MAX_DATA/2], rea_min[MAX_DATA/2];
      float rea_sec[MAX_DATA/2], rea_coda[MAX_DATA/2], rea_amp[MAX_DATA/2];
      float rea_per[MAX_DATA/2], rea_baz_obs[MAX_DATA/2], rea_baz_cal[MAX_DATA/2];
      float rea_vel[MAX_DATA/2], rea_ain[MAX_DATA/2], rea_baz_res[MAX_DATA/2];
      float rea_dist[MAX_DATA/2], rea_az[MAX_DATA/2];
      int rea_nstat, rea_nphase;
      float rea_res[MAX_DATA/2];
      int rea_year[MAX_DATA/2], rea_month[MAX_DATA/2], rea_day[MAX_DATA/2];
      float rea_moment[MAX_DATA/2];
      int rea_nmag;
      float rea_sdrop[MAX_DATA/2], rea_omega0[MAX_DATA/2], rea_cornerf[MAX_DATA/2];
      float rea_radius[MAX_DATA/2], rea_swin[MAX_DATA/2], rea_vs[MAX_DATA/2];
      float rea_vp[MAX_DATA/2], rea_q_below_1hz[MAX_DATA/2], rea_q0[MAX_DATA/2];
      float rea_qalpha[MAX_DATA/2],rea_kappa[MAX_DATA/2], rea_density[MAX_DATA/2];
      float rea_slope[MAX_DATA/2], rea_mc[MAX_DATA/2], rea_ml[MAX_DATA/2];
      float rea_mb[MAX_DATA/2], rea_ms[MAX_DATA/2], rea_mw[MAX_DATA/2];
      float rea_geo_dist[MAX_DATA/2];
      int rea_nhead, rea_nrecord, rea_nspec, rea_nhyp;
      char rea_id_line[80];
      int rea_nmacro, rea_nwav, rea_nfault, rea_ncomment;
      char rea_macro[100][80], rea_wav[500][80], rea_fault[100][80], rea_comment[1000][80];
      float rea_av_moment, rea_av_sdrop, rea_av_omega0, rea_av_cornerf;
      float rea_av_radius, rea_av_swin, rea_av_mw, rea_av_slope;
      char rea_auto[MAX_DATA/2][20];
      int rea_err_unit, rea_read_err, rea_write_err;
      char rea_locality[68];
   } rea4_ ;

   extern struct comblck_rea5 {
      char rea_stat[MAX_DATA/2][5];
   } rea5_ ;

   extern struct comblck_rea8 {
      char rea_phase[MAX_DATA/2][8];
      double rea_abs_time[MAX_DATA/2];
   } rea8_ ;

   extern struct comblck_rea9 {
      char rea_phase_cal[MAX_DATA/2][8];
      char rea_mag_type[MAX_DATA/2][2];
      float rea_time_obs[MAX_DATA/2], rea_time_cal[MAX_DATA/2];
      float rea_mag[MAX_DATA/2], rea_mag_res[MAX_DATA/2];
      float rea_wt[MAX_DATA/2], rea_baz_wt[MAX_DATA/2];
      float hyp_dx,hyp_dy,hyp_dz,hyp_do;
      int rea_di[MAX_DATA/2],rea_baz_di[MAX_DATA/2];
   } rea9_ ;

}




//
// Structures for holding the event file in memory.
//
struct db_time {
   int year;
   int month;
   int day;
   int hour;
   int minute;
   float second;
};

struct text_lines {
   int numlines;                       // Number of lines in the 'lines' string.
   char *lines;                        // Pointer to string with lines. All lines are 80 characters long.
};                                     // The lines in the string are NOT seperated by any character.

struct spectral_ {
   float moment;                       // Log moment, Nm
   float sdrop;                        // Stress drop, bar
   float omega0;                       // Log spectral flat level, ns
   float cornerf;                      // Corner f
   float radius;                       // Source radius
   float swin;                         // Window lenght used
   float vs;                           // S-velocity at source, km/s
   float vp;                           // P-velocity at source, km/s
   float q0;                           // Q0
   float qalpha;                       // Q alpha
   float kappa;                        // Kappa
   float density;                      // Density g/cm**3
   float slope;                        // Measured slope of spectrum
   float geo_dist;                     // Geo distance
};

struct magn_ {
   float mc;                           // Coda
   float ml;                           // Local
   float mb;                           // mb
   float ms;                           // ms
   float mw;                           // mw
};

struct phase_ {
   // Phase data parameters.
   char agency[6];                     // Agency
   char comp[5];                       // Components
   char co[3];                         // 2 letter components
   char phase[9];                      // Phase name
   char onset;                         // Onset I or E or blank
   char weight_in;                     // Input weight
   char weight_out[3];                 // Weight out
   char polarity;                      // Polarity, D or C
   int year;                           // Date and time.
   int month;
   int day;
   int hour;
   int min;
   float sec;
   double abs_time;                    // Abs time of phase time
   float coda;                         // Coda length in s
   float amp;                          // Amplitude (in Nm)
   float per;                          // Period of amplitude
   float baz_obs;                      // Observed back azimuth
   float baz_cal;                      // Calculated back azimuth
   float vel;                          // Observed apparent velocity
   float ain;                          // Observed signal to noise ratio
   float baz_res;                      // Back azimuth residual
   float res;                          // Travel time residual
   float dist;                         // Epicentral distance
   float az;                           // Azimuth
   char autoproc[21];                  // Name of auto process for making par.
   struct spectral_ spectral;          // Spectral data for phase.
   struct magn_ magnitudes;            // Magnitude data for phase.
   struct phase_ *next;                // Pointer to the next phase in linked list.
};

struct phases_ {
   int nphase;                         // Number of phases.
   struct phase_ *first;               // Pointer to first phase in list.
};

struct mag_info {
   float mag;                          // Magnitude value.
   char type;                          // Magnitude type.
   char agency[6];                     // Magnitude agency.
};

struct hyp_mag_type {
   char nmag;                          // Number of magnitudes in arrays.
   float mag[6];                       // Magnitude
   char mag_agency[6][6];              // Agency
};

struct hyp_magnitudes_ {
   struct hyp_mag_type MW;
   struct hyp_mag_type ML;
   struct hyp_mag_type MC;
   struct hyp_mag_type Mb;
   struct hyp_mag_type MB;
   struct hyp_mag_type Ms;
   struct hyp_mag_type MS;
};

struct hypocenter_ {
   struct db_time time;                // Hypocenter date and time.
   char model;                         // Location model indicator
   char dist_id;                       // Distance indicator
   char type;                          // Event type like E
   char fix_org;                       // Fix origin time flag
   float lat;                          // Latitude
   float lon;                          // Longitude
   float depth;                        // Depth
   char depth_flag;                    // Depth flag
   char epi_flag;                      // Epicenter flag
   char agency[6];                     // Hypocenter agency, 3 characters only.
   int nstat;                          // Number of stations
   float rms;                          // Rms of hypocenter solution
   char high_accuracy;                 // High accurcy flag
   char error;                         // True if hypocenter has an error
   char autoproc[21];                  // Name of auto process for parameter
   float gap;                          // Gap, degrees
   float sec_err;                      // Origin time error (sec)
   float lat_err;                      // Latitude error (km)
   float lon_err;                      // Longitude error (km)
   float depth_err;                    // Depth error (km)
   float cov[3];                       // Covariance, xy,xz,yz (kmXkm) (Allways 3 values here)
   struct hyp_magnitudes_ magnitudes;  // Magnitudes
   struct hypocenter_ *next;           // Pointer to the next hypocenter.
};

struct hypocenters_ {
   int nhyp;                           // Number of hypocenters.
   int nmag;                           // Number of magnitudes
   mag_info *mag_all;                  // All magnitudes. Pointer to array of mag_info structures.
   struct hypocenter_ *first;          // Pointer to first hypocenter in list.
};

struct spec_avg_ {
   float av_moment;                    // Log moment (Nm)
   float av_sdrop;                     // Stress drop, bar
   float av_omega0;                    // Log spectral flat level, ns
   float av_cornerf;                   // Corner f
   float av_radius;                    // Source radius
   float av_swin;                      // Window lenght used
   float av_mw;                        // Moment mag
   float av_slope;                     // Slope
};

struct event_node_ {
   int nstat;                          // Number of stations
   int nhead;                          // Number of header lines
   int nrecord;                        // Number of records
   int nspec;                          // Number of spectra
   char id_line[81];                   // Event ID line
   char action[4];                     // Action parameter from ID line.
   char locality[69];                  // Locality.
   struct text_lines macros;           // Structure holding all macro data.
   struct text_lines wavefiles;        // Structure holding all wave filenames.
   struct text_lines faults;           // Structure holding all fault lines.
   struct text_lines comments;         // Structure holding all comment lines.
   struct phases_ phases;              // Structure holding all phase data.
   struct spec_avg_ spectral_avg;      // Structure holding spectral averages.
   struct hypocenters_ hypocenters;    // Structure holding all hypcocenter data.
};




//
// Declaration of support functions.
//
void FreeNode(event_node_*);
event_node_* CreateNodeFromCommonBlocks();
void ClearCommonBlocks();
bool IsDataLineEmpty(char[], int);
void UnPadString(char*, int);
void fstrcpy(char[], char*, int);




//
// Definition of the ReadSfile class.
//
class ReadSfile
{
public:
   ReadSfile();
   bool OpenFile(char[]);
   void CloseFile();
   event_node_ *GetNextEvent(int*);

private:
   FILE *Sfile;
   bool FileOpen, EndOfFile;
};




//
// Definition of the WriteSfile class.
//
class WriteSfile
{
public:
   WriteSfile();
   bool OpenFile(char[]);
   void CloseFile();
   bool WriteEvent(event_node_*, int*);

private:
   FILE *Sfile;
   bool FileOpen;
};


#endif // REA_C_H

