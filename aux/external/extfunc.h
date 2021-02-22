
/* The following definitions and structures will be used to pass data into */
/* and out of external functions specified by the user, which will be      */
/* loaded at run-time.                                                     */
/* The calling sequence for the external functions will be:                */
/*                                                                         */
/* long extfunc(argc, argv, call_data, update)                             */
/* int argc;                                                               */
/* char **argv;                                                            */
/* sac_files *call_data;                                                   */
/* long      *update;                                                      */
/*                                                                         */
/* To create shared objects from which SAC will load the external functions*/
/* do:                                                                     */
/*                                                                         */
/* cc -o libxxx.so -G extern.c                                             */
/*                                                                         */
/* This will create a shared object named libxxx.so, which will contain    */
/* the external function.  Multiple external functions can be stored in a  */
/* shared object.  By default, SAC will look for a shared object named     */
/* libsac.so.  You can override this default behavior by setting           */
/* environmental variable SACSOLIST to the name or names of your shared    */
/* objects.  For example, if you have two libraries of shared objects,     */
/* libabc.so and libdef.so you can:                                        */
/*                                                                         */
/* setenv SACSOLIST "libabc.so libdef.so"                                  */
/*                                                                         */
/* After this, SAC will look in those two s.o.'s for any external commands */
/* that you ask to have loaded.  One additional complication, is that the  */
/* LD_LIBRARY_PATH environmental variable is used to determine which       */
/* directories to search for the shared objects.                           */
/*                                                                         */
/* To load an external command into SAC, use the load command.             */
/*                                                                         */
/* SAC> load extfunc                                                       */
/*                                                                         */
/* where extfunc is the name of a function contained in your shared object.*/

#define MFHDR 70
#define MNHDR 15
#define MIHDR 20
#define MLHDR  5
#define MKHDR 24

#define MVHDRC 6             /* SAC file version                        */

/* update flag values */
#define IGNORE  0
#define APPEND  1
#define REPLACE 2

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

typedef struct {
  float ext_fhdr[MFHDR];     /* floating point header fields            */
  int  ext_nhdr[MNHDR];     /* integer (int) header fields            */
  int  ext_ihdr[MIHDR];     /* enumerated header fields                */
  int  ext_lhdr[MLHDR];     /* logical header fields                   */

  char  ext_khdr[MKHDR] [9]; /* character header fields                 */
} sac_header;


typedef struct {
  int nfiles;               /* number of files                         */

  sac_header   **ext_hdrs;   /* array of header struct, one per file    */

  float     **ext_yvalues;   /* 2-d array of y values, there are nfiles */
                             /* vectors of values, each vector is npts  */
                             /* int.  So, the vectors may not all be   */
                             /* the same length.                        */
  
  float     **ext_xvalues;   /* 2-d array of x values, similar to the y */
                             /* data described above, but, with the     */
                             /* additional complication that for evenly */
                             /* spaced data, no x data is actually      */
                             /* passed.  In this (evenly spaced) case   */
                             /* x values are calculated based upon      */
                             /* header fields begin (or b), ennd (or e) */
                             /* and delta.  In the case where no x data */
                             /* is present, the pointer will be set to  */
                             /* the NULL value.                         */
} sac_files;

  
sac_header *makehdr();
/* sac_header *makehdr( sac_header ); */

int getehdr();
/* int getehdr(sac_header *header, char *fieldname, int *error); */

void setehdr();
/* void setehdr(sac_header *header, char *fieldname, int value, int *error); */

float getfhdr();
/* float getfhdr(sac_header *header, char *fieldname, int *error); */

void setfhdr();
/* void setfhdr(sac_header *header, char *fieldname, float value, int *error); */

int getnhdr();
/* int getnhdr(sac_header *header, char *fieldname, int *error); */

void setnhdr();
/* void setnhdr(sac_header *header, char *fieldname, int value, int *error); */

int getlhdr();
/* int getlhdr(sac_header *header, char *fieldname, int *error); */

void setlhdr();
/* void setlhdr(sac_header *header, char *fieldname, int value, int *error); */

char *getahdr();
/* char *getahdr(sac_header *header, char *fieldname, int *error); */
/* This functions returns a pointer to the value in the actual header field. */
/* You should make a copy of it before modifying it.  And you should not     */
/* free this returned address.                                               */

void setahdr();
/* void setahdr(sac_header *header, char *fieldname, char *value, int *error); */


/* tables of header names */

/* floating point fields */
/* mag (magnitude) added. maf 970205 */
static char *float_hdr_fields[MFHDR] = { "delta"   , "depmin"  , "depmax"  , "scale" , "odelta"  ,
                                         "b"       , "e"       , "o"       , "a"     , "fmt"     ,
                                         "t0"      , "t1"      , "t2"      , "t3"    , "t4"      ,
                                         "t5"      , "t6"      , "t7"      , "t8"    , "t9"      ,
                                         "f"       , "resp0"   , "resp1"   , "resp2" , "resp3"   ,
                                         "resp4"   , "resp5"   , "resp6"   , "resp7" , "resp8"   ,
                                         "resp9"   , "stla"    , "stlo"    , "stel"  , "stdp"    ,
                                         "evla"    , "evlo"    , "evel"    , "evdp"  , "mag"     ,
                                         "user0"   , "user1"   , "user2"   , "user3" , "user4"   ,
                                         "user5"   , "user6"   , "user7"   , "user8" , "user9"   ,
                                         "dist"    , "az"      , "baz"     , "gcarc" , "sb"      ,
                                         "sdelta"  , "depmen"  , "cmpaz"   , "cmpinc", "xminimum",
                                         "xmaximum", "yminimum", "ymaximum", "adjtm" , "fhdr65"  ,
                                         "fhdr66"  , "fhdr67"  , "fhdr68"  , "fhdr69", "fhdr70"  
};

/* int header fields */  /* Note: ninf, nhst, and nsn were changed to norid, nevid, and nwfid
				   for compatability with the CSS format.  maf 961031 */
static char *int_hdr_fields[MNHDR] = {  "nzyear"  , "nzjday"  , "nzhour"  , "nzmin" , "nzsec"   ,
                                         "nzmsec"  , "nvhdr"   , "norid"   , "nevid" , "npts"    ,
                                         "nsnpts"  , "nwfid"   , "nxsize"  , "nysize", "nhdr15"
};

/* enumerated header fields */
/* imagtyp and imagsrc (magnitude type and source) added.  maf 970205 */
static char *enum_hdr_fields[MIHDR] = {  "iftype"  , "idep"    , "iztype"  , "ihdr4" , "iinst"   ,
                                         "istreg"  , "ievreg"  , "ievtyp"  , "iqual" , "isynth"  ,
                                         "imagtyp" , "imagsrc" , "ihdr13"  , "ihdr14", "ihdr15"  ,
                                         "ihdr16"  , "ihdr17"  , "ihdr18"  , "ihdr19", "ihdr20"
};

/* logical header fields */
static char *log_hdr_fields[MLHDR] =  {  "leven"   , "lpspol"  , "lovrok"  , "lcalda", "lhdr5"    };

/* character header fields */
/* note:  "kevnm" is 17 characters int. All other fields are eight chars int. */
static char *char_hdr_fields[MKHDR]=  {  "kstnm"   , "kevnm"   , " "       , "khole"   , "ko"      ,
                                         "ka"      , "kt0"     , "kt1"     , "kt2"     , "kt3"     ,
                                         "kt4"     , "kt5"     , "kt6"     , "kt7"     , "kt8"     ,
                                         "kt9"     , "kf"      , "kuser0"  , "kuser1"  , "kuser2"  ,
                                         "kcmpnm"  , "knetwk"  , "kdatrd"  , "kinst"
};    

/* enumerated header values */
#define ITIME   1  /* Time series file            */
#define IRLIM   2  /* Spectral file-real/imag     */
#define IAMPH   3  /* Spectral file-ampl/phase    */
#define IXY     4  /* General x vs y file         */
#define IUNKN   5  /* Unknown                     */
#define IDISP   6  /* Displacement (NM)           */
#define IVEL    7  /* Velocity (NM/SEC)           */
#define IACC    8  /* Acceleration (NM/SEC/SEC)   */
#define IB      9  /* Begin time                  */
#define IDAY   10  /* GMT day                     */
#define IO     11  /* Event origin time           */
#define IA     12  /* First arrival time          */
#define IT0    13  /* User defined time pick 0    */
#define IT1    14  /* User defined time pick 1    */
#define IT2    15  /* User defined time pick 2    */
#define IT3    16  /* User defined time pick 3    */
#define IT4    17  /* User defined time pick 4    */
#define IT5    18  /* User defined time pick 5    */
#define IT6    19  /* User defined time pick 6    */
#define IT7    20  /* User defined time pick 7    */
#define IT8    21  /* User defined time pick 8    */
#define IT9    22  /* User defined time pick 9    */
#define IRADNV 23  /* Radial (NTS)                */
#define ITANNV 24  /* Tangential (NTS)            */
#define IRADEV 25  /* Radial (EVENT)              */
#define ITANEV 26  /* Tangential (EVENT)          */
#define INORTH 27  /* North positive              */
#define IEAST  28  /* East positive               */
#define IHORZA 29  /* Horizontal (ARB)            */
#define IDOWN  30  /* Down positive               */
#define IUP    31  /* Up positive                 */
#define ILLLBB 32  /* LLL broadband               */
#define IWWSN1 33  /* WWSN 15-100                 */
#define IWWSN2 34  /* WWSN 30-100                 */
#define IHGLP  35  /* High-gain int-period       */
#define ISRO   36  /* SRO                         */
#define INUCL  37  /* Nuclear event               */
#define IPREN  38  /* Nuclear pre-shot event      */
#define IPOSTN 39  /* Nuclear post-shot event     */
#define IQUAKE 40  /* Earthquake                  */
#define IPREQ  41  /* Foreshock                   */
#define IPOSTQ 42  /* Aftershock                  */
#define ICHEM  43  /* Chemical explosion          */
#define IOTHER 44  /* Other                       */
#define IGOOD  45  /* Good                        */
#define IGLCH  46  /* Gliches                     */
#define IDROP  47  /* Dropouts                    */
#define ILOWSN 48  /* Low signal to noise ratio   */
#define IRLDTA 49  /* Real data                   */
#define IVOLTS 50  /* Velocity (volts)            */
#define IXYZ   51  /* General XYZ (3-D) file      */
#define IMB    52  /* Bodywave Magnitude */ /* These 19 added to describe */
#define IMS    53  /* Surface Magnitude */ /* magnitude type and source. */
#define IML    54  /* Local Magnitude  */ /* maf 970205 */
#define IMW    55  /* Moment Magnitude */
#define IMD    56  /* Duration Magnitude */
#define IMX    57  /* User Defined Magnitude */
#define INEIC  58  /* INEIC */
#define IPDE   59  /* IPDE */
#define IISC   60  /* IISC */
#define IREB   61  /* IREB */
#define IUSGS  62  /* IUSGS */
#define IBRK   63  /* IBRK */
#define ICALTECH 64/* ICALTECH */
#define ILLNL  65  /* ILLNL */
#define IEVLOC 66  /* IEVLOC */
#define IJSOP  67  /* IJSOP */
#define IRAB   68  /* IRAB */
#define IUSER  69  /* IUSER */
#define IUNKNOWN 70/* IUNKNOWN */
/* These 17 added for ievtyp. maf 970325 */
#define IQB	71/* Quarry or mine blast confirmed by quarry */
#define IQB1	72/* Quarry or mine blast with designed shot information-ripple fired */
#define IQB2	73/* Quarry or mine blast with observed shot information-ripple fired */
#define IQBX    74/* Quarry or mine blast - single shot */
#define IQMT    75/* Quarry or mining-induced events: tremors and rockbursts */
#define IEQ     76/* Earthquake */
#define IEQ1    77/* Earthquakes in a swarm or aftershock sequence */
#define IEQ2    78/* Felt earthquake */
#define IME     79/* Marine explosion */
#define IEX	80/* Other explosion */
#define INU	81/* Nuclear explosion */
#define INC	82/* Nuclear cavity collapse */
#define IO_	83/* Other source of known origin */
#define IL	84/* Local event of unknown origin */
#define IR	85/* Regional event of unknown origin */
#define IT	86/* Teleseismic event of unknown origin */
#define IU	87/* Undetermined or conflicting information  */

#define MINENUM  1
#define MAXENUM 51


/* undefined values */
#define FUNDEF (-12345.)    /* undefined value for float fields          */
#define IUNDEF (-12345)     /* undefined value for enumerated fields     */
#define NUNDEF (-12345)     /* undefined value for integer (int) fields */
#define AUNDEF "-12345  "   /* undefined value for character fields      */

/**************   FORTRAN interface support   ****************************/

sac_files *indata;  /* Global variable used by FORTRAN header access    */
                     /* functions.                                       */

void fgetahdr_();
/* void fgetahdr_(int *hdr_index, char *fieldname, char *value,
                  int *error, int lenfield, int lenvalue)             */

/* FORTRAN calling sequence:
   call fgetahdr(integer*4 hdr_index, character fieldname, character value, integer*4 error)
   
   The last two arguments in the C language spec. (lenfield and lenvalue)
   are passed automatically by the FORTRAN compiler.  The calling program
   SHOULD NOT include these in its call.                                 */


void fsetahdr_();
/* void fsetahdr_(int *hdr_index, char *fieldname, char *value,
                  int *error, int lenfield, int lenvalue)             */

/* FORTRAN calling sequence:
   call fsetahdr(integer*4 hdr_index, character fieldname, character value, integer*4 error) */

void fgetehdr_();
/* void fgetehdr_(int *hdr_index, char *fieldname, int *value, 
                  int *error, int lenfield)                            */

/* FORTRAN calling sequence:
   call fgetehdr(integer*4 hdr_index, character fieldname, integer*4 value, integer*4 error) */

void fsetehdr_();
/* void fsetehdr_(int *hdr_index, char *fieldname, int *value,
                  int *error, int lenfield)                            */

/* FORTRAN calling sequence:
   call fsetehdr(integer*4 hdr_index, character fieldname, integer*4 value, integer*4 error) */

void fgetfhdr_();
/* void fgetfhdr_(int *hdr_index, char *fieldname, float *value,
                  int *error, int lenfield)                            */

/* FORTRAN calling sequence:
   call fgetfhdr(integer*4 hdr_index, character fieldname, real*4 value, integer*4 error)    */

void fsetfhdr_();
/* void fsetfhdr_(int *hdr_index, char *fieldname, float *value,
                  int *error, int lenfield)                             */

/* FORTRAN calling sequence:
   call fsetfhdr(integer*4 hdr_index, character fieldname, real*4 value, integer*4 error)     */

void fgetlhdr_();
/* void fgetlhdr_(int *hdr_index, char *fieldname, int *value,
                  int *error, int lenfield)                              */

/* FORTRAN calling sequence:
   call fgetlhdr(integer*4 hdr_index, character fieldname, integer*4 value, integer*4 error)  */

void fsetlhdr_();
/* void fsetlhdr_(int *hdr_index, char *fieldname, int *value,
                  int *error, int lenfield)                              */

/* FORTRAN calling sequence:
   call fsetlhdr(integer*4 hdr_index, character fieldname, integer*4 value, integer*4 error)  */

void fgetnhdr_();
/* void fgetnhdr_(int *hdr_index, char *fieldname, int *value,
                  int *error, int lenfield)                               */

/* FORTRAN calling sequence:
   call fgetnhdr(integer*4 hdr_index, character fieldname, integer*4 value, integer*4 error)  */

void fsetnhdr_();
/* void fsetnhdr_(int *hdr_index, char *fieldname, int *value,
                  int *error, int lenfield)                                */

/* FORTRAN calling sequence:
   call fsetnhdr(integer*4 hdr_index, character fieldname, integer*4 value, integer*4 error)  */















