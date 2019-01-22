/***************************************************************************
 * mscut.c
 *  
 * Another simple example of using libmseed.
 *
 * cut a mseed file into one hour files or 15 min files
 * used to cut seiscomp archive files
 *
 * original is the msview Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 * 
 * peter voss geological survey of denmark and greenland 2008-02-28
 *
 * win32 part is not tested !!
 *
 * changes : 
 * 2008-04-17 pv: changed variable "outfile" to fix linux compilation problem
 * 2014-04-14 pv: cleanup warning
 * 2016-12-09 pv: added -D
 *
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#ifndef WIN32
  #include <signal.h>
  static void term_handler (int sig);
#endif

#include <libmseed.h>

#define VERSION "[libmseed " LIBMSEED_VERSION " example]"
#define PACKAGE "mscut"

static short int verbose   = 0;
static short int ppackets  = 0;
static short int basicsum  = 0;
static short int opennewfile = 1;
static short int closeoldfile = 0;
static short int hourwindow    = 1;
static short int quarterwindow = 0;
static short int windowlength = 15;
static int   reclen        = 0;
static char *inputfile     = 0;
static char outfile[256];

static int parameter_proc (int argcount, char **argvec);
static void usage (void);
static void term_handler (int sig);

int
main (int argc, char **argv)
{
  MSRecord *msr = 0;

  int dataflag   = 0;
  int totalrecs  = 0;
  int totalsamps = 0;
  int retcode;

char time[25];
char timeoldfile[7]="--";
char timenewfile[7]="--";
char srcname[100];
int isec;
int  ifract;
hptime_t hptime;
time_t tsec;
struct tm *tm;
FILE *ofp = 0;


/* the win32 part is not checked !!! peter voss */
#ifndef WIN32
  /* Signal handling, use POSIX calls with standardized semantics */
  struct sigaction sa;

  sa.sa_flags = SA_RESTART;
  sigemptyset (&sa.sa_mask);

  sa.sa_handler = term_handler;
  sigaction (SIGINT, &sa, NULL);
  sigaction (SIGQUIT, &sa, NULL);
  sigaction (SIGTERM, &sa, NULL);

  sa.sa_handler = SIG_IGN;
  sigaction (SIGHUP, &sa, NULL);
  sigaction (SIGPIPE, &sa, NULL);
#endif

  /* Process given parameters (command line and parameter file) */
  if (parameter_proc (argc, argv) < 0)
    return -1;
      
  /* Loop over the input file */
  while ( (retcode = ms_readmsr (&msr, inputfile, reclen, NULL, NULL, 1,
                                 dataflag, verbose)) == MS_NOERROR )
    {      
  ms_hptime2seedtimestr (msr->starttime, time, 1);
  isec = MS_HPTIME2EPOCH(msr->starttime);

  ifract = msr->starttime - (isec * HPTMODULUS);
  /* Adjust for negative epoch times */
  if ( msr->starttime < 0 && ifract != 0 )
    {
      isec -= 1;
      ifract = HPTMODULUS - (-ifract);
    }
  tsec = (time_t) isec;
  if ( ! (tm = gmtime ( &tsec )) )
    return 0;
//  return NULL;

      totalrecs++;
      totalsamps += msr->samplecnt;

//      printf ("%s\n", time);
//      printf ("%02d %02d\n", tm->tm_hour, tm->tm_min);
//      printf ("%02d:%02d:%02d.%06d\n", tm->tm_hour, tm->tm_min, tm->tm_sec, ifract); 

      if ( quarterwindow ) {
//      if ( tm->tm_min==0 || tm->tm_min==15 || tm->tm_min==30 || tm->tm_min==45) { 
//      if ( tm->tm_min%15==0 ) { 
        if ( tm->tm_min%windowlength==0 ) { 
          sprintf (timenewfile,"%02d%02d", tm->tm_hour, tm->tm_min);
          if(strcmp(timenewfile,timeoldfile)!=0) {
            strcpy(timeoldfile,timenewfile);
            opennewfile=1;
          }
        }
      }
      if ( hourwindow ) {
        sprintf (timenewfile,"%02d", tm->tm_hour);
        if(strcmp(timenewfile,timeoldfile)!=0) {
          strcpy(timeoldfile,timenewfile);
          opennewfile=1;
        }
      }

    /* open the output file */
    if ( opennewfile ) {
    if(closeoldfile) fclose(ofp);
    opennewfile=0;
    /* name of new file: */
    sprintf (outfile,"%4d-%02d-%02d-%02d%02d-%02d.",tm->tm_year+1900,tm->tm_mon+1,tm->tm_mday,tm->tm_hour,tm->tm_min,tm->tm_sec);
    msr_srcname (msr, srcname, 0);
    strcat(outfile,srcname);
    printf("new file : %s\n",outfile);
    if ((ofp=fopen(outfile,"wb"))==NULL)
      {
        fprintf(stderr,"Cannot open output file : %s\n",outfile);
        return -1;
      }
    else closeoldfile=1;
    }

    fwrite(msr->record,1,msr->reclen,ofp);
    } /* end of Loop over the input file */
  
  fclose(ofp);
  /* Make sure everything is cleaned up */
  ms_readmsr (&msr, NULL, 0, NULL, NULL, 0, 0, 0);
  
  if ( basicsum )
    printf ("Records: %d, Samples: %d\n", totalrecs, totalsamps);
  
  return 0;
}  /* End of main() */


/***************************************************************************
 * parameter_proc():
 * Process the command line parameters.
 *
 * Returns 0 on success, and -1 on failure
 ***************************************************************************/
static int
parameter_proc (int argcount, char **argvec)
{
  int optind;

  /* Process all command line arguments */
  for (optind = 1; optind < argcount; optind++)
    {
      if (strcmp (argvec[optind], "-V") == 0)
	{
	  fprintf (stderr, "%s version: %s\n", PACKAGE, VERSION);
	  exit (0);
	}
      else if (strcmp (argvec[optind], "-h") == 0)
	{
	  usage();
	  exit (0);
	}
      else if (strncmp (argvec[optind], "-v", 2) == 0)
	{
	  verbose += strspn (&argvec[optind][1], "v");
	}
      else if (strncmp (argvec[optind], "-p", 2) == 0)
	{
	  ppackets += strspn (&argvec[optind][1], "p");
	}
      else if (strcmp (argvec[optind], "-s") == 0)
	{
	  basicsum = 1;
	}
      else if (strcmp (argvec[optind], "-H") == 0)
	{
	   hourwindow = 1 ;
	}
      else if (strcmp (argvec[optind], "-Q") == 0)
	{
	   hourwindow = 0 ;
	   quarterwindow = 1 ;
	}
      else if (strcmp (argvec[optind], "-D") == 0)
	{
	   windowlength = atoi (argvec[++optind]);
	   hourwindow = 0 ;
	   quarterwindow = 1 ;
	   if ( 60%windowlength != 0 ) {
	      fprintf(stderr, "Error: 60 modulo the duration (%d) must be zero!\n",windowlength);
	      exit (1);
	   }
	}
      else if (strcmp (argvec[optind], "-r") == 0)
	{
	  reclen = atoi (argvec[++optind]);
	}
      else if (strncmp (argvec[optind], "-", 1) == 0 &&
	       strlen (argvec[optind]) > 1 )
	{
	  fprintf(stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
	}
      else if ( inputfile == 0 )
	{
	  inputfile = argvec[optind];
	}
      else
	{
	  fprintf(stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
	}
    }

  /* Make sure an inputfile was specified */
  if ( ! inputfile )
    {
      fprintf (stderr, "No input file was specified\n\n");
      fprintf (stderr, "%s version %s\n\n", PACKAGE, VERSION);
      fprintf (stderr, "Try %s -h for usage\n", PACKAGE);
      exit (1);
    }

  /* Report the program version */
  if ( verbose )
    printf ("%s version: %s\n", PACKAGE, VERSION);

  return 0;
}  /* End of parameter_proc() */


/***************************************************************************
 * usage():
 * Print the usage message and exit.
 ***************************************************************************/
static void
usage (void)
{
  fprintf (stderr, "%s version: %s\n\n", PACKAGE, VERSION);
  fprintf (stderr, "Usage: %s [options] file\n\n", PACKAGE);
  fprintf (stderr,
	   " ## Options ##\n"
	   " -H             Cut into one hour files (default)\n"
	   " -Q             Cut into 15 min files\n"
	   " -D minutes     Cut into files with a duration defined by minutes,\n"
	   "                60 modulo the number of minutes must be zero.\n"
	   " -V             Report program version\n"
	   " -h             Show this usage message\n"
	   " -v             Be more verbose, multiple flags can be used\n"
	   " -p             Print details of header, multiple flags can be used\n"
	   " -s             Print a basic summary after processing a file\n"
	   " -r bytes       Specify record length in bytes, required if no Blockette 1000\n"
	   "\n"
	   " file           File of Mini-SEED records\n"
	   "\n");
}  /* End of usage() */


#ifndef WIN32
/***************************************************************************
 * term_handler:
 * Signal handler routine. 
 ***************************************************************************/
static void
term_handler (int sig)
{
  exit (0);
}
#endif
