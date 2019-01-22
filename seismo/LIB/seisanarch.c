/*
 * SEISANARCH
 * Author: R. Luis
 * Purpose: c functions for accessing mini-seed archives containing dayfiles
 * Last Update: 2014.05.20
 * 
 * Main Functions:
 * getarchdata_ - Populates an array with samples for a given request
 * getarchinfo_ - Computes the number of samples that may be extracted by getarchdata_
 *
 * Notes:
 * These functions are prepared to work only with archives of with the following structures:
 * Buffer of Uniform Data (BUD) - <BUDdir>/<NET>/<STA>/STA.NET.LOC.CHAN.YEAR.DAY
 * SeisComP Data Structure (SDS) - <SDSdir>/<YEAR>/<NET>/<STA>/<CHAN.TYPE>/NET.STA.LOC.CHAN.TYPE.YEAR.DAY
 *
 * History of Changes:
 * 2015.09.11 lo fix to getMSTG
 *
 * 2014.03.10 peter voss
 *   change error message if the is no data file quality D,M,R or E
 * 2013.09.11 peter voss
 *   Added a search for SCP files with data quality M,R or E, if D isnt present.
 * 2013.01.31
 *   Changed skipsample counter from int to long in the copySamples function.
 *   This solves limitation of integer number when the number of samples to skip
 *   is too high.
 * 2011.05.11
 *   Fixed error in getMSTG when computing endtime of long mseed records
 * 2011.02.02
 *   Fixed error in the generation of the archive file names
 * 2011.01.13
 *   Fixed new bug with missing samples on the start of the data output
 * 2011.01.07
 *   Fixed bug with filenames. There is still some confusion on the conversion from fortran to c
 * 2010.12.22
 *   Fixed bug with seconds and microseconds on starttime.
 *   Fixed bug with filenames for start and end times.
 * 2010.11.15
 *   Removed BUD-specific function to compute filename and added generic naming funcion
 *   This allows usage of SDS and BUD. Eventually other structures may be added.
 * 2010.10.14
 *   First version
 *   Reads only from BUD structures, requires adaptation for SDS as well.
 *
 * List of TODOs
 * - Include bissection algorithm for searching data, current algorithm is slow at end of file
 * - Allow more generic search with more than two files for eventually adding SEISAN databases
 * - Perform data search without loading data. Might be even faster.
 *
 */




#include <libmseed.h>
#include <stdio.h>
#include <string.h>




/* Declarations */
char *trim_string( char *original, int nchar );
char *getArchFilename( hptime_t time, char *filename, char *af, char *sta, char *chan, char quality, char *net, char *loc, int *archiveType );
void getMSTG( hptime_t starttime, hptime_t endtime, char *file1, char *file2, MSTraceGroup *mstg, int dataflag );
int copySamples( MSTraceGroup *mstg, int *curSample, hptime_t *starttime, hptime_t *endtime );


/* Globals */
static int seisanArchDebug = 0;
static char FileFlags[] = { 'D', 'M', 'R', 'E', '?' };
static int nFileFlags = 5;

/*
getarchinfo
Return base data from a BUD archive
If data for the requested station and time interval is unavailable, returns zero number of samples. 
 */
void getarchinfo_( archiveFolder, archiveFolderLen, archiveType, stat, statlen, chan, chanlen, net, netlen, loc, loclen, year, month, day, hour, min, sec, t_interval,
	nsamp, samprate, t_error, nyear, nmonth, nday, nhour, nmin, nsec )
//Input arguments
char *archiveFolder; //Name of the base folder of the archive
int *archiveFolderLen; //Length of the archive folder name
int *archiveType; //Integer indicating the type of archive to access
//   0 - BUD Archive
//   1 - SeisComP Archive
char *stat; //station name
int *statlen; //station name length
char *chan; //channel name
int *chanlen; //channel name length
char *net; //network name
int *netlen; //network name length
char *loc; //location name
int *loclen; //location name length
int *year; //start time year 
int *month; //start time month
int *day; //start time day of month
int *hour; //start time hour
int *min; //start time minute
float *sec; //start time second
int *t_interval; //Requested time interval
//Output arguments
int *nsamp; //Number of available samples for the requested interval
float *samprate; //Sampling rate
int *t_error; //Timing error
int *nyear;
int *nmonth;
int *nday;
int *nhour;
int *nmin;
float *nsec;
{
	//Set default output parameters in case no data is found
	*nsamp = 0;
	*samprate = 0.0f;
	*t_error = 0;


	//Calculate start and end times of the request
	int doy_start; //Start day of year
	int doy_end; //End day of year
	hptime_t starttime; //Start time in format from libmseed
	hptime_t endtime; //Same as starttime
	BTime bstarttime; //Start time in BTime format (SEED specification)
	BTime bendtime; //Save as bstarttime

	ms_md2doy( *year, *month, *day, &doy_start ); //Compute doy for starttime

	//Compute integer seconds and microseconds
	int intsec = ( int ) ( *sec );
	float auxsec = ( *sec - ( float ) intsec ) * 1000000;
	int intusec = ( int ) ( auxsec );

	//For debugging
	if( seisanArchDebug )
		printf( "SeisanArch: Request Start: %2d:%2d:%2d.%3d\n",
		*hour, *min, intsec, intusec );

	//Compute start and end times
	starttime = ms_time2hptime( *year, doy_start, *hour, *min, intsec, intusec );
	endtime = starttime + ( hptime_t ) ( *t_interval ) * ( hptime_t ) ( 1000000 );

	//Compute btimes
	ms_hptime2btime( starttime, &bstarttime );
	ms_hptime2btime( endtime, &bendtime );

	//Determine file names
	char fstart[120]; //Start file name
	char fend[120]; //End file name
	//Prepare char arrays with input data
	char *tarch = trim_string( archiveFolder, *archiveFolderLen ); //Base folder
	char *tnet = trim_string( net, *netlen ); //Network code
	char *tstat = trim_string( stat, *statlen ); //Station code
	char *tloc = trim_string( loc, *loclen ); //Location code
	char *tchan = trim_string( chan, *chanlen ); //Channel code

	if( seisanArchDebug )
		printf( "SeisanArch: ARC_ARCHIVE:\n <%s>\n", tarch );

	//Get filename, according to archive type
	FILE *fp = NULL;
	int i;
	for( i = 0; i < nFileFlags; i++ ) /* Note the hardcoded number of flags */
	{
		/* Compute file names with the current flag */
		getArchFilename( starttime, fstart, tarch, tstat, tchan, FileFlags[i], tnet, tloc, archiveType );
		getArchFilename( endtime, fend, tarch, tstat, tchan, FileFlags[i], tnet, tloc, archiveType );

		/* Check if the start file exists */
		if( ( fp = fopen( fstart, "r" ) ) != NULL )
		{
			/* The file exists!!! Close file, break and continue */
			fclose( fp );

			if( seisanArchDebug )
				printf( "SeisanArch: Open Filenames:\n <%s>\n <%s>\n", fstart, fend );
			break;
		}
		/* The file does not exist... move to next flag */
		if( seisanArchDebug )
			fprintf( stderr, "SeisanArch: cannot open Filenames:\n <%s>\n", fstart );
	}


	//Initiate MSTraceGroup
	MSTraceGroup *mstg = 0;
	mstg = mst_initgroup( mstg );

	//Read sequentially all record headers without including data
	getMSTG( starttime, endtime, fstart, fend, mstg, 0 );

	//Check for more than one read trace//Compute btimes
	if( mstg->numtraces < 1 )
	{
/*		printf( "Traces not found\n" ); lo */
		return;
	}

	//For debugging
	if( seisanArchDebug )
		printf( "SeisanArch: Old starttime - %ld\n", ( long ) starttime );

	//Count samples and determine new starttime
	*nsamp = copySamples( mstg, 0, &starttime, &endtime );

	//For debugging
	if( seisanArchDebug )
		printf( "SeisanArch: New starttime - %ld\n", ( long ) starttime );

	//Update samplerate output
	*samprate = ( float ) mstg->traces->samprate;

	//Produce new starttime output args
	char ts[27];
	ms_hptime2isotimestr( starttime, ts, 1 );
	sscanf( ts, "%4d-%2d-%2dT%2d:%2d:%9f",
		nyear, nmonth, nday, nhour, nmin, nsec );

	// Cleanup memory and close file
	mst_freegroup( &mstg );
}

/*
get_BUDData
Return sample data from a BUD archive
If data for the requested station and time interval is unavailable, returns samples as zero. 
 */
void getarchdata_( archiveFolder, archiveFolderLen, archiveType, stat, statlen, chan, chanlen, net, netlen, loc, loclen, year, month, day, hour, min, sec, t_interval,
	samples )
//Input arguments
char *archiveFolder; //Name of the base folder of the BUD archive
int *archiveFolderLen; //Length of the archive folder name
int *archiveType; //Archive type
char *stat; //station name
int *statlen; //station name length
char *chan; //channel name
int *chanlen; //channel name length
char *net; //network name
int *netlen; //network name length
char *loc; //location name
int *loclen; //location name length
int *year; //start time year 
int *month; //start time month
int *day; //start time day of month
int *hour; //start time hour
int *min; //start time minute
float *sec; //start time second
int *t_interval; //Requested time interval in seconds
//Output arguments
int *samples; //Array with the requested samples
{
	//printf("\n     ##### STARTING GETBUDDATA #####\n");
	//Calculate start and end times
	int doy_start;
	int doy_end;
	hptime_t starttime;
	hptime_t endtime;
	BTime bstarttime;
	BTime bendtime;

	ms_md2doy( *year, *month, *day, &doy_start ); //Compute doy for starttime

	//Compute integer seconds and microseconds
	int intsec = ( int ) ( *sec );
	float auxsec = ( *sec - ( float ) intsec ) * 1000000;
	int intusec = ( int ) ( auxsec );

	//Compute start and end times
	starttime = ms_time2hptime( *year, doy_start, *hour, *min, intsec, intusec );
	endtime = starttime + ( hptime_t ) ( *t_interval ) * ( hptime_t ) ( 1000000 );

	//For debugging
	//printf("<<<<<<< %2d:%2d:%2d.%3d >>>>>>>\n", *hour, *min, intsec, intusec);

	ms_hptime2btime( starttime, &bstarttime ); //Compute btimes
	ms_hptime2btime( endtime, &bendtime );

	//Determine file names
	char fstart[120]; //Start file
	char fend[120]; //End file
	char *tarch = trim_string( archiveFolder, *archiveFolderLen );
	char *tnet = trim_string( net, *netlen );
	char *tstat = trim_string( stat, *statlen );
	char *tloc = trim_string( loc, *loclen );
	char *tchan = trim_string( chan, *chanlen );

	//Get filename, according to archive type
	FILE *fp = NULL;
	int i;
	for( i = 0; i < nFileFlags; i++ ) /* Note the hardcoded number of flags */
	{
		/* Compute file names with the current flag */
		getArchFilename( starttime, fstart, tarch, tstat, tchan, FileFlags[i], tnet, tloc, archiveType );
		getArchFilename( endtime, fend, tarch, tstat, tchan, FileFlags[i], tnet, tloc, archiveType );

		/* Check if the start file exists */
		if( ( fp = fopen( fstart, "r" ) ) != NULL )
		{
			/* The file exists!!! Close file, break and continue */
			fclose( fp );

			if( seisanArchDebug )
				printf( "SeisanArch: Open Filenames:\n <%s>\n <%s>\n", fstart, fend );
			break;
		}
		/* The file does not exist... move to next flag */
		if( seisanArchDebug )
			fprintf( stderr, "SeisanArch: cannot open Filenames:\n <%s>\n", fstart );
	}


	//Init MSTraceGroup
	MSTraceGroup *mstg = 0;
	mstg = mst_initgroup( mstg );

	//Read sequentially all record headers including data
	getMSTG( starttime, endtime, fstart, fend, mstg, 1 );
	//mst_printtracelist (mstg, 0, 1, 1); //For debugging

	//Check for more than one read trace
	if( mstg->numtraces < 1 )
		return;

	//Copy samples
	int nsamples = copySamples( mstg, samples, &starttime, &endtime );


	// Cleanup memory and close file
	mst_freegroup( &mstg );
}

/*
Function to trim a char string and limit the number of characters
 */
char *trim_string( char *original, int nchar )
{
	int i;
	for( i = 0; i < nchar; i++ )
		if( original[i] == ' ' )
			break;
	char *trimmed = malloc( i + 1 );
	trimmed[i] = '\0';
	strncpy( trimmed, original, i );
	return trimmed;
}

/*
Function to compute Archive filenames
 */
char *getArchFilename( hptime_t time, char *filename, char *af, char *stat, char *chan, char quality, char *net, char *loc, int *archiveType )
{
	BTime btime;
	ms_hptime2btime( time, &btime ); //Compute btime
	//Determine file name
	if( *archiveType == 0 )
	{
		//BUD Archive
		sprintf( filename, "%s/%s/%s/%s.%s.%s.%s.%d.%03d",
			af, net, stat, stat, net, loc, chan, btime.year, btime.day );
	}
	else if( *archiveType == 1 )
	{
		//SDS Archive
		sprintf( filename, "%s/%d/%s/%s/%s.%c/%s.%s.%s.%s.%c.%d.%03d",
			af, btime.year, net, stat, chan, quality, net, stat, loc, chan, quality, btime.year, btime.day );
	}
	else
	{
		//Unknown archive type, leave
		return NULL;
	}
	return filename;
}

/*
Function to read a set of records in one or two files and return mstg
 */
void getMSTG( hptime_t starttime, hptime_t endtime, char *file1, char *file2, MSTraceGroup *mstg, int dataflag )
{
	//Read headers
	MSRecord *msr = NULL;
	int retcode;
	hptime_t recstart;
	hptime_t recend;
	int fcount;
	int nFiles = 2; //Number of files to check
	char *curFilename = file1;

	//Compare file names
	if( strcmp( file1, file2 ) == 0 )
		nFiles = 1; //Identical filenames, check only one

	for( fcount = 0; fcount < nFiles; fcount++ )
	{
		while( ( retcode = ms_readmsr( &msr, curFilename, 0, NULL, NULL, 1, dataflag, 0 ) ) == MS_NOERROR )
		{
			//msr_print(msr,0); //For debugging

			//Check if trace is relevant for request
			recstart = msr->starttime;
			recend = msr->starttime + ( hptime_t ) ( msr->samplecnt * 1000000.0 / msr->samprate );

			//Check if there is any need to continue
			if( recstart > endtime )
				break;

			//Add record to trace group
/*			if( ( recstart <= endtime ) && ( recend >= starttime ) )    lo */
/* previous line was including end of block when equal to requested time; and
 * was a problem if there was no data between start and end time, lo 11 Sep 2015 */
			if( ( recstart < endtime ) && ( recend > starttime ) )
			{
                            /*printf("debug lo\n");*/
				mst_addmsrtogroup( mstg, msr, 0, 0, 0 );
			}
		}
		ms_readmsr( &msr, NULL, 0, NULL, NULL, 0, 0, 0 ); //Reset record and free file
		msr_free( &msr ); /* 2014.04.28 - RSL - Missing core to close the file */

		curFilename = file2; //Set second file name

		//Check if it is necessary to go for another file
		if( endtime < recend )
			break;
	}
}

/*
Function to extract samples or simply count the number of samples from an mseed trace group
Returns the number of extracted samples.
if curSample is zero, does not copy samples. Only return the number of samples.
 */
int copySamples( MSTraceGroup *mstg, int *curSample, hptime_t *starttime, hptime_t *endtime )
{
	//mst_printtracelist( mstg, 1, 1, 1); //For debuging

	//Find first valid sample, looking for the earliest trace start,
	//the start of the trace containing starttime, if it exists
	MSTrace *mst = mstg->traces; //Initialize first trace in group
	hptime_t earliestStart = mst->starttime; //Initialize earliest starttime
	hptime_t latestEnd = mst->endtime; //Initialize latest endtime
	hptime_t startTrace = 0;

	//Cycle all records, to search for earliest record starttime
	while( mst != 0 )
	{
		//Compare starttime of record with earliest and the start time
		//of the trace that contains the requested start timme
		if( mst->starttime < earliestStart )
			earliestStart = mst->starttime;

		//Check if trace contains starttime, lo add one sample at end to check for overlap lo
		if( mst->starttime <= *starttime && *starttime <= mst->endtime + 1000000 / mstg->traces->samprate )
			startTrace = mst->starttime;
/*			printf( "SeisanArch: starttime %ld endtime %ld\n", ( long ) mst->starttime , (long) mst->endtime + 1000000 / mstg->traces->samprate );
			printf( "SeisanArch: starttime %ld\n", ( long ) *starttime );
			printf( "SeisanArch: difference %ld\n", ( long ) *starttime - mst->starttime);
			printf( "SeisanArch: startTrace %ld\n", ( long ) startTrace ); */
         

		//Move to next record
		mst = mst -> next;
	}

	//Debuging times 
	/* 
	char timeString[50];
	ms_hptime2isotimestr(earliestStart, timeString, 1);
	printf("Earliest Trace Start\n%s\n", timeString);
	ms_hptime2isotimestr(*starttime, timeString, 1);
	printf("Starttime\n%s\n", timeString);
	 */

	//For debugging
	if( seisanArchDebug )
		printf( "SeisanArch: Earliest start record - %ld\n", ( long ) earliestStart );

	//At this point, earliestStart contains the earliest starttime, which can be compared
	//with the requested starttime
	//If the requested starttime is before the earliest record, then it must become it
	//Otherwise, the starttime must be ajusted exactly to the closest sample
	long skipsamp = 0; //Number of samples to skip
	if( *starttime < earliestStart )
	{
		//This means that the first available record is after the requested
		//start time. So the starttime is shifted forwards to the first available sample
		*starttime = earliestStart;
	}
	else
	{
		//Compute the number of samples to skip in the first trace
		skipsamp = ( long ) ( ( double ) ( *starttime - startTrace ) * mstg->traces->samprate / 1000000 );

		//For debugging
		if( seisanArchDebug )
			printf( "SeisanArch: startTrace %ld\n", ( long ) startTrace );
	/*		printf( "SeisanArch: Will skip %ld samples\n", ( long ) skipsamp );*/

		//Compute new starttime
		*starttime = startTrace + ( hptime_t ) ( ( double ) skipsamp / mstg->traces->samprate * 1000000 );
	}

	//For debuging
	//printf("Skipping %d samples from the first trace\n", skipsamp);

	//Cycle traces again to copy/count samples
	//Note that the traces are not necessarily in time order
	//So the position of each sample in the final vector must be calculated
	mst = mstg->traces;
	int *curRecordSample; //Pointer for samples in each record
	int i = 0;
	int curRecordStartSample = 0;
	int curRecordEndSample = 0;
	int samplePos = 0;
	int maxSamplePos = 0; //Position of the last sample -> corresponds to sample count
	hptime_t curSampleTime;

	while( mst != 0 )
	{
		//char timeString[50];
		//ms_hptime2isotimestr(mst->starttime, timeString, 1);
		//printf("Trace Start\n%s\n", timeString);

		//Compute starting sample for this record
		curRecordStartSample = ( mst->starttime<*starttime ) ? skipsamp : 0;

		//Compute ending sample for this record
		curRecordEndSample = ( mst->endtime <= *endtime ) ?
			mst->samplecnt :
			( int ) ( ( double ) ( *endtime - mst->starttime ) * mst->samprate / 1000000 + 1 );

		//Compute position of the first sample on the array
		samplePos = ( mst->starttime<*starttime ) ? 0 :
			( int ) ( ( ( double ) ( mst->starttime - *starttime ) ) * mst->samprate / 1000000 + 0.5 );

		int lastSamp = samplePos + ( curRecordEndSample - curRecordStartSample ) - 1;
		if( lastSamp > maxSamplePos )
			maxSamplePos = lastSamp;

		//For debuging	
		//printf("<<<< Extraction Report >>>>\nRecord start sample: %d\nRecord end sample: %d\nStartting position in output array: %d\nPosition of last sample: %d\n",
		//	curRecordStartSample, curRecordEndSample, samplePos, lastSamp);

		if( curSample != 0 )
			for( i = curRecordStartSample; i < curRecordEndSample; i++ )
				*( curSample + samplePos + i - curRecordStartSample ) = *( ( int* ) mst->datasamples + i );

		//Move to next record
		mst = mst -> next;
	}
	return(maxSamplePos + 1 );
}





