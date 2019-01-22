/*
 * SEISAN MSEED HANDLING ROUTINES
 * The seisan routines for handling miniSEED waveform data
 * 
 * Author: R. Luis
 *
 * Last Update: 17.03.2015 
 * 17.03.2015 pv : changes added for reading float format mseed data e.g.:
 *                 populateSamples is replaced by populateFloatSamples and populateIntegerSamples
 * 28.04.2014 rl : updated comments, changed error with updating data quality
 *					fixed string handling function. Fixed getStatMSTG to close
 *					file after reading.
 * 24.10.2011 rl : more comments
 * 
 * Summary: These routines make use of Chad Trabant's miniSEED handling library,
 * libmseed, to provide simple access to data from Fortran routines. They have
 * been specifically designed for usage by the SEISAN Earthquake Analysis
 * Software Package. Nonetheless, they are self-contained hence may be used     
 * independently of SEISAN.
 * 
 * Main Functions (to be called from Fortran Routines)
 * seed_contents     : Returns a summary of the data available on a given file
 * seed_channel_read : Reads data from a file populating an input array
 * seed_channel_write: Write data to a miniseed file
 * 
 */


/*******************************************************************************
 *                                                                             *
 *                                  Includes                                   *
 *                                                                             *
 ******************************************************************************/
#include <string.h>
#include <stdio.h>
#include <libmseed.h>



/*******************************************************************************
 *                                                                             *
 *                                 Definitions                                 *
 *                                                                             *
 ******************************************************************************/
#define ERROR_TIMING 'E' //Flag for an indication of timing error
#define ERROR_GAP 'G' //Flag for a detected gap
#define ERROR_OVERLAP 'O' //Flag for a detected overlap
#define ERROR_NOERROR ' ' //Flag for no error



/*******************************************************************************
 *                                                                             *
 *                            Internal Structures                              *
 *                                                                             *
 ******************************************************************************/

/* MSEED_CHANNEL is used by seed_contents to store information on a channel when
 * mapping the contents of the miniSEED file. */
typedef struct
{
	char station[11]; //Station
	char channel[11]; //Channel
	char network[11]; //Network
	char location[11]; //Location
	hptime_t starttime; //Start time of the first record in the file for this 
	//channel. Note that this is not necessarily the earliest record
	hptime_t endtime; //Endtime + 1 sample period of the last non-overlapping 
	//record in the file for this channel.
	int nsamp; //Number of samples for this channel, including gaps. This also
	//considers blocks excluded as overlaps as gaps.
	off_t fileStart; //Position of the first byte of the first block for this
	//channel on the file
	off_t fileEnd; //Position of the last byte of the last non-overlapping block
	//for this channel on the file + 1
	char overlapFlag; //Stores the overlap flag
	char gapFlag; //Stores the gap flag
	char timingErrorFlag; //Stores the timing error flag
	float samprate; //sampling rate
} MSEED_CHANNEL;



/*******************************************************************************
 *                                                                             *
 *                           Function Declarations                             *
 *                                                                             *
 ******************************************************************************/

// getTimingErrorIndication: Returns a timing error flag from a SEED data 
// quality byte
char getTimingErrorIndication( char in );

// findExistingChannel: Finds a MSEED_CHANNEL within an array of these struct.
MSEED_CHANNEL *findExistingChannel( MSRecord *, MSEED_CHANNEL *, int );

// strtrim, strcpyl, and strcpyx: String manipulation functions to handle
// fortran character arrays
char* strtrim( char* );
int strcpyl( char *s1, char *s2, int len );
int strcpyx( char *s1, char *s2, int len );

// populateDate: Takes a hptime_t date (see libmseed documentation) and stores
// as a standard date
int populateDate( hptime_t, int*, int*, int*, int*, int*, float* );

// getStatMSTG: Reads samples from a miniSEED file into a pre-allocated array as
// integers.
int getStatMSTG( char *filename, off_t filestart, off_t fileend,
	char *stat, char *chan, char *net, char *loc,
	hptime_t *starttime, hptime_t *endtime,
	float *samples, int *nsamples );
// pv:	int *samples, int *nsamples );

// populateSamples: Support function for getStatMSTG
//int populateSamples( int *samples, int *nsamples,
//	hptime_t *starttime, hptime_t *endtime, int firstRecord, MSRecord *msr );

// populateFloatSamples: Support function for getStatMSTG
int populateFloatSamples( float *samples, int *nsamples,
	hptime_t *starttime, hptime_t *endtime, int firstRecord, MSRecord *msr );

// populateIntegerSamples: Support function for getStatMSTG
int populateIntegerSamples( float *samples, int *nsamples,
	hptime_t *starttime, hptime_t *endtime, int firstRecord, MSRecord *msr );

/*******************************************************************************
 *                                                                             *
 *                                seed_contents                                *
 *                                                                             *
 * seed_contents is used to map the contents of a miniSEED file supporting the *
 * usage of seed_channel_read. seed_contents should always be called reading   *
 * data with the seed_channel_read function. However, this is not mandatory.   *                                                               *
 *                                                                             *
 * seed_contents must receive as input parameters a filename and a set of      *
 * pointers to string, integer and real arrays that are to contain information *
 * on the channels encoded in the file to be analysed. These arrays include    *
 * channel information (station, channel, network, and location) as well as    *
 * start time (year, month, day, etc.) and the sampling rate.                  *
 *                                                                             *
 * seed_contents will also check the SEED data quality flag, filling an 80     *
 * character array for each encoded channel with character quality flags. Each *
 * position of the array corresponds to a specific flag, which should be       *
 * filled with a space character (' ') if inactive.                            *
 *                                                                             *
 * seed_contents will fill arrays with the file position of the first byte of  *
 * the first block of each channel and the file position of the last byte of   *
 * the last non-overlapping block of each channel plus one. This information   *
 * is to be provided to the seed_channel_read function to speed up reading of  *
 * non-mulplexed miniSEED files.                                               *
 *                                                                             *
 * Finally, seed_contents provides an array with the number of samples for each*
 * encoded channel including gaps and skipping overlapped blocks. When reading *
 * data for each channel, the calling Fortran routines should reserve a number *
 * of integer samples as indicated by seed_contents.                           *
 *                                                                             *
 * seed_contents and the remaining functions in this file consider a gap or    *
 * overlap as deviations of plus or minus half a sample period of the start    *
 * time of a miniSEED block.                                                   *
 *                                                                             *
 *                                                                             *
 * Input Parameters:                                                           *
 *                                                                             *
 * filename : 80 character string with the name of the miniSEED file.          *
 *                                                                             *
 * maxChannelCount : integer with the maximum acceptable number of channels.   *
 *                                                                             *
 * channelCount : integer to be returned with the actual number of channels in *
 *                the miniSEED file for the specified time interval, if        *
 *                existing. This implies that if there are no channels within  *
 *                the specified interval, this value will return zero even     *
 *                though there might be channels encoded on distinct time      *
 *                intervals.                                                   *
 *                                                                             *
 * stat, chan, net, loc : Arrays of strings to receive the names of station (5 *
 *                char), channel (3 char), network (2 char) and location (2    *
 *                char) for each channel. Each array must have a length equal  *
 *                to maxChannelCount.                                          *
 *                                                                             *
 * year, month, day, hour, min, sec: Arrays of integers (except sec which is   *
 *                real) to be populated with the start time of each encoded    *
 *                channel. If the first element of the year array is zero,     *
 *                seed_contents will map the entire file. If not, seed_contents*
 *                will assume that the first and second elements of these      *
 *                arrays constitute a time interval limiting the search. In    *
 *                this case, seed_contents will limit the search to blocks     *
 *                within the specified time interval.                          *
 *                The start time of one channel corresponds always to the start*
 *                time of the first available block in the file. This assumes  *
 *                that at that block is the earliest, considering any blocks   *
 *                with the same or earlier start times as overlaps.            *
 *                                                                             *
 * samprate: Array of reals to be populated with the sampling rate of the each *
 *                encoded channel. Note that only the sampling rate indicated  *
 *                on the first block of each channel is considered. Variable   *
 *                sampling rates are not considered by seed_contents and only  *
 *                blocks within the specified time interval, if existing.      *
 *                                                                             *
 * nsamp: Array of integers to be populated with the available number of       *
 *                samples for each encoded channel within the specified time   *
 *                interval, if existing. The number of samples for each channel*
 *                assumes that any gaps will be filled with zeros. Furthermore *
 *                overlapping blocks will be discarded, which may result in    *
 *                additional gaps. In practice, the user should reserve this   *
 *                number of samples for extraction when calling                *
 *                seed_channel_read.                                           *
 *                                                                             *
 * qualityFlags: Array of 80 character strings to be populated with quality    *
 *                indication flags for each encoded channel. Each character in *
 *                each string has its specific purpose and inactive flags are  *
 *                represented by a space character ' '.                        *
 *                The quality flags are checked for all miniSEED blocks and set*
 *                if any of those blocks presents a flagable indication. E.g.  *
 *                if a single block out of theverified blocks for the channel  *
 *                under analysis within the specified time interval has an     *
 *                active timing error indication, the corresponding quality    *
 *                flag will be set.
 *                Currently, only three flags are used, as follows:            *
 *                                                                             *
 *                position 1: Timing Error Indication                          *
 *                   This flag takes the value ERROR_TIMING ('E') when the     *
 *                   corresponding SEED data quality flag is set.              *
 *                position 2: Gap Indication                                   *
 *                   This flag takes the value ERROR_GAP ('G') when a gap      *
 *                   exceeding half a sample period is detected. Note that     *
 *                   discarded overlapping blocks are not considered as gaps.  *                                      *
 *                position 3: Overlap Indication                               *
 *                   This flag takes the value ERROR_OVERLAP ('O') when an     *
 *                   overlap exceeding half a sample period is detected.       *
 *                                                                             *
 * filestart, fileend: These are integer arrays with the file positions for the*
 *                start and end of each encoded channel, taking into account   *
 *                the specified time interval, if existing. The filestart is   *
 *                first byte of the first block of each channel containing the *
 *                specified time interval, if existing. fileend corresponds to *
 *                the last byte of the last block of each channel containing   *
 *                the specified time interval plus one or the first byte of the*
 *                following block. These parameters may be used to speed up    *
 *                reading of data on a call to seed_channel_read. However, are *
 *                not mandatory.                                               *
 *                                                                             *
 ******************************************************************************/
void seed_contents_( filename, maxChannelCount, channelCount,
	stat, chan, net, loc,
	year, month, day, hour, min, sec,
	samprate, nsamp, qualityFlags, filestart, fileend,
	len_filename, len_stat, len_chan, len_net, len_loc, len_qualityFlag )
char* filename; //File to process
int *maxChannelCount; //Maximum number of channels to reserve
int *channelCount; //Output parameter for the actual channel count
char *stat; //Pointer to an array of station names to be filled
char *chan; //Pointer to an array of channel names to be filled
char *net; //Pointer to an array of network names to be filled
char *loc; //Pointer to an array of location names to be filled
int *year; //Pointer to an array of starting years to be filled
int *month; //Pointer to an array of starting months to be filled
int *day; //Pointer to an array of starting days to be filled
int *hour; //Pointer to an array of starting hours to be filled
int *min; //Pointer to an array of starting minutes to be filled
float *sec; //Pointer to an array of starting seconds to be filled
float *samprate; //Pointer to an array of sampling rates to be filled
int *nsamp; //Pointer to an array of number of samples to be filled
char *qualityFlags; //Pointer to an array of quality flag strings
int *filestart; //Pointer to an array of file starting positions to be filled
int *fileend; //Pointer to an array of file end positions to be filled
//The remaining input arguments are filled automaticaly by the compiler.
size_t len_filename; //Length of the filename string
size_t len_stat; //Length of each station string
size_t len_chan; //Length of each channel string
size_t len_net; //Length of each network string
size_t len_loc; //Length of each location string
size_t len_qualityFlag; //Length of the quality flag strings
{
	
	/* Variable declarations */
	MSRecord *msr = NULL; /* Pointers to the miniseed records */
	int retcode; /* Reader return code - not used */
	int dataflag = 0; /* Read or not data - not used */
	hptime_t recstart, recend; /* Start and end of record */
	hptime_t starttime, endtime; /* Start and end of search */
	hptime_t gap, maxGap = 0; /* Compute gaps */
	MSEED_CHANNEL channels[*maxChannelCount]; /* Fill with channel info */
	int nchannels = 0; /* Current number of channels */
	int i, m; /* General purpose counters */
	int doy_start; /* For day of year calculations */
	int deltaSamp; /* For counting sample differences */
	off_t curFilePos; /* Indicator of the current file position */
	char curTimingFlag; /* Timing issues flag */

	/* Prepare strings from Fortran
	 * This null terminates the strings from fortran and trims to remove any
	 * leftover spaces.
	 **************************************************************************/
	filename[( int ) len_filename - 1] = '\0'; /* Null terminate the filename */
	strtrim( filename ); /* Trim the filename */

	/*printf("pv: seed_contents\n");*/

	/* Check start and end times to find operation mode.
	 * If *year is zero means mapping the entire file
	 * If *year is larger than zero, it is necessary to find a given time 
	 * interval within the file.
	 **************************************************************************/
	if( *year > 0.0 )
	{
		/* This mode of operation means to find a specific time interval within
		 * the file. For this purpose, it is necessary to first convert the date
		 * given by fortran to a date that can be used with the miniseed search
		 * routines
		 **********************************************************************/
		
		/* Calculate start time from the fortran input */
		ms_md2doy( *year, *month, *day, &doy_start ); /* doy for starttime */
		starttime = ms_time2hptime( *year, doy_start, *hour, *min,
			( int ) ( *sec ),
			( int ) ( ( *sec - ( int ) ( *sec ) )*1000000.0 ) ); /* Starttime */
		
		/* Calculate end time from the fortran input */
		ms_md2doy( *( year + 1 ), *( month + 1 ),
			*( day + 1 ), &doy_start ); /* doy for endtime */
		endtime = ms_time2hptime( *( year + 1 ), doy_start, *( hour + 1 ),
			*( min + 1 ), ( int ) ( *( sec + 1 ) ),
			( int ) ( ( *( sec + 1 ) 
			- ( int ) ( *( sec + 1 ) ) )*1000000.0 ) ); /* Endtime */
	}
	else
	{
		/* This mode of operation implies mapping the entire file. In this case
		 * start and end times can be set to the respective minimum and 
		 * maximum values*/
		starttime = -1;
		endtime = ( hptime_t ) ( 18446744073709551615.0 );
	}




	/* Start reading blocks from the file. This section reads the headers on
	 * each block to find if they belong to the time interval defined by
	 * starttime -> endtime. It also collects the available channels and checks
	 * for gaps and overlaps.
	 * The functional procedure is as follows:
	 * - Read block header
	 * - Check if the channel has been seen before
	 *		- If the channel has not been seen before, create new record for 
	 *		  this channel with new start or end times.
	 *		- If this is an already seen channel, update start and end times.
	 *		  Also, check if there are overlaps or gaps.
	 **************************************************************************/
	
	/* while cycle reads a new block on each cycle */
	while( ( retcode = ms_readmsr( &msr, filename, 0, &curFilePos, NULL, 1, dataflag, 0 ) )
		== MS_NOERROR )
	{
		/* Find out if the current channel has already been identified before */
		MSEED_CHANNEL *curChannel =
			findExistingChannel( msr, channels, nchannels );

		/* Compute start and end times for the current record */
		recstart = msr->starttime;
		recend = msr->starttime
			+ ( hptime_t ) ( ( double ) msr->samplecnt * 1000000.0
			/ msr->samprate );

		/* Check if the current record is within the required time interval */
		/*if( recend < starttime || recstart > endtime )*/
		if( recend < starttime || recstart >= endtime )
		{
			/* Its not, move to next record */
			continue;
		}

		/* Process record
		 * If this is the first time this channel is found (curChannel==NULL)
		 * Then it is necessary to create a new entry on the channels array.
		 * Otherwise, just update the corresponding entry with the endtime and
		 * end file position.
		 **********************************************************************/
		if( curChannel == NULL )
		{
/*                printf("debug lo\n"); */
			/******************************************************************
			 * This is a new channel, introduce it in the channels array      *
			 ******************************************************************/
			
			/* Check maximum number of channels */
			if( nchannels == *maxChannelCount )
			{
				/* The maximum number of channels is already reached */
				//printf("Unable to add more channels to channel structure\n");
				continue;
			}
			
			/* Create new entry in the channels array */
			strcpy( channels[nchannels].station, msr->station );
			strcpy( channels[nchannels].channel, msr->channel );
			strcpy( channels[nchannels].network, msr->network );
			strcpy( channels[nchannels].location, msr->location );

			/* Set the initial value for the sample count.
			 * At this point, the number of samples is the number of samples in
			 * the block. However, this value must be corrected if the starttime
			 * or endtime are adjusted when finding more records.
			 ******************************************************************/
			channels[nchannels].nsamp = msr->samplecnt;

			/* The requested start time is always different from the block start
			 * So, it is necessary to adjust the starttime to the exact time of
			 * the first sample within the requested time interval
			 ******************************************************************/
			
			/* Compare the start time with the record start */
			/*if( starttime < recstart )*/
			if( starttime <= recstart )    /* lo */
			{
				/* In this case the requested interval starts before the
				 * start of the record. This corresponds to the case where 
				 * there are no records before starttime. As such the new 
				 * starttime must be a postponed version of the old starttime
				 **************************************************************/
				channels[nchannels].starttime = recstart;
			}
			else
			{
				/* In this case, the requested interval starts after the 
				 * start of the record. Hence the new starttime is a delayed
				 * version of the requested.
				 **************************************************************/
				
				/* Compute the difference is sample times */
				deltaSamp = ( int ) ( ( double ) ( starttime - recstart )
					* msr->samprate / 1000000.0 ) + 1;
				
				/* Update the channel start time */
				channels[nchannels].starttime = recstart
					+ ( hptime_t ) ( ( double ) deltaSamp 
					* 1000000.0 / msr->samprate );
				
				/* Some of the record's samples will have to be discarded. Hence
				 * it is necessary to correct the number of samples
				 **************************************************************/
				channels[nchannels].nsamp -= deltaSamp;
			}

			/* Check if this is the last record on the request
			 * If so, it is necessary to set the number of samples accordingly
			 ******************************************************************/
			if( endtime <= recend )
			{
				channels[nchannels].nsamp = ( int ) ( ( double ) ( endtime
					- channels[nchannels].starttime )
					* msr->samprate / 1000000.0 );
			}

			/*The endtime may be set as the end of the block since its not 
			 * required for the output.
			 ******************************************************************/
			channels[nchannels].endtime = recend;
			channels[nchannels].overlapFlag = ' ';
			channels[nchannels].gapFlag = ' ';
			channels[nchannels].timingErrorFlag =
				getTimingErrorIndication( msr->dataquality );
			channels[nchannels].samprate = ( float ) msr->samprate;
			channels[nchannels].fileStart = curFilePos;
			channels[nchannels].fileEnd = curFilePos + msr->reclen;
			nchannels++;
		}
		else
		{
			/******************************************************************
			 * This is an existing channel, update its date/time parameters   *
			 ******************************************************************/
			
			/* Define the maximum allowed gap for this channel 
			 * Note that this has to be done channel by channel as the sampling
			 * rates may change */
			maxGap = ( hptime_t ) ( 1000000.0 / msr->samprate / 2 );
			
			/* Compute gap between this record and the endtime of this channel*/
			gap = recstart - curChannel->endtime;
			
			/* Compare the gap with the maximum allowed gap */
			if( gap > maxGap )
			{
				/* The gap is more than what is allowed. To handle this, we
				 * assume blank samples to fill in the gap and set the gap
				 * flag.
				 **************************************************************/
				
				/* Add false samples corresponding to gap */
				curChannel->nsamp += ( int ) ( ( double ) gap
					/ 1000000.0 * msr->samprate );
				
				/* Set gap flag */
				curChannel->gapFlag = ERROR_GAP;
				
			}
			else if( gap < -maxGap )
			{
				/* Detected overlap - Flag it
				 ****************************/
				curChannel->overlapFlag = ERROR_OVERLAP;
				continue;
			}

			/* Evaluate timing error indication from miniseed record */
			curTimingFlag = getTimingErrorIndication( msr->dataquality );
			if( curTimingFlag != ' ' )
				curChannel->timingErrorFlag = curTimingFlag;

			/* Add samples for this record
			 * For this, it is necessary to check if the requested interval
			 * ends in this record. If not, add all the samples of the record.
			 * If it does, we need to compute how many samples of the record
			 * will be used.
			 *****************************/
			if( endtime >= recend )
			{
				/* The entire record belongs to the requested interval so all
				 * samples can be added */
				curChannel->nsamp += ( int ) msr->samplecnt;
			}
			else
			{
				/* This is the last record of the requested interval. Compute
				 * the samples that will be required */
				curChannel->nsamp += ( int ) ( ( double ) ( endtime - recstart )
					* msr->samprate / 1000000.0 );
			}

			/* Update endtime for this channel
			 *********************************/
			curChannel->endtime = recend; /* This is NOT correct */
			
			/* Update the file position to the end of this record */
			curChannel->fileEnd = curFilePos + msr->reclen;
		}
	} /* End of file reading cycle */

	/* Reset record and free file  - This comes from Chad's example routines */
	ms_readmsr( &msr, NULL, 0, NULL, NULL, 0, 0, 0 );
	msr_free( &msr );

	/* Return if there are no valid channels */
	if( retcode != MS_ENDOFFILE )
	{
		*maxChannelCount = 0;
		return;
	}

	/* Store data in output arrays */
	for( i = 0; i < nchannels; i++ )
	{

		/* Station data */
		strcpyx( stat + i * ( int ) len_stat, channels[i].station, len_stat );
		strcpyx( chan + i * ( int ) len_chan, channels[i].channel, len_chan );
		strcpyx( net + i * ( int ) len_net, channels[i].network, len_net );
		strcpyx( loc + i * ( int ) len_loc, channels[i].location, len_loc );

		/* Populate date */
		populateDate( channels[i].starttime, year + i, month + i,
			day + i, hour + i, min + i, sec + i );

		/* Sampling rate */
		*( samprate + i ) = channels[i].samprate;

		/* Number of samples including gaps */
		*( nsamp + i ) = channels[i].nsamp;

		/* File position */
		*( filestart + i ) = ( int ) channels[i].fileStart;
		*( fileend + i ) = ( int ) channels[i].fileEnd;

		/***************
		 * Error flags *
		 ***************/
		//Timing Error
		//*(qualityFlags + i * (int) len_qualityFlag) = channels[i].timingErrorFlag;

		//Gap Error
		//*(qualityFlags + i * (int) len_qualityFlag + 1) = channels[i].gapFlag;

		//Overlap Error
		//*(qualityFlags + i * (int) len_qualityFlag + 2) = channels[i].overlapFlag;

		//Remainder
		//for (m = 3; m < len_qualityFlag; m++) 
		//    *(qualityFlags + i * (int) len_qualityFlag + m) = ' ';
	}


	/* Returns number of processed channels
	 **************************************/
	*channelCount = nchannels;

	
	return;
}

/*******************************************************************************
 *                                                                             *
 *                             seed_channel_read                               *
 *                                                                             *
 * Reads data from a miniseed file, given the filename, channel to read and    *
 * time interval. Must be given a pointer to a sample array with enough samples*
 * to store the complete time interval, even if there are gaps. All gaps will  *
 * be filled with zeros except if there is one gap terminating after the end of*
 * the requested time interval. In that case, the sample array will be filled  *
 * until the last available sample.                                            *
 * Input parameters are:                                                       *
 *                                                                             *
 * filename : This is the input filename. Use the absolute or relative paths.  *
 *                  If this file does not exist, seed_channel_read will return *
 *                  -1 and will not change the input arrays.                   *
 *                                                                             *
 * stat, chan, net, loc : Character pointers to strings containing the names of*
 *                  the station, channel, network and location, respectively. A*
 *                  maximum of 11 characters is considered for each string.    *
 *                  Nonetheless the seed protocol specifies less than that.    *                                                *
 *                                                                             *
 * year, month, day, hour, min : Integer pointers to the starting date and time*
 *                  parameters. seed_channel_read will try to read samples     *
 *                  after the start time.                                      *
 *                  In case the available samples start after the start time,  *
 *                  seed_channel_read will populate the samples array starting *
 *                  from the begining, i.e. the first available sample will go *
 *                  to the first position of the array, effectively changing   *
 *                  actual start time.
 *
 * sec : Pointer to a 32 bit float with the startting second. Same rules as for*
 *                  the previous time parameters apply                         *
 *                                                                             *
 * t_interval : Pointer to a 32 bit float with the time interval to be         *
 *                  extracted. In case there are no samples for the full       *
 *                  interval, the samples of the output array will be left     *
 *                  with zeros                                                 *
 *                                                                             *
 * maxSamples : Pointer to an integer containing the maximum number of samples *
 *                  to extract. This corresponds to the capacity of the array  *
 *                  made available by the calling routine for filling with     *
 *                  samples.                                                   *
 *                                                                             *
 * nsamples : Pointer to an integer that will be filled with the actual number *
 *                  of samples that have been extracted.                       *
 *                                                                             *
 * samples : Pointer to an integer array to be filled with samples. The array  *
 *                  capacity must correspond to maxSamples.                    *
 *                                                                             *
 * qualityFlags : Not working                                                  *
 *                                                                             *
 * filestart : Pointer to an integer with the position where the data to be    *
 *                  extracted begins in the file. This is the value given by   *
 *                  the call to seed_contents. However, if it is unknown, just *
 *                  give a value zero.                                         *
 *                                                                             *
 * fileend : Ponter to an integer with the position where the data to be       *
 *                  extracted ends in the file. This is the value given by     *
 *                  the call to seed_contents. If unknown, give an maximum int.*
 *                                                                             *
 ******************************************************************************/
void seed_channel_read_( filename, stat, chan, net, loc,
	year, month, day, hour, min, sec,
	t_interval, maxSamples, nsamples, samples, qualityFlags,
	filestart, fileend,
	len_filename, len_stat,
	len_chan, len_net, len_loc, len_qualityFlags )
char *filename; //Filename to be accessed
char *stat; //Station (5 char)
char *chan; //Channel (3 char)
char *net; //Network (2 char)
char *loc; //Location(2 char)
int *year; //Year    (integer)
int *month; //Month   (integer)
int *day; //Day     (integer)
int *hour; //Hour    (integer)
int *min; //Minute  (integer)
float *sec; //Second (real)
float *t_interval; //Time interval to extract (real)
int *maxSamples; //Maximum number of samples to fill
int *nsamples; //Actual number of samples that have been used
// pv: int *samples; //Array of samples to fill in
float *samples; //Array of samples to fill in
char *qualityFlags;
int *filestart;
int *fileend;
//The remaining input variables are filled in automatically by fortran
size_t len_filename;
size_t len_stat;
size_t len_chan;
size_t len_net;
size_t len_loc;
size_t len_qualityFlags;
{
	int doy_start;
	int i;
	hptime_t starttime;
	hptime_t endtime;
	char statn[( int ) len_stat], chann[( int ) len_chan],
		netn[( int ) len_net], locn[( int ) len_loc];


	/* Prepare strings from Fortran
	 ******************************/
	filename[( int ) len_filename - 1] = '\0'; // Null terminate the filename
	strtrim( filename ); // Trim the filename
	strcpyl( statn, stat, ( int ) len_stat );
	strcpyl( chann, chan, ( int ) len_chan );
	strcpyl( netn, net, ( int ) len_net );
	strcpyl( locn, loc, ( int ) len_loc );

	//printf("pv: seed_channel_read_\n");

	/* Calculate start and end times in microseconds since 1970
	 **********************************************************/
	if( *t_interval > 0.0 )
	{
		ms_md2doy( *year, *month, *day, &doy_start ); //Compute doy for starttime
		starttime = ms_time2hptime( *year, doy_start, *hour, *min,
			( int ) ( *sec ),
			( int ) ( ( *sec - ( int ) ( *sec ) )*1000000.0 ) ); //Starttime
		endtime = starttime + ( hptime_t ) ( *t_interval * 1000000.0 );
	}
	else
	{
		starttime = ( hptime_t ) 0;
		endtime = ( hptime_t ) ( 18446744073709551615.0 );
	}

	//Zero samples array
	for( i = 0; i < *maxSamples; i++ )
	{
	// pv:	*( samples + i ) = 0;
		*( samples + i ) = 0.0;
	}

	// Retrieve samples from file into the samples array
	*nsamples = getStatMSTG( filename, 
		( off_t ) ( *filestart ), ( off_t ) ( *fileend ),
		strtrim( statn ), strtrim( chann ),
		strtrim( netn ), strtrim( locn ),
		&starttime, &endtime,
		samples, maxSamples );



	// Populate start date correctly
	populateDate( starttime, year, month,
		day, hour, min, sec );

	return;
}

/*******************************************************************************
 *                                                                             *
 *                             seed_channel_write                              *
 *                                                                             *
 * Writes data to a miniseed file                                              *
 ******************************************************************************/
void seed_channel_write_( filename,
	stat, chan, net, loc,
	year, month, day, hour, min, sec, samprate,
	nsamples, fsamples,
	appendFlag, compFlag, recSize, qualityFlags,
	len_filename, len_stat, len_chan, len_net, len_loc )
char *filename; //Path and name of the file to write
char *stat; //Station (5 char)
char *chan; //Channel (3 char)
char *net; //Network (2 char)
char *loc; //Location(2 char)
int *year; //Year    (integer)
int *month; //Month   (integer)
int *day; //Day     (integer)
int *hour; //Hour    (integer)
int *min; //Minute  (integer)
float *sec; //Second (real)
float *samprate;
int *nsamples; //Maximum number of samples to fill and returned number of samp
int *fsamples; //Array of samples to fill in
int *appendFlag; //Append flag: 0 to replace, 1 to append
int *compFlag; //Compression flag: 0: STEIM1,   1: STEIM2
int *recSize;
char *qualityFlags;
//The remaining input variables are filled in automatically by fortran
size_t len_filename;
size_t len_stat;
size_t len_chan;
size_t len_net;
size_t len_loc;
{
	int precords;
	/* pv 
        int verbose = 0;
        */
	int verbose = 1;
	MSTrace *mst = NULL;
	int *samples;
	int i;
	int encform; //Encoding format

	/* Prepare strings from fortran */
	char statn[( int ) len_stat], chann[( int ) len_chan],
		netn[( int ) len_net], locn[( int ) len_loc];
	strcpyl( statn, stat, ( int ) len_stat );
	strtrim( statn );
	strcpyl( chann, chan, ( int ) len_chan );
	strtrim( chann );
	strcpyl( netn, net, ( int ) len_net );
	strtrim( netn );
	strcpyl( locn, loc, ( int ) len_loc );
	strtrim( locn );
	filename[( int ) len_filename - 1] = '\0'; // Null terminate the filename
	strtrim( filename ); // Trim the filename

	/* Store the samples on an internal buffer
	 *****************************************/
	samples = malloc( *nsamples * sizeof(int ) );
	//Unable to allocate memory
	if( samples == NULL )
		return;
	for( i = 0; i<*nsamples; i++ )
		samples[i] = *( fsamples + i );

	/* Initialize the miniseed trace
	 *******************************/
	mst = mst_init( mst );

	/* Populate MSTrace values */
	strcpy( mst->network, netn );
	strcpy( mst->station, statn );
	strcpy( mst->channel, chann );
	strcpy( mst->location, locn );

	int doy_start;
	ms_md2doy( *year, *month, *day, &doy_start ); //Compute doy for starttime
	mst->starttime = ms_time2hptime( *year, doy_start, *hour, *min,
		( int ) ( *sec ),
		( int ) ( ( *sec - ( int ) ( *sec ) )*1000000.0 ) );
	mst->samprate = *samprate;
	mst->datasamples = ( void* ) samples; /* pointer to 32-bit integer data samples */
	mst->numsamples = ( int64_t ) ( *nsamples );
	mst->samplecnt = ( int64_t ) ( *nsamples );

	mst->sampletype = 'i'; /* declare type to be 32-bit integers */

	//Set timing error indication
	if( *qualityFlags == ERROR_TIMING )
		mst->dataquality |= 0x08;

	//Define encoding formats
//	printf("pv: set enform\n");
	switch( *compFlag )
	{
		case 0:
			encform = DE_STEIM1;
			break;
		case 1:
			encform = DE_STEIM2;
			break;
		default:
			encform = DE_STEIM1;
			break;
	}

	/* Write big-endian records, using Steim compression */
	precords = mst_writemseed( mst, filename, 1 - *appendFlag, *recSize, encform, 0, verbose );

	/* Disconnect datasamples pointer, otherwise mst_free() will free it */
	mst->datasamples = NULL;
	mst_free( &mst );
	//free(samples);
	return;
}

/*******************************************************************************
 * Internal function to read a byte with the seed flags and indicate if there  *
 * is timing uncertainty                                                       *
 * Returns a char ' ' for no uncertainty and 'E' for timing error              *
 ******************************************************************************/
char getTimingErrorIndication( char in )
{
	char res = 0x08 & in;
	if( res > 0 )
	{
		return 'E';
	}
	return ' ';
}

/*******************************************************************************
 * Internal function to search data for a channel on a miniseed file and return*
 * the corresponding group of records - Should be replaced with Chad's equiv.  *
 ******************************************************************************/
int getStatMSTG( char *filename, off_t filestart, off_t fileend,
	char *stat, char *chan, char *net, char *loc,
	hptime_t *starttime, hptime_t *endtime,
	float *samples, int *nsamples )
// pv:	int *samples, int *nsamples )
{


	//Read headers
	MSRecord *msr = NULL;
	int retcode;
	hptime_t recstart = 0;
	hptime_t recend = 0;
	off_t filepos = -filestart;
	int tempcount, sampleCount = 0;
	int firstRecord = 1;
	//printf("pv:  stat: %s, chan: %s, net: %s, loc: %s\n",stat,chan,net,loc);

	if( *starttime > 0 )
	{
		//Use time restrictions
		while( ( retcode = ms_readmsr( &msr, filename, 0,
			&filepos, NULL, 1, 1, 0 ) ) == MS_NOERROR )
		{
			//msr_print(msr, 0); //For debugging

			//Check if current offset is within limits
			if( filepos >= fileend )
				break;

			//Check if trace is relevant for request
			if( strcmp( stat, msr->station ) != 0 ||
				strcmp( chan, msr->channel ) != 0 ||
				strcmp( net, msr->network ) != 0 ||
				strcmp( loc, msr->location ) != 0 )
				continue;

			recstart = msr->starttime;
			//recstart = msr_starttime ( msr );

			//Check if this block follows the last one
			if( recstart < ( recend - ( hptime_t ) ( 500000.0 / msr->samprate ) ) )
				continue; //This is an overlap... ignore record

			recend = msr->starttime + ( hptime_t ) ( msr->samplecnt * 1000000.0 / msr->samprate );

			//Check if there is any need to continue
			if( recstart > *endtime )
				break;

			//Add record to trace group
			if( ( recstart <= *endtime ) && ( recend >= *starttime ) )
			{
				//Place samples in buffer
				//tempcount = populateFloatSamples( samples, nsamples, starttime, endtime, firstRecord, msr );
				/* pv check data type before reading */
				/* Read different sample types */
				if ( msr->sampletype == 'f' || msr-> sampletype == 'd' ) {
					//    samptype = "FLOAT";
					//printf("pv: SEISAN read miniseed data in float or double\n");
					tempcount = populateFloatSamples( samples, nsamples, starttime, NULL, firstRecord, msr );
					// return -1;
				}
				else if ( msr->sampletype == 'i' ) {
					//   samptype = "INTEGER";
					// printf("pv: SEISAN read miniseed data in integer\n");
					tempcount = populateIntegerSamples( samples, nsamples, starttime, NULL, firstRecord, msr );
				}
				else if ( msr->sampletype == 'a' ) {
					//  samptype = "ASCII";
					printf("!!! SEISAN cannot read miniseed data in ascii format !!!\n");
					return -1;
				}
				else {
					fprintf (stderr, "Error, unrecognized sample type: '%c'\n",
						msr->sampletype);
					return -1;
				}

				if( tempcount > sampleCount )
					sampleCount = tempcount;
			}
			firstRecord = 0;
		}
	}
	else
	{
		//There are no time restrictions
		while( ( retcode = ms_readmsr( &msr, filename, 0,
			&filepos, NULL, 1, 1, 0 ) ) == MS_NOERROR )
		{
			//msr_print(msr, 0); //For debugging

			//Check if current offset is within limits
			if( filepos >= fileend )
			{
				//printf("Reached end of the file\n");
				break;
			}

			//Check if trace is relevant for request
			if( strcmp( stat, msr->station ) != 0 ||
				strcmp( chan, msr->channel ) != 0 ||
				strcmp( net, msr->network ) != 0 ||
				strcmp( loc, msr->location ) != 0 )
			{
				//printf("Discarded - Wrong station\n");
				continue;
			}

			//Update recstart with the current starttime
			recstart = msr->starttime;

			//Check if this block follows the last one
			if( recstart < ( recend - ( hptime_t ) ( 500000.0 / msr->samprate ) ) )
			{
				//printf("Discarded - Its an overlap : %f < %f\n", recstart, (recend - (hptime_t) (500000.0 / msr->samprate)));
				continue; //This is an overlap... ignore record
			}

			recend = msr->starttime + ( hptime_t ) ( msr->samplecnt * 1000000.0 / msr->samprate );

			//The first available record will define the starttime
			if( firstRecord == 1 )
				*starttime = recstart;

			//Always try to define the endtime
			*endtime = recend;

			/* Read different sample types */
			if ( msr->sampletype == 'f' || msr-> sampletype == 'd' ) {
				//    samptype = "FLOAT";
				//printf("pv: SEISAN read miniseed data in float or double\n");
				tempcount = populateFloatSamples( samples, nsamples, starttime, NULL, firstRecord, msr );
				// return -1;
			}
			else if ( msr->sampletype == 'i' ) {
				//   samptype = "INTEGER";
				// printf("pv: SEISAN read miniseed data in integer\n");
				// tempcount = populateFloatSamples( samples, nsamples, starttime, NULL, firstRecord, msr );
				tempcount = populateIntegerSamples( samples, nsamples, starttime, NULL, firstRecord, msr );
			}
			else if ( msr->sampletype == 'a' ) {
				//  samptype = "ASCII";
				printf("!!! SEISAN cannot read miniseed data in ascii format !!!\n");
				return -1;
			}
			else {
				fprintf (stderr, "Error, unrecognized sample type: '%c'\n",
					msr->sampletype);
				return -1;
			}

			if( tempcount > sampleCount )
				sampleCount = tempcount;
			firstRecord = 0;
		}
	}

	ms_readmsr( &msr, NULL, 0, NULL, NULL, 0, 0, 0 ); //Reset record and free file
	msr_free( &msr ); /* 2014.04.28 - RSL - Missing core to close the file */
	return sampleCount;
}

/*
int populateSamples( int *samples, int *nsamples,
	hptime_t *starttime, hptime_t *endtime, int firstRecord, MSRecord *msr )
{
	int recPos;
	int readPos = 0;
	int counter = 0;
	int maxRecPos = *nsamples;

	//msr_print(msr, 0); //For debugging

	//Calculate position of the record start with respect to starttime
	recPos = ( int ) ( ( double ) ( msr->starttime - *starttime ) * msr->samprate / 1000000.0 + 0.5 );
	//printf("Original recPos: %d (%d)\n" , recPos, *nsamples);

	//Check if this is the first record to be dropped in the array
	if( firstRecord == 1 )
	{
		//It is! The starttime must be adjusted appropriately
		if( recPos >= 0 )
		{
			//In this case, the record starts after the original starttime
			//So the new starttime becomes the start of the record
			*starttime = msr->starttime;
			//In addition to correcting the starttime in this case, it is also
			//necessary to reset the recPos since now the record must be dropped
			//at exactly the start of the array
			recPos = 0;
		}
		else
		{
			//In this case, the record starts before the original starttime
			//So the new starttime has to be readjusted at a middle position
			//within the record
			*starttime = msr->starttime
				+ ( hptime_t ) ( ( double ) ( -recPos + 1 ) * 1000000.0 / msr->samprate );
		}
	}

	//Set read position and readjust recording position
	if( recPos < 0 )
	{
		readPos = -recPos;
		recPos = 0;
	}

	//Calculate maximum recording position according to endtime
	if( endtime != NULL )
		maxRecPos = ( int ) ( ( double ) ( *endtime - *starttime )
		* msr->samprate / 1000000.0 );


	//printf("Times %f <-> %f\n", (double)(*starttime), 
	//(double)(msr->starttime));
	//printf("RecPos: %d; ReadPos: %d; ",
	//        recPos, readPos);


	//Copy samples
	for(; recPos < *nsamples &&
		recPos < maxRecPos &&
		readPos < ( int ) msr->samplecnt;
		recPos++, readPos++, counter++ )
		*( samples + recPos ) = *( ( int* ) msr->datasamples + readPos );

	//printf(" Writing: %d; Curr. count: %d\n",
	//        counter, recPos);
	// printf("pv:  Writing: %d; Curr. count: %d\n",counter, recPos);

	return recPos;
}
*/

/*******************************************************************************
 ******************************************************************************/
int populateFloatSamples( float *samples, int *nsamples,
	hptime_t *starttime, hptime_t *endtime, int firstRecord, MSRecord *msr )
{
	int recPos;
	int readPos = 0;
	int counter = 0;
	int maxRecPos = *nsamples;

	//msr_print(msr, 0); //For debugging

	//Calculate position of the record start with respect to starttime
	recPos = ( int ) ( ( double ) ( msr->starttime - *starttime ) * msr->samprate / 1000000.0 + 0.5 );
	//printf("Original recPos: %d (%d)\n" , recPos, *nsamples);

	//Check if this is the first record to be dropped in the array
	if( firstRecord == 1 )
	{
		//It is! The starttime must be adjusted appropriately
		if( recPos >= 0 )
		{
			//In this case, the record starts after the original starttime
			//So the new starttime becomes the start of the record
			*starttime = msr->starttime;
			//In addition to correcting the starttime in this case, it is also
			//necessary to reset the recPos since now the record must be dropped
			//at exactly the start of the array
			recPos = 0;
		}
		else
		{
			//In this case, the record starts before the original starttime
			//So the new starttime has to be readjusted at a middle position
			//within the record
			*starttime = msr->starttime
				+ ( hptime_t ) ( ( double ) ( -recPos + 1 ) * 1000000.0 / msr->samprate );
		}
	}

	//Set read position and readjust recording position
	if( recPos < 0 )
	{
		readPos = -recPos;
		recPos = 0;
	}

	//Calculate maximum recording position according to endtime
	if( endtime != NULL )
		maxRecPos = ( int ) ( ( double ) ( *endtime - *starttime )
		* msr->samprate / 1000000.0 );


	//printf("Times %f <-> %f\n", (double)(*starttime), 
	//(double)(msr->starttime));
	//printf("RecPos: %d; ReadPos: %d; ",
	//        recPos, readPos);


	//Copy samples
	for(; recPos < *nsamples &&
		recPos < maxRecPos &&
		readPos < ( int ) msr->samplecnt;
		recPos++, readPos++, counter++ )
		*( samples + recPos ) = *( ( float* ) msr->datasamples + readPos );
		// v1 *( samples + recPos ) = *( ( float* ) msr->datasamples + readPos );
		// pv: *( samples + recPos ) = *( ( int* ) msr->datasamples + readPos );

	//printf(" Writing: %d; Curr. count: %d\n",
	//        counter, recPos);
	// printf("pv:  Writing: %d; Curr. count: %d\n",counter, recPos);

	return recPos;
}

/*******************************************************************************
 ******************************************************************************/
// int populateFloatSamples( float *samples, int *nsamples,
int populateIntegerSamples( float *samples, int *nsamples,
	hptime_t *starttime, hptime_t *endtime, int firstRecord, MSRecord *msr )
{
	int recPos;
	int readPos = 0;
	int counter = 0;
	int maxRecPos = *nsamples;

	//msr_print(msr, 0); //For debugging

	//Calculate position of the record start with respect to starttime
	recPos = ( int ) ( ( double ) ( msr->starttime - *starttime ) * msr->samprate / 1000000.0 + 0.5 );
	//printf("Original recPos: %d (%d)\n" , recPos, *nsamples);

	//Check if this is the first record to be dropped in the array
	if( firstRecord == 1 )
	{
		//It is! The starttime must be adjusted appropriately
		if( recPos >= 0 )
		{
			//In this case, the record starts after the original starttime
			//So the new starttime becomes the start of the record
			*starttime = msr->starttime;
			//In addition to correcting the starttime in this case, it is also
			//necessary to reset the recPos since now the record must be dropped
			//at exactly the start of the array
			recPos = 0;
		}
		else
		{
			//In this case, the record starts before the original starttime
			//So the new starttime has to be readjusted at a middle position
			//within the record
			*starttime = msr->starttime
				+ ( hptime_t ) ( ( double ) ( -recPos + 1 ) * 1000000.0 / msr->samprate );
		}
	}

	//Set read position and readjust recording position
	if( recPos < 0 )
	{
		readPos = -recPos;
		recPos = 0;
	}

	//Calculate maximum recording position according to endtime
	if( endtime != NULL )
		maxRecPos = ( int ) ( ( double ) ( *endtime - *starttime )
		* msr->samprate / 1000000.0 );


	//printf("Times %f <-> %f\n", (double)(*starttime), 
	//(double)(msr->starttime));
	//printf("RecPos: %d; ReadPos: %d; ",
	//        recPos, readPos);


	//Copy samples
	for(; recPos < *nsamples &&
		recPos < maxRecPos &&
		readPos < ( int ) msr->samplecnt;
		recPos++, readPos++, counter++ )
		*( samples + recPos ) = *( ( int* ) msr->datasamples + readPos );
		// v1 *( samples + recPos ) = *( ( float* ) msr->datasamples + readPos );
		// pv: *( samples + recPos ) = *( ( int* ) msr->datasamples + readPos );

	//printf(" Writing: %d; Curr. count: %d\n",
	//        counter, recPos);
	// printf("pv:  Writing: %d; Curr. count: %d\n",counter, recPos);

	return recPos;
}

/*******************************************************************************
 * Internal function to find a channel on an existing list of channel structs  *
 * from SEISAN. Returns pointer to channel or NULL                             *
 ******************************************************************************/
MSEED_CHANNEL *findExistingChannel(
	MSRecord *msr,
	MSEED_CHANNEL *channels, int nchannels )
{
	int i;
	for( i = 0; i < nchannels; i++ )
	{
		if( strcmp( msr->station, channels[i].station ) == 0 &&
			strcmp( msr->channel, channels[i].channel ) == 0 &&
			strcmp( msr->network, channels[i].network ) == 0 &&
			strcmp( msr->location, channels[i].location ) == 0 )

			return channels + i;
	}
	return NULL;
}

/*******************************************************************************
 * Internal function to trim a string                                          *
 ******************************************************************************/
char* strtrim( char *in )
{
	int i;
	for( i = strlen( in ) - 1; i >= 0; i-- )
	{
		if( in[i] == ' ' )
			in[i] = '\0';
		if( in[i] != '\0' )
			break;
	}
	return in;
}

/*******************************************************************************
 * Internal function to copy a set of characters from one string to another    *
 ******************************************************************************/
int strcpyl( char *s1, char *s2, int len )
{
	int i;
	for( i = 0; i < len; i++ )
	{
		s1[i] = s2[i];
	}
	s1[len] = '\0';
	return 0;
}

int strcpyx( char *s1, char *s2, int len )
{
	int i, ffl = 0;
	for( i = 0; i < len; i++ )
	{
		if( *( s2 + i ) == '\0' )
			ffl = 1;
		if( ffl == 0 )
		{
			*( s1 + i ) = *( s2 + i );
		}
		else
		{
			*( s1 + i ) = ' ';
		}
	}
	return 0;
}

/*******************************************************************************
 * Internal function to populate a date into year/month/day/hour/min/sec ints  *
 ******************************************************************************/
char ts[27];

int populateDate( hptime_t t, int *year, int *month, int *day, int *hour,
	int *min, float *sec )
{
	ms_hptime2isotimestr( t, ts, 1 );
	sscanf( ts, "%4d-%2d-%2dT%2d:%2d:%9f",
		year, month, day, hour, min, sec );
	return 0;
}

/*******************************************************************************
 * Internal function to extract samples or simply count the number of samples  *
 * from an mseed trace group. Returns the number of extracted samples. If      *
 * curSample is zero, does not copy samples, returning the number of samples.  *
 ******************************************************************************/
int copySamples_NEW_NOT_TO_USE( MSTraceGroup *mstg,
	int *curSample, hptime_t *starttime, hptime_t *endtime )
{
	//mst_printtracelist( mstg, 1, 1, 1); //For debuging

	/* Find first valid sample, looking for the earliest trace start,
	 * the start of the trace containing starttime, if it exists
	 ****************************************************************/
	MSTrace *mst = mstg->traces; //Initialize first trace in group
	hptime_t earliestStart = mst->starttime; //Initialize earliest starttime
	hptime_t startTrace = 0;

	/* Cycle all records, to search for earliest record starttime
	 ************************************************************/
	while( mst != 0 )
	{
		//Compare starttime of record with earliest and the start time
		//of the trace that contains the requested start timme
		if( mst->starttime < earliestStart )
			earliestStart = mst->starttime;
		//Check if trace contains starttime
		if( mst->starttime <= *starttime && *starttime <= mst->endtime )
			startTrace = mst->starttime;
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

	/* At this point, earliestStart contains the earliest starttime,
	 * which can be compared with the requested starttime.
	 * If the requested starttime is before the earliest record,
	 * then it must become it.
	 * Otherwise, the starttime must be ajusted exactly to the closest sample
	 ************************************************************************/
	int skipsamp = 0; //Number of samples to skip
	if( *starttime < earliestStart )
	{
		//This means that the first available record is after the requested
		//start time. So the starttime is shifted forwards to the first available sample
		*starttime = earliestStart;
	}
	else
	{
		//Compute the number of samples to skip in the first trace
		skipsamp = ( int ) ( ( double ) ( *starttime - startTrace ) * mstg->traces->samprate / 1000000 );
		//Compute new starttime
		*starttime = startTrace + ( hptime_t ) ( ( double ) skipsamp / mstg->traces->samprate * 1000000 );
	}

	//For debuging
	//printf("Skipping %d samples from the first trace\n", skipsamp);

	/* Cycle traces again to copy/count samples
	 * Note that the traces are not necessarily in time order
	 * So the position of each sample in the final vector must be calculated
	 ***********************************************************************/
	mst = mstg->traces;
	int i = 0;
	int curRecordStartSample = 0;
	int curRecordEndSample = 0;
	int samplePos = 0;
	int maxSamplePos = 0; //Position of the last sample -> corresponds to sample count

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
