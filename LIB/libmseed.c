/****************************************************************************
 *
 * Routines to manage files of Mini-SEED.
 *
 * Written by Chad Trabant
 *   IRIS Data Management Center
 *
 * modified: 2011.129
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "libmseed.h"

static int ms_fread (char *buf, int size, int num, FILE *stream);

/* Pack type parameters for the 8 defined types:
 * [type] : [hdrlen] [sizelen] [chksumlen]
 */
int8_t packtypes[9][3] = {
  { 0, 0, 0 },
  { 8, 8, 8 },
  { 11, 8, 8 },
  { 11, 8, 8 },
  { 11, 8, 8 },
  { 11, 8, 8 },
  { 13, 8, 8 },
  { 15, 8, 8 },
  { 22, 15, 10 }};

/*********************************************************************
 * Notes about packed files as read by ms_readmsr_main()
 *
 * In general a packed file includes a pack file identifier at the
 * very beginning, followed by pack header for a data block, followed
 * by the data block, followed by a chksum for the data block.  The
 * pack header, data block and chksum are then repeated for each data
 * block in the file:
 *
 *   ID    HDR     DATA    CHKSUM    HDR     DATA    CHKSUM
 * |----|-------|--....--|--------|-------|--....--|--------| ...
 *
 *      |________ repeats ________|
 *
 * The HDR section contains fixed width ASCII fields identifying the
 * data in the next section and it's length in bytes.  With this
 * information the offset of the next CHKSUM and HDR are completely
 * predictable.
 *
 * packtypes[type][0]: length of pack header length
 * packtypes[type][1]: length of size field in pack header
 * packtypes[type][2]: chksum length following data blocks, skipped
 *
 * Notes from seed_pack.h documenting the PQI and PLS pack types:
 *
 * ___________________________________________________________________
 * There were earlier pack file types numbered 1 through 6.  These have been discontinued.
 * Current file formats can be described as follows:
 *
 * Quality-Indexed Pack - Type 7:
 * _____10_____2__2___3_____8_______mod 256_______8_____2__2___3_____8_______mod 256_______8____ ...
 * |PQI-      |q |lc|chn|  size  | ...data... | chksum |q |lc|chn|  size  | ...data... | chksum  ...
 * parsing guide:
 *      10    |     15 hdr       |     xx     |   8    |    15 hdr        |    xx   
 *            |+0|+2|+4 |+7      |
 * 
 * 
 * Large-Size Pack - Type 8: (for large channel blocks)
 * _____10_____2__2___3_____15_______mod 256_______8____2__2__2___3_____15_______mod 256_______8____ ...
 * |PLS-------|q |lc|chn|  size  | ...data... | chksum |--|q |lc|chn|  size  | ...data... | chksum  ...
 * uniform parsing guide:
 * |    10    |       22         |    xx      |    10     |      22          |       xx   |
 *            |+0|+2|+4 |+7      |
 * (note the use of hyphens after the PLS marker and just after the checksum.  this will serve as a visual
 * aid when scanning between channel blocks and provide consistent 32 byte spacing between data blocks)
 * ___________________________________________________________________
 *
 *********************************************************************/

/* Initialize the global file reading parameters */
MSFileParam gMSFileParam = {NULL, "", NULL, 0, 0, 0, 0, 0, 0, 0};


/**********************************************************************
 * ms_readmsr:
 *
 * This routine is a simple wrapper for ms_readmsr_main() that uses
 * the global file reading parameters.  This routine is not thread
 * safe and cannot be used to read more than one file at a time.
 *
 * See the comments with ms_readmsr_main() for return values and
 * further description of arguments.
 *********************************************************************/
int
ms_readmsr (MSRecord **ppmsr, char *msfile, int reclen, off_t *fpos,
	    int *last, flag skipnotdata, flag dataflag, flag verbose)
{
  MSFileParam *msfp = &gMSFileParam;
  
  return ms_readmsr_main (&msfp, ppmsr, msfile, reclen, fpos,
			  last, skipnotdata, dataflag, NULL, verbose);
}  /* End of ms_readmsr() */


/**********************************************************************
 * ms_readmsr_r:
 *
 * This routine is a simple wrapper for ms_readmsr_main() that uses
 * the re-entrant capabilities.  This routine is thread safe and can
 * be used to read more than one file at a time as long as separate
 * MSFileParam structures are used for each file.
 *
 * See the comments with ms_readmsr_main() for return values and
 * further description of arguments.
 *********************************************************************/
int
ms_readmsr_r (MSFileParam **ppmsfp, MSRecord **ppmsr, char *msfile,
	      int reclen, off_t *fpos, int *last, flag skipnotdata,
	      flag dataflag, flag verbose)
{
  return ms_readmsr_main (ppmsfp, ppmsr, msfile, reclen, fpos,
			  last, skipnotdata, dataflag, NULL, verbose);
}  /* End of ms_readmsr_r() */


/**********************************************************************
 * ms_shift_msfp:
 *
 * A helper routine to shift (remove bytes from the beginning of) the
 * file reading buffer for a MSFP.  The buffer length, reading offset
 * and file position indicators are all updated as necessary.
 *
 *********************************************************************/
static void
ms_shift_msfp (MSFileParam *msfp, int shift)
{
  if ( ! msfp )
    return;
  
  if ( shift <= 0 && shift > msfp->readlen )
    {
      ms_log (2, "ms_shift_msfp(): Cannot shift buffer, shift: %d, readlen: %d, readoffset: %d\n",
	      shift, msfp->readlen, msfp->readoffset);
      return;
    }
  
  memmove (msfp->rawrec, msfp->rawrec + shift, msfp->readlen - shift);
  msfp->readlen -= shift;
  
  if ( shift < msfp->readoffset )
    {
      msfp->readoffset -= shift;
    }
  else
    {
      msfp->filepos += (shift - msfp->readoffset);
      msfp->readoffset = 0;
    }
  
  return;
}  /* End of ms_shift_msfp() */


/* Macro to calculate length of unprocessed buffer */
#define MSFPBUFLEN(MSFP) (MSFP->readlen - MSFP->readoffset)

/* Macro to return current reading position */
#define MSFPREADPTR(MSFP) (MSFP->rawrec + MSFP->readoffset)

/**********************************************************************
 * ms_readmsr_main:
 *
 * This routine will open and read, with subsequent calls, all
 * Mini-SEED records in specified file.
 *
 * All static file reading parameters are stored in a MSFileParam
 * struct and returned (via a pointer to a pointer) for the calling
 * routine to use in subsequent calls.  A MSFileParam struct will be
 * allocated if necessary.  This routine is thread safe and can be
 * used to read multiple files in parallel as long as the file reading
 * parameters are managed appropriately.
 *
 * If reclen is 0 or negative the length of every record is
 * automatically detected.  For auto detection of record length the
 * record must include a 1000 blockette or be followed by a valid
 * record header or end of file.
 *
 * If *fpos is not NULL it will be updated to reflect the file
 * position (offset from the beginning in bytes) from where the
 * returned record was read.  As a special case, if *fpos is not NULL
 * and the value it points to is less than 0 this will be interpreted
 * as a (positive) starting offset from which to begin reading data;
 * this feature does not work with packed files.
 *
 * If *last is not NULL it will be set to 1 when the last record in
 * the file is being returned, otherwise it will be 0.
 *
 * If the skipnotdata flag is true any data chunks read that do not
 * have valid data record indicators (D, R, Q, M, etc.) will be skipped.
 *
 * dataflag will be passed directly to msr_unpack().
 *
 * If a Selections list is supplied it will be used to determine when
 * a section of data in a packed file may be skipped, packed files are
 * internal to the IRIS DMC.
 *
 * After reading all the records in a file the controlling program
 * should call it one last time with msfile set to NULL.  This will
 * close the file and free allocated memory.
 *
 * Returns MS_NOERROR and populates an MSRecord struct at *ppmsr on
 * successful read, returns MS_ENDOFFILE on EOF, otherwise returns a
 * libmseed error code (listed in libmseed.h) and *ppmsr is set to
 * NULL.
 *********************************************************************/
int
ms_readmsr_main (MSFileParam **ppmsfp, MSRecord **ppmsr, char *msfile,
		 int reclen, off_t *fpos, int *last, flag skipnotdata,
		 flag dataflag, Selections *selections, flag verbose)
{
  MSFileParam *msfp;
  off_t packdatasize = 0;
  int packskipsize;
  int parseval = 0;
  int readsize = 0;
  int readcount = 0;
  int retcode = MS_NOERROR;
  
  if ( ! ppmsr )
    return MS_GENERROR;
  
  if ( ! ppmsfp )
    return MS_GENERROR;
  
  msfp = *ppmsfp;
  
  /* Initialize the file read parameters if needed */
  if ( ! msfp )
    {
      msfp = (MSFileParam *) malloc (sizeof (MSFileParam));
      
      if ( msfp == NULL )
	{
	  ms_log (2, "ms_readmsr_main(): Cannot allocate memory for MSFP\n");
	  return MS_GENERROR;
	}
      
      /* Redirect the supplied pointer to the allocated params */
      *ppmsfp = msfp;
      
      msfp->fp = NULL;
      msfp->filename[0] = '\0';
      msfp->rawrec = NULL;
      msfp->readlen = 0;
      msfp->readoffset = 0;
      msfp->packtype = 0;
      msfp->packhdroffset = 0;
      msfp->filepos = 0;
      msfp->filesize = 0;
      msfp->recordcount = 0;
    }
  
  /* When cleanup is requested */
  if ( msfile == NULL )
    {
      msr_free (ppmsr);
      
      if ( msfp->fp != NULL )
	fclose (msfp->fp);
      
      if ( msfp->rawrec != NULL )
	free (msfp->rawrec);
      
      /* If the file parameters are the global parameters reset them */
      if ( *ppmsfp == &gMSFileParam )
	{
	  gMSFileParam.fp = NULL;
	  gMSFileParam.filename[0] = '\0';
          gMSFileParam.rawrec = NULL;
	  gMSFileParam.readlen = 0;
	  gMSFileParam.readoffset = 0;
	  gMSFileParam.packtype = 0;
	  gMSFileParam.packhdroffset = 0;
	  gMSFileParam.filepos = 0;
	  gMSFileParam.filesize = 0;
	  gMSFileParam.recordcount = 0;
	}
      /* Otherwise free the MSFileParam */
      else
	{
	  free (*ppmsfp);
	  *ppmsfp = NULL;
	}
      
      return MS_NOERROR;
    }
  
  /* Allocate reading buffer */
  if ( msfp->rawrec == NULL )
    {
      if ( ! (msfp->rawrec = (char *) malloc (MAXRECLEN)) )
	{
	  ms_log (2, "ms_readmsr_main(): Cannot allocate memory for read buffer\n");
	  return MS_GENERROR;
	}
    }
  
  /* Sanity check: track if we are reading the same file */
  if ( msfp->fp && strncmp (msfile, msfp->filename, sizeof(msfp->filename)) )
    {
      ms_log (2, "ms_readmsr_main() called with a different file name without being reset\n");
      
      /* Close previous file and reset needed variables */
      if ( msfp->fp != NULL )
	fclose (msfp->fp);
      
      msfp->fp = NULL;
      msfp->readlen = 0;
      msfp->readoffset = 0;
      msfp->packtype = 0;
      msfp->packhdroffset = 0;
      msfp->filepos = 0;
      msfp->filesize = 0;
      msfp->recordcount = 0;
    }
  
  /* Open the file if needed, redirect to stdin if file is "-" */
  if ( msfp->fp == NULL )
    {
      /* Store the filename for tracking */
      strncpy (msfp->filename, msfile, sizeof(msfp->filename) - 1);
      msfp->filename[sizeof(msfp->filename) - 1] = '\0';
      
      if ( strcmp (msfile, "-") == 0 )
	{
	  msfp->fp = stdin;
	}
      else
	{
	  if ( (msfp->fp = fopen (msfile, "rb")) == NULL )
	    {
	      ms_log (2, "Cannot open file: %s (%s)\n", msfile, strerror (errno));
	      msr_free (ppmsr);
	      
	      return MS_GENERROR;
	    }
	  else
	    {
	      /* Determine file size */
	      struct stat sbuf;
	      
	      if ( fstat (fileno(msfp->fp), &sbuf) )
		{
		  ms_log (2, "Cannot open file: %s (%s)\n", msfile, strerror (errno));
		  msr_free (ppmsr);
		  
		  return MS_GENERROR;
		}
	      
	      msfp->filesize = sbuf.st_size;
	    }
	}
    }
  
  /* Seek to a specified offset if requested */
  if ( fpos != NULL && *fpos < 0 )
    {
      /* Only try to seek in real files, not stdin */
      if ( msfp->fp != stdin )
	{
	  if ( lmp_fseeko (msfp->fp, *fpos * -1, SEEK_SET) )
	    {
	      ms_log (2, "Cannot seek in file: %s (%s)\n", msfile, strerror (errno));
	      
	      return MS_GENERROR;
	    }
	  
	  msfp->filepos = *fpos * -1;
	  msfp->readlen = 0;
	  msfp->readoffset = 0;
	}
    }
  
  /* Zero the last record indicator */
  if ( last )
    *last = 0;
  
  /* Read data and search for records */
  for (;;)
    {
      /* Read more data into buffer if not at EOF and buffer has less than MINRECLEN
       * or more data is needed for the current record detected in buffer. */
      if ( ! feof(msfp->fp) && (MSFPBUFLEN(msfp) < MINRECLEN || parseval > 0) )
	{
	  /* Reset offsets if no unprocessed data in buffer */
	  if ( MSFPBUFLEN(msfp) <= 0 )
	    {
	      msfp->readlen = 0;
	      msfp->readoffset = 0;
	    }
	  /* Otherwise shift existing data to beginning of buffer */
	  else if ( msfp->readoffset > 0 )
	    {
	      ms_shift_msfp (msfp, msfp->readoffset);
	    }
	  
	  /* Determine read size */
	  readsize = (MAXRECLEN - msfp->readlen);
	  
	  /* Read data into record buffer */
	  readcount = ms_fread (msfp->rawrec + msfp->readlen, 1, readsize, msfp->fp);
	  
	  if ( readcount != readsize )
	    {
	      if ( ! feof (msfp->fp) )
		{
		  ms_log (2, "Short read of %d bytes starting from %lld\n",
			  readsize, msfp->filepos);
		  retcode = MS_GENERROR;
		  break;
		}
	    }
	  
	  /* Update read buffer length */
	  msfp->readlen += readcount;
	  
	  /* File position corresponding to start of buffer; not strictly necessary */
	  if ( msfp->fp != stdin )
	    msfp->filepos = lmp_ftello (msfp->fp) - msfp->readlen;
	}
      
      /* Test for packed file signature at the beginning of the file */
      if ( msfp->filepos == 0 && *(MSFPREADPTR(msfp)) == 'P' && MSFPBUFLEN(msfp) >= 48 )
	{
	  msfp->packtype = 0;
	  
	  /* Determine pack type, the negative pack type indicates initial header */
	  if ( ! memcmp ("PED", MSFPREADPTR(msfp), 3) )
	    msfp->packtype = -1;
	  else if ( ! memcmp ("PSD", MSFPREADPTR(msfp), 3) )
	    msfp->packtype = -2;
	  else if ( ! memcmp ("PLC", MSFPREADPTR(msfp), 3) )
	    msfp->packtype = -6;
	  else if ( ! memcmp ("PQI", MSFPREADPTR(msfp), 3) )
	    msfp->packtype = -7;
	  else if ( ! memcmp ("PLS", MSFPREADPTR(msfp), 3) )
	    msfp->packtype = -8;
	  
	  if ( verbose > 0 )
	    ms_log (1, "Detected packed file (%3.3s: type %d)\n", MSFPREADPTR(msfp), -msfp->packtype);
	}
      
      /* Read pack headers, initial and subsequent headers including (ignored) chksum values */
      if ( msfp->packtype && (msfp->packtype < 0 || msfp->filepos == msfp->packhdroffset) && MSFPBUFLEN(msfp) >= 48 )
	{
	  char hdrstr[30];
	  long long datasize;
	  
	  /* Determine bytes to skip before header: either initial ID block or type-specific chksum block */
	  packskipsize = ( msfp->packtype < 0 ) ? 10 : packtypes[msfp->packtype][2];
	  
	  if ( msfp->packtype < 0 )
	    msfp->packtype = -msfp->packtype;
	  
	  /* Read pack length from pack header accounting for bytes that should be skipped */
	  memset (hdrstr, 0, sizeof(hdrstr));
	  memcpy (hdrstr, MSFPREADPTR(msfp) + (packtypes[msfp->packtype][0] + packskipsize - packtypes[msfp->packtype][1]),
		  packtypes[msfp->packtype][1]);
	  sscanf (hdrstr, " %lld", &datasize);
	  packdatasize = (off_t) datasize;
	  
	  /* Next pack header = File position + skipsize + header size + data size
	   * This offset is actually to the data block chksum which is skipped by the logic above,
	   * the next pack header should directly follow the chksum. */
	  msfp->packhdroffset = msfp->filepos + packskipsize + packtypes[msfp->packtype][0] + packdatasize;
	  
	  if ( verbose > 1 )
	    ms_log (1, "Read packed file header at offset %lld (%d bytes follow), chksum offset: %lld\n",
		    (long long int) (msfp->filepos + packskipsize), packdatasize,
		    (long long int) msfp->packhdroffset);
	  
	  /* Shift buffer to new reading offset (aligns records in buffer) */
	  ms_shift_msfp (msfp, msfp->readoffset + (packskipsize + packtypes[msfp->packtype][0]));
	} /* End of packed header processing */
      
      /* Check for match if selections are supplied and pack header was read, */
      /* only when enough data is in buffer and not reading from stdin pipe */
      if ( selections && msfp->packtype && packdatasize && MSFPBUFLEN(msfp) >= 48 && msfp->fp != stdin )
	{
	  char srcname[100];
	  
	  ms_recsrcname (MSFPREADPTR(msfp), srcname, 1);
	  
	  if ( ! ms_matchselect (selections, srcname, HPTERROR, HPTERROR, NULL) )
	    {
	      /* Update read position if next section is in buffer */
	      if ( MSFPBUFLEN(msfp) >= (msfp->packhdroffset - msfp->filepos) )
		{
		  if ( verbose > 1 )
		    {
		      ms_log (1, "Skipping (jump) packed section for %s (%d bytes) starting at offset %lld\n",
			      srcname, (msfp->packhdroffset - msfp->filepos), (long long int) msfp->filepos);
		    }
		  
		  msfp->readoffset += (msfp->packhdroffset - msfp->filepos);
		  msfp->filepos = msfp->packhdroffset;
		  packdatasize = 0;
		}
	      
	      /* Otherwise seek to next pack header and reset reading position */
	      else
		{
		  if ( verbose > 1 )
		    {
		      ms_log (1, "Skipping (seek) packed section for %s (%d bytes) starting at offset %lld\n",
			      srcname, (msfp->packhdroffset - msfp->filepos), (long long int) msfp->filepos);
		    }

		  if ( lmp_fseeko (msfp->fp, msfp->packhdroffset, SEEK_SET) )
		    {
		      ms_log (2, "Cannot seek in file: %s (%s)\n", msfile, strerror (errno));
		      
		      return MS_GENERROR;
		      break;
		    }
		  
		  msfp->filepos = msfp->packhdroffset;
		  msfp->readlen = 0;
		  msfp->readoffset = 0;
		  packdatasize = 0;
		}
	      
	      /* Return to top of loop for proper pack header handling */
	      continue;
	    }
	} /* End of selection processing */
      
      /* Attempt to parse record from buffer */
      if ( MSFPBUFLEN(msfp) >= MINRECLEN )
	{
	  int parselen = MSFPBUFLEN(msfp);
	  
	  /* Limit the parse length to offset of pack header if present in the buffer */
	  if ( msfp->packhdroffset && msfp->packhdroffset < (msfp->filepos + MSFPBUFLEN(msfp)) )
	    parselen = msfp->packhdroffset - msfp->filepos;
	  
 	  parseval = msr_parse (MSFPREADPTR(msfp), parselen, ppmsr, reclen, dataflag, verbose);
	  
	  /* Record detected and parsed */
	  if ( parseval == 0 )
	    {
	      if ( verbose > 1 )
		ms_log (1, "Read record length of %d bytes\n", (*ppmsr)->reclen);
	      
	      /* Test if this is the last record if file size is known (not pipe) */
	      if ( last && msfp->filesize )
		if ( (msfp->filesize - (msfp->filepos + (*ppmsr)->reclen)) < MINRECLEN )
		  *last = 1;
	      
	      /* Return file position for this record */
	      if ( fpos )
		*fpos = msfp->filepos;
	      
	      /* Update reading offset, file position and record count */
	      msfp->readoffset += (*ppmsr)->reclen;
	      msfp->filepos += (*ppmsr)->reclen;
	      msfp->recordcount++;
	      
	      retcode = MS_NOERROR;
	      break;
	    }
	  else if ( parseval < 0 )
	    {
	      /* Skip non-data if requested */ 
	      if ( skipnotdata )
		{
		  if ( verbose > 1 )
		    {
		      if ( MS_ISVALIDBLANK((char *)MSFPREADPTR(msfp)) )
			ms_log (1, "Skipped %d bytes of blank/noise record at byte offset %lld\n",
				MINRECLEN, (long long) msfp->filepos);
		      else
			ms_log (1, "Skipped %d bytes of non-data record at byte offset %lld\n",
				MINRECLEN, (long long) msfp->filepos);
		    }
		  
		  /* Skip MINRECLEN bytes, update reading offset and file position */
		  msfp->readoffset += MINRECLEN;
		  msfp->filepos += MINRECLEN;
		}
	      /* Parsing errors */ 
	      else
		{
		  ms_log (2, "Cannot detect record at byte offset %lld: %s\n",
			  (long long) msfp->filepos, msfile);
		  
		  /* Print common errors and raw details if verbose */
		  ms_parse_raw (MSFPREADPTR(msfp), MSFPBUFLEN(msfp), verbose, -1);
		  
		  retcode = parseval;
		  break;
		}
	    }
	  else /* parseval > 0 (found record but need more data) */
	    {
	      /* Determine implied record length if needed */
	      int32_t impreclen = reclen;
	      
	      /* Check for parse hints that are larger than MAXRECLEN */
	      if ( (MSFPBUFLEN(msfp) + parseval) > MAXRECLEN )
		{
		  if ( skipnotdata )
		    {
		      /* Skip MINRECLEN bytes, update reading offset and file position */
		      msfp->readoffset += MINRECLEN;
		      msfp->filepos += MINRECLEN;
		    }
		  else
		    {
		      retcode = MS_OUTOFRANGE;
		      break;
		    }
		}
	      
	      /* Pack header check, if pack header offset is within buffer */
	      else if ( impreclen <= 0 && msfp->packhdroffset &&
			msfp->packhdroffset < (msfp->filepos + MSFPBUFLEN(msfp)) )
		{
		  impreclen = msfp->packhdroffset - msfp->filepos;
		  
		  /* Check that record length is within range and a power of 2.
		   * Power of two if (X & (X - 1)) == 0 */
		  if ( impreclen >= MINRECLEN && impreclen <= MAXRECLEN &&
		       (impreclen & (impreclen - 1)) == 0 )
		    {
		      /* Set the record length implied by the next pack header */
		      reclen = impreclen;
		    }
		  else
		    {
		      ms_log (1, "Implied record length (%d) is invalid\n", impreclen);
		      
		      retcode = MS_NOTSEED;
		      break;
		    }
		}
	      
	      /* End of file check */
	      else if ( impreclen <= 0 && feof (msfp->fp) )
		{
		  impreclen = msfp->filesize - msfp->filepos;
		  
		  /* Check that record length is within range and a power of 2.
		   * Power of two if (X & (X - 1)) == 0 */
		  if ( impreclen >= MINRECLEN && impreclen <= MAXRECLEN &&
		       (impreclen & (impreclen - 1)) == 0 )
		    {
		      /* Set the record length implied by the end of the file */
		      reclen = impreclen;
		    }
		  /* Otherwise a trucated record */
		  else
		    {
		      if ( verbose )
			{
			  if ( msfp->filesize )
			    ms_log (1, "Truncated record at byte offset %lld, filesize %d: %s\n",
				    (long long) msfp->filepos, msfp->filesize, msfile);
			  else
			    ms_log (1, "Truncated record at byte offset %lld\n",
				    (long long) msfp->filepos);
			}
		      
		      retcode = MS_ENDOFFILE;
		      break;
		    }
		}
	    }
	}  /* End of record detection */
      
      /* Finished when within MINRECLEN from EOF and buffer less than MINRECLEN */
      if ( (msfp->filesize - msfp->filepos) < MINRECLEN && MSFPBUFLEN(msfp) < MINRECLEN )
	{
	  if ( msfp->recordcount == 0 && msfp->packtype == 0 )
	    {
	      if ( verbose > 0 )
		ms_log (2, "%s: No data records read, not SEED?\n", msfile);
	      retcode = MS_NOTSEED;
	    }
	  else
	    {
	      retcode = MS_ENDOFFILE;
	    }
	  
	  break;
	}
    }  /* End of reading, record detection and parsing loop */
  
  /* Cleanup target MSRecord if returning an error */
  if ( retcode != MS_NOERROR )
    {
      msr_free (ppmsr);
    }
  
  return retcode;
}  /* End of ms_readmsr_main() */


/*********************************************************************
 * ms_readtraces:
 *
 * This is a simple wrapper for ms_readtraces_selection() that uses no
 * selections.
 *
 * See the comments with ms_readtraces_selection() for return values
 * and further description of arguments.
 *********************************************************************/
int
ms_readtraces (MSTraceGroup **ppmstg, char *msfile, int reclen,
	       double timetol, double sampratetol, flag dataquality,
	       flag skipnotdata, flag dataflag, flag verbose)
{
  return ms_readtraces_selection (ppmstg, msfile, reclen,
				  timetol, sampratetol, NULL,
				  dataquality, skipnotdata,
				  dataflag, verbose);
}  /* End of ms_readtraces() */


/*********************************************************************
 * ms_readtraces_timewin:
 *
 * This is a wrapper for ms_readtraces_selection() that creates a
 * simple selection for a specified time window.
 *
 * See the comments with ms_readtraces_selection() for return values
 * and further description of arguments.
 *********************************************************************/
int
ms_readtraces_timewin (MSTraceGroup **ppmstg, char *msfile, int reclen,
		       double timetol, double sampratetol,
		       hptime_t starttime, hptime_t endtime, flag dataquality,
		       flag skipnotdata, flag dataflag, flag verbose)
{
  Selections selection;
  SelectTime selecttime;
  
  selection.srcname[0] = '*';
  selection.srcname[1] = '\0';
  selection.timewindows = &selecttime;
  selection.next = NULL;
  
  selecttime.starttime = starttime;
  selecttime.endtime = endtime;
  selecttime.next = NULL;
  
  return ms_readtraces_selection (ppmstg, msfile, reclen,
				  timetol, sampratetol, &selection,
				  dataquality, skipnotdata,
				  dataflag, verbose);
}  /* End of ms_readtraces_timewin() */


/*********************************************************************
 * ms_readtraces_selection:
 *
 * This routine will open and read all Mini-SEED records in specified
 * file and populate a trace group.  This routine is thread safe.
 *
 * If reclen is <= 0 the length of every record is automatically
 * detected.
 *
 * If a Selections list is supplied it will be used to limit which
 * records are added to the trace group.
 *
 * Returns MS_NOERROR and populates an MSTraceGroup struct at *ppmstg
 * on successful read, otherwise returns a libmseed error code (listed
 * in libmseed.h).
 *********************************************************************/
int
ms_readtraces_selection (MSTraceGroup **ppmstg, char *msfile, int reclen,
			 double timetol, double sampratetol,
			 Selections *selections, flag dataquality,
			 flag skipnotdata, flag dataflag, flag verbose)
{
  MSRecord *msr = 0;
  MSFileParam *msfp = 0;
  int retcode;
  
  if ( ! ppmstg )
    return MS_GENERROR;
  
  /* Initialize MSTraceGroup if needed */
  if ( ! *ppmstg )
    {
      *ppmstg = mst_initgroup (*ppmstg);
      
      if ( ! *ppmstg )
	return MS_GENERROR;
    }
  
  /* Loop over the input file */
  while ( (retcode = ms_readmsr_main (&msfp, &msr, msfile, reclen, NULL, NULL,
				      skipnotdata, dataflag, NULL, verbose)) == MS_NOERROR)
    {
      /* Test against selections if supplied */
      if ( selections )
	{
	  char srcname[50];
	  hptime_t endtime;
	  
	  msr_srcname (msr, srcname, 1);
	  endtime = msr_endtime (msr);
	  
	  if ( ms_matchselect (selections, srcname, msr->starttime, endtime, NULL) == NULL )
	    {
	      continue;
	    }
	}
      
      /* Add to trace group */
      mst_addmsrtogroup (*ppmstg, msr, dataquality, timetol, sampratetol);
    }
  
  /* Reset return code to MS_NOERROR on successful read by ms_readmsr() */
  if ( retcode == MS_ENDOFFILE )
    retcode = MS_NOERROR;
  
  ms_readmsr_main (&msfp, &msr, NULL, 0, NULL, NULL, 0, 0, NULL, 0);
  
  return retcode;
}  /* End of ms_readtraces_selection() */


/*********************************************************************
 * ms_readtracelist:
 *
 * This is a simple wrapper for ms_readtracelist_selection() that uses
 * no selections.
 *
 * See the comments with ms_readtracelist_selection() for return
 * values and further description of arguments.
 *********************************************************************/
int
ms_readtracelist (MSTraceList **ppmstl, char *msfile, int reclen,
		  double timetol, double sampratetol, flag dataquality,
		  flag skipnotdata, flag dataflag, flag verbose)
{
  return ms_readtracelist_selection (ppmstl, msfile, reclen,
				     timetol, sampratetol, NULL,
				     dataquality, skipnotdata,
				     dataflag, verbose);
}  /* End of ms_readtracelist() */


/*********************************************************************
 * ms_readtracelist_timewin:
 *
 * This is a wrapper for ms_readtraces_selection() that creates a
 * simple selection for a specified time window.
 *
 * See the comments with ms_readtraces_selection() for return values
 * and further description of arguments.
 *********************************************************************/
int
ms_readtracelist_timewin (MSTraceList **ppmstl, char *msfile, int reclen,
			  double timetol, double sampratetol,
			  hptime_t starttime, hptime_t endtime, flag dataquality,
			  flag skipnotdata, flag dataflag, flag verbose)
{
  Selections selection;
  SelectTime selecttime;
  
  selection.srcname[0] = '*';
  selection.srcname[1] = '\0';
  selection.timewindows = &selecttime;
  selection.next = NULL;
  
  selecttime.starttime = starttime;
  selecttime.endtime = endtime;
  selecttime.next = NULL;
  
  return ms_readtracelist_selection (ppmstl, msfile, reclen,
				     timetol, sampratetol, &selection,
				     dataquality, skipnotdata,
				     dataflag, verbose);
}  /* End of ms_readtracelist_timewin() */


/*********************************************************************
 * ms_readtracelist_selection:
 *
 * This routine will open and read all Mini-SEED records in specified
 * file and populate a trace list.  This routine is thread safe.
 *
 * If reclen is <= 0 the length of every record is automatically
 * detected.
 *
 * If a Selections list is supplied it will be used to limit which
 * records are added to the trace list.
 *
 * Returns MS_NOERROR and populates an MSTraceList struct at *ppmstl
 * on successful read, otherwise returns a libmseed error code (listed
 * in libmseed.h).
 *********************************************************************/
int
ms_readtracelist_selection (MSTraceList **ppmstl, char *msfile, int reclen,
			    double timetol, double sampratetol,
			    Selections *selections, flag dataquality,
			    flag skipnotdata, flag dataflag, flag verbose)
{
  MSRecord *msr = 0;
  MSFileParam *msfp = 0;
  int retcode;
  
  if ( ! ppmstl )
    return MS_GENERROR;
  
  /* Initialize MSTraceList if needed */
  if ( ! *ppmstl )
    {
      *ppmstl = mstl_init (*ppmstl);
      
      if ( ! *ppmstl )
	return MS_GENERROR;
    }
  
  /* Loop over the input file */
  while ( (retcode = ms_readmsr_main (&msfp, &msr, msfile, reclen, NULL, NULL,
				      skipnotdata, dataflag, NULL, verbose)) == MS_NOERROR)
    {
      /* Test against selections if supplied */
      if ( selections )
	{
	  char srcname[50];
	  hptime_t endtime;
	  
	  msr_srcname (msr, srcname, 1);
	  endtime = msr_endtime (msr);
	  
	  if ( ms_matchselect (selections, srcname, msr->starttime, endtime, NULL) == NULL )
	    {
	      continue;
	    }
	}
      
      /* Add to trace list */
      mstl_addmsr (*ppmstl, msr, dataquality, 1, timetol, sampratetol);
    }
  
  /* Reset return code to MS_NOERROR on successful read by ms_readmsr() */
  if ( retcode == MS_ENDOFFILE )
    retcode = MS_NOERROR;
  
  ms_readmsr_main (&msfp, &msr, NULL, 0, NULL, NULL, 0, 0, NULL, 0);
  
  return retcode;
}  /* End of ms_readtracelist_selection() */


/*********************************************************************
 * ms_fread:
 *
 * A wrapper for fread that handles EOF and error conditions.
 *
 * Returns the return value from fread.
 *********************************************************************/
static int
ms_fread (char *buf, int size, int num, FILE *stream)
{
  int read = 0;
  
  read = fread (buf, size, num, stream);
  
  if ( read <= 0 && size && num )
    {
      if ( ferror (stream) )
	ms_log (2, "ms_fread(): Cannot read input file\n");
      
      else if ( ! feof (stream) )
	ms_log (2, "ms_fread(): Unknown return from fread()\n");
    }
  
  return read;
}  /* End of ms_fread() */


/***************************************************************************
 * ms_record_handler_int:
 *
 * Internal record handler.  The handler data should be a pointer to
 * an open file descriptor to which records will be written.
 *
 ***************************************************************************/
static void
ms_record_handler_int (char *record, int reclen, void *ofp)
{
  if ( fwrite(record, reclen, 1, (FILE *)ofp) != 1 )
    {
      ms_log (2, "Error writing to output file\n");
    }
}  /* End of ms_record_handler_int() */


/***************************************************************************
 * msr_writemseed:
 *
 * Pack MSRecord data into Mini-SEED record(s) by calling msr_pack() and
 * write to a specified file.
 *
 * Returns the number of records written on success and -1 on error.
 ***************************************************************************/
int
msr_writemseed ( MSRecord *msr, char *msfile, flag overwrite, int reclen,
		 flag encoding, flag byteorder, flag verbose )
{
  FILE *ofp;
  char srcname[50];
  char *perms = (overwrite) ? "wb":"ab";
  int packedrecords = 0;
  
  if ( ! msr || ! msfile )
    return -1;
  
  /* Open output file or use stdout */
  if ( strcmp (msfile, "-") == 0 )
    {
      ofp = stdout;
    }
  else if ( (ofp = fopen (msfile, perms)) == NULL )
    {
      ms_log (1, "Cannot open output file %s: %s\n", msfile, strerror(errno));
      
      return -1;
    }
  
  /* Pack the MSRecord */
  if ( msr->numsamples > 0 )
    {
      msr->encoding = encoding;
      msr->reclen = reclen;
      msr->byteorder = byteorder;
      
      packedrecords = msr_pack (msr, &ms_record_handler_int, ofp, NULL, 1, verbose-1);
      
      if ( packedrecords < 0 )
        {
	  msr_srcname (msr, srcname, 1);
          ms_log (1, "Cannot write Mini-SEED for %s\n", srcname);
        }
    }
  
  /* Close file and return record count */
  fclose (ofp);
  
  return (packedrecords >= 0) ? packedrecords : -1;
}  /* End of msr_writemseed() */


/***************************************************************************
 * mst_writemseed:
 *
 * Pack MSTrace data into Mini-SEED records by calling mst_pack() and
 * write to a specified file.
 *
 * Returns the number of records written on success and -1 on error.
 ***************************************************************************/
int
mst_writemseed ( MSTrace *mst, char *msfile, flag overwrite, int reclen,
		 flag encoding, flag byteorder, flag verbose )
{
  FILE *ofp;
  char srcname[50];
  char *perms = (overwrite) ? "wb":"ab";
  int packedrecords = 0;
  
  if ( ! mst || ! msfile )
    return -1;
  
  /* Open output file or use stdout */
  if ( strcmp (msfile, "-") == 0 )
    {
      ofp = stdout;
    }
  else if ( (ofp = fopen (msfile, perms)) == NULL )
    {
      ms_log (1, "Cannot open output file %s: %s\n", msfile, strerror(errno));
      
      return -1;
    }
  
  /* Pack the MSTrace */
  if ( mst->numsamples > 0 )
    {
      packedrecords = mst_pack (mst, &ms_record_handler_int, ofp, reclen, encoding,
				byteorder, NULL, 1, verbose-1, NULL);
      
      if ( packedrecords < 0 )
        {
	  mst_srcname (mst, srcname, 1);
          ms_log (1, "Cannot write Mini-SEED for %s\n", srcname);
        }
    }
  
  /* Close file and return record count */
  fclose (ofp);
  
  return (packedrecords >= 0) ? packedrecords : -1;
}  /* End of mst_writemseed() */


/***************************************************************************
 * mst_writemseedgroup:
 *
 * Pack MSTraceGroup data into Mini-SEED records by calling mst_pack()
 * for each MSTrace in the group and write to a specified file.
 *
 * Returns the number of records written on success and -1 on error.
 ***************************************************************************/
int
mst_writemseedgroup ( MSTraceGroup *mstg, char *msfile, flag overwrite,
		      int reclen, flag encoding, flag byteorder, flag verbose )
{
  MSTrace *mst;
  FILE *ofp;
  char srcname[50];
  char *perms = (overwrite) ? "wb":"ab";
  int trpackedrecords;
  int packedrecords = 0;
  
  if ( ! mstg || ! msfile )
    return -1;
  
  /* Open output file or use stdout */
  if ( strcmp (msfile, "-") == 0 )
    {
      ofp = stdout;
    }
  else if ( (ofp = fopen (msfile, perms)) == NULL )
    {
      ms_log (1, "Cannot open output file %s: %s\n", msfile, strerror(errno));
      
      return -1;
    }
  
  /* Pack each MSTrace in the group */
  mst = mstg->traces;
  while ( mst )
    {
      if ( mst->numsamples <= 0 )
        {
          mst = mst->next;
          continue;
        }
      
      trpackedrecords = mst_pack (mst, &ms_record_handler_int, ofp, reclen, encoding,
                                  byteorder, NULL, 1, verbose-1, NULL);
      
      if ( trpackedrecords < 0 )
        {
	  mst_srcname (mst, srcname, 1);
          ms_log (1, "Cannot write Mini-SEED for %s\n", srcname);
        }
      else
        {
          packedrecords += trpackedrecords;
        }
      
      mst = mst->next;
    }
  
  /* Close file and return record count */
  fclose (ofp);
  
  return packedrecords;
}  /* End of mst_writemseedgroup() */

/***************************************************************************
 * genutils.c
 *
 * Generic utility routines
 *
 * Written by Chad Trabant
 * ORFEUS/EC-Project MEREDIAN
 * IRIS Data Management Center
 *
 * modified: 2012.114
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "lmplatform.h"
#include "libmseed.h"

static hptime_t ms_time2hptime_int (int year, int day, int hour,
				    int min, int sec, int usec);

static struct tm *ms_gmtime_r (int64_t *timep, struct tm *result);


/***************************************************************************
 * ms_recsrcname:
 *
 * Generate a source name string for a specified raw data record in
 * the format: 'NET_STA_LOC_CHAN' or, if the quality flag is true:
 * 'NET_STA_LOC_CHAN_QUAL'.  The passed srcname must have enough room
 * for the resulting string.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
ms_recsrcname (char *record, char *srcname, flag quality)
{
  struct fsdh_s *fsdh;
  char network[6];
  char station[6];
  char location[6];
  char channel[6];
  
  if ( ! record )
    return NULL;
  
  fsdh = (struct fsdh_s *) record;
  
  ms_strncpclean (network, fsdh->network, 2);
  ms_strncpclean (station, fsdh->station, 5);
  ms_strncpclean (location, fsdh->location, 2);
  ms_strncpclean (channel, fsdh->channel, 3);
  
  /* Build the source name string including the quality indicator*/
  if ( quality )
    sprintf (srcname, "%s_%s_%s_%s_%c",
             network, station, location, channel, fsdh->dataquality);
  
  /* Build the source name string without the quality indicator*/
  else
    sprintf (srcname, "%s_%s_%s_%s", network, station, location, channel);
  
  return srcname;
} /* End of ms_recsrcname() */


/***************************************************************************
 * ms_splitsrcname:
 *
 * Split srcname into separate components: "NET_STA_LOC_CHAN[_QUAL]".
 * Memory for each component must already be allocated.  If a specific
 * component is not desired set the appropriate argument to NULL.
 *
 * Returns 0 on success and -1 on error.
 ***************************************************************************/
int
ms_splitsrcname (char *srcname, char *net, char *sta, char *loc, char *chan,
		 char *qual)
{
  char *id;
  char *ptr, *top, *next;
  int sepcnt = 0;
  
  if ( ! srcname )
    return -1;
  
  /* Verify number of separating underscore characters */
  id = srcname;
  while ( (id = strchr (id, '_')) )
    {
      id++;
      sepcnt++;
    }
  
  /* Either 3 or 4 separating underscores are required */
  if ( sepcnt != 3 && sepcnt != 4 )
    {
      return -1;
    }
  
  /* Duplicate srcname */
  if ( ! (id = strdup(srcname)) )
    {
      fprintf (stderr, "ms_splitsrcname(): Error duplicating srcname string");
      return -1;
    }
  
  /* Network */
  top = id;
  if ( (ptr = strchr (top, '_')) )
    {
      next = ptr + 1;
      *ptr = '\0';
      
      if ( net )
	strcpy (net, top);
      
      top = next;
    }
  /* Station */
  if ( (ptr = strchr (top, '_')) )
    {
      next = ptr + 1;
      *ptr = '\0';
      
      if ( sta )
	strcpy (sta, top);
      
      top = next;
    }
  /* Location */
  if ( (ptr = strchr (top, '_')) )
    {
      next = ptr + 1;
      *ptr = '\0';
      
      if ( loc )
	strcpy (loc, top);
      
      top = next;
    }
  /* Channel & optional Quality */
  if ( (ptr = strchr (top, '_')) )
    {
      next = ptr + 1;
      *ptr = '\0';
      
      if ( chan )
	strcpy (chan, top);
      
      top = next;
      
      /* Quality */
      if ( *top && qual )
	{
	  /* Quality is a single character */
	  *qual = *top;
	}
    }
  /* Otherwise only Channel */
  else if ( *top && chan )
    {
      strcpy (chan, top);
    }
  
  /* Free duplicated stream ID */
  if ( id )
    free (id);
  
  return 0;
}  /* End of ms_splitsrcname() */


/***************************************************************************
 * ms_strncpclean:
 *
 * Copy up to 'length' characters from 'source' to 'dest' while
 * removing all spaces.  The result is left justified and always null
 * terminated.  The destination string must have enough room needed
 * for the non-space characters within 'length' and the null
 * terminator, a maximum of 'length + 1'.
 * 
 * Returns the number of characters (not including the null terminator) in
 * the destination string.
 ***************************************************************************/
int
ms_strncpclean (char *dest, const char *source, int length)
{
  int sidx, didx;
  
  if ( ! dest )
    return 0;
  
  if ( ! source )
    {
      *dest = '\0';
      return 0;
    }

  for ( sidx=0, didx=0; sidx < length ; sidx++ )
    {
      if ( *(source+sidx) == '\0' )
	{
	  break;
	}

      if ( *(source+sidx) != ' ' )
	{
	  *(dest+didx) = *(source+sidx);
	  didx++;
	}
    }

  *(dest+didx) = '\0';
  
  return didx;
}  /* End of ms_strncpclean() */


/***************************************************************************
 * ms_strncpcleantail:
 *
 * Copy up to 'length' characters from 'source' to 'dest' without any
 * trailing spaces.  The result is left justified and always null
 * terminated.  The destination string must have enough room needed
 * for the characters within 'length' and the null terminator, a
 * maximum of 'length + 1'.
 *
 * Returns the number of characters (not including the null terminator) in
 * the destination string.
 ***************************************************************************/
int
ms_strncpcleantail (char *dest, const char *source, int length)
{
  int idx, pretail;
  
  if ( ! dest )
    return 0;
  
  if ( ! source )
    {
      *dest = '\0';
      return 0;
    }
  
  *(dest+length) = '\0';
  
  pretail = 0;
  for ( idx=length-1; idx >= 0 ; idx-- )
    {
      if ( ! pretail && *(source+idx) == ' ' )
	{
	  *(dest+idx) = '\0';
	}
      else
	{
	  pretail++;
	  *(dest+idx) = *(source+idx);
	}
    }
  
  return pretail;
}  /* End of ms_strncpcleantail() */


/***************************************************************************
 * ms_strncpopen:
 *
 * Copy 'length' characters from 'source' to 'dest', padding the right
 * side with spaces and leave open-ended.  The result is left
 * justified and *never* null terminated (the open-ended part).  The
 * destination string must have enough room for 'length' characters.
 * 
 * Returns the number of characters copied from the source string.
 ***************************************************************************/
int
ms_strncpopen (char *dest, const char *source, int length)
{
  int didx;
  int dcnt = 0;
  int term = 0;
  
  if ( ! dest )
    return 0;
  
  if ( ! source )
    {
      for ( didx=0; didx < length ; didx++ )
	{
	  *(dest+didx) = ' ';
	}
      
      return 0;
    }
  
  for ( didx=0; didx < length ; didx++ )
    {
      if ( !term )
	if ( *(source+didx) == '\0' )
	  term = 1;
      
      if ( !term )
	{
	  *(dest+didx) = *(source+didx);
	  dcnt++;
	}
      else
	{
	  *(dest+didx) = ' ';
	}
    }
  
  return dcnt;
}  /* End of ms_strncpopen() */


/***************************************************************************
 * ms_doy2md:
 *
 * Compute the month and day-of-month from a year and day-of-year.
 *
 * Year is expected to be in the range 1800-5000, jday is expected to
 * be in the range 1-366, month will be in the range 1-12 and mday
 * will be in the range 1-31.
 *
 * Returns 0 on success and -1 on error.
 ***************************************************************************/
int
ms_doy2md(int year, int jday, int *month, int *mday)
{
  int idx;
  int leap;
  int days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  
  /* Sanity check for the supplied year */
  if ( year < 1800 || year > 5000 )
    {
      ms_log (2, "ms_doy2md(): year (%d) is out of range\n", year);
      return -1;
    }
  
  /* Test for leap year */
  leap = ( ((year%4 == 0) && (year%100 != 0)) || (year%400 == 0) ) ? 1 : 0;

  /* Add a day to February if leap year */
  if ( leap )
    days[1]++;

  if (jday > 365+leap || jday <= 0)
    {
      ms_log (2, "ms_doy2md(): day-of-year (%d) is out of range\n", jday);
      return -1;
    }
    
  for ( idx=0; idx < 12; idx++ )
    {
      jday -= days[idx];

      if ( jday <= 0 )
	{
	  *month = idx + 1;
	  *mday = days[idx] + jday;
	  break;
	}
    }

  return 0;
}  /* End of ms_doy2md() */


/***************************************************************************
 * ms_md2doy:
 *
 * Compute the day-of-year from a year, month and day-of-month.
 *
 * Year is expected to be in the range 1800-5000, month is expected to
 * be in the range 1-12, mday is expected to be in the range 1-31 and
 * jday will be in the range 1-366.
 *
 * Returns 0 on success and -1 on error.
 ***************************************************************************/
int
ms_md2doy(int year, int month, int mday, int *jday)
{
  int idx;
  int leap;
  int days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  
  /* Sanity check for the supplied parameters */
  if ( year < 1800 || year > 5000 )
    {
      ms_log (2, "ms_md2doy(): year (%d) is out of range\n", year);
      return -1;
    }
  if ( month < 1 || month > 12 )
    {
      ms_log (2, "ms_md2doy(): month (%d) is out of range\n", month);
      return -1;
    }
  if ( mday < 1 || mday > 31 )
    {
      ms_log (2, "ms_md2doy(): day-of-month (%d) is out of range\n", mday);
      return -1;
    }
  
  /* Test for leap year */
  leap = ( ((year%4 == 0) && (year%100 != 0)) || (year%400 == 0) ) ? 1 : 0;
  
  /* Add a day to February if leap year */
  if ( leap )
    days[1]++;
  
  /* Check that the day-of-month jives with specified month */
  if ( mday > days[month-1] )
    {
      ms_log (2, "ms_md2doy(): day-of-month (%d) is out of range for month %d\n",
	       mday, month);
      return -1;
    }

  *jday = 0;
  month--;
  
  for ( idx=0; idx < 12; idx++ )
    {
      if ( idx == month )
	{
	  *jday += mday;
	  break;
	}
      
      *jday += days[idx];
    }
  
  return 0;
}  /* End of ms_md2doy() */


/***************************************************************************
 * ms_btime2hptime:
 *
 * Convert a binary SEED time structure to a high precision epoch time
 * (1/HPTMODULUS second ticks from the epoch).  The algorithm used is
 * a specific version of a generalized function in GNU glibc.
 *
 * Returns a high precision epoch time on success and HPTERROR on
 * error.
 ***************************************************************************/
hptime_t
ms_btime2hptime (BTime *btime)
{
  hptime_t hptime;
  int shortyear;
  int a4, a100, a400;
  int intervening_leap_days;
  int days;
  
  if ( ! btime )
    return HPTERROR;
  
  shortyear = btime->year - 1900;

  a4 = (shortyear >> 2) + 475 - ! (shortyear & 3);
  a100 = a4 / 25 - (a4 % 25 < 0);
  a400 = a100 >> 2;
  intervening_leap_days = (a4 - 492) - (a100 - 19) + (a400 - 4);
  
  days = (365 * (shortyear - 70) + intervening_leap_days + (btime->day - 1));
  
  hptime = (hptime_t ) (60 * (60 * ((hptime_t) 24 * days + btime->hour) + btime->min) + btime->sec) * HPTMODULUS
    + (btime->fract * (HPTMODULUS / 10000));
    
  return hptime;
}  /* End of ms_btime2hptime() */


/***************************************************************************
 * ms_btime2isotimestr:
 *
 * Build a time string in ISO recommended format from a BTime struct.
 *
 * The provided isostimestr must have enough room for the resulting time
 * string of 25 characters, i.e. '2001-07-29T12:38:00.0000' + NULL.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
ms_btime2isotimestr (BTime *btime, char *isotimestr)
{  
  int month = 0;
  int mday = 0;
  int ret;

  if ( ! isotimestr )
    return NULL;

  if ( ms_doy2md (btime->year, btime->day, &month, &mday) )
    {
      ms_log (2, "ms_btime2isotimestr(): Error converting year %d day %d\n",
	      btime->year, btime->day);
      return NULL;
    }
  
  ret = snprintf (isotimestr, 25, "%4d-%02d-%02dT%02d:%02d:%02d.%04d",
		  btime->year, month, mday,
		  btime->hour, btime->min, btime->sec, btime->fract);
  
  if ( ret != 24 )
    return NULL;
  else
    return isotimestr;
}  /* End of ms_btime2isotimestr() */


/***************************************************************************
 * ms_btime2mdtimestr:
 *
 * Build a time string in month-day format from a BTime struct.
 * 
 * The provided isostimestr must have enough room for the resulting time
 * string of 25 characters, i.e. '2001-07-29 12:38:00.0000' + NULL.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
ms_btime2mdtimestr (BTime *btime, char *mdtimestr)
{ 
  int month = 0;
  int mday = 0;
  int ret;
  
  if ( ! mdtimestr )
    return NULL;
  
  if ( ms_doy2md (btime->year, btime->day, &month, &mday) )
    {
      ms_log (2, "ms_btime2mdtimestr(): Error converting year %d day %d\n",
              btime->year, btime->day);
      return NULL;
    }
  
  ret = snprintf (mdtimestr, 25, "%4d-%02d-%02d %02d:%02d:%02d.%04d",
                  btime->year, month, mday,
                  btime->hour, btime->min, btime->sec, btime->fract);

  if ( ret != 24 )
    return NULL;
  else
    return mdtimestr;
}  /* End of ms_btime2mdtimestr() */


/***************************************************************************
 * ms_btime2seedtimestr:
 *
 * Build a SEED time string from a BTime struct.
 *
 * The provided seedtimestr must have enough room for the resulting time
 * string of 23 characters, i.e. '2001,195,12:38:00.0000' + NULL.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
ms_btime2seedtimestr (BTime *btime, char *seedtimestr)
{
  int ret;
  
  if ( ! seedtimestr )
    return NULL;
  
  ret = snprintf (seedtimestr, 23, "%4d,%03d,%02d:%02d:%02d.%04d",
		  btime->year, btime->day,
		  btime->hour, btime->min, btime->sec, btime->fract);
  
  if ( ret != 22 )
    return NULL;
  else
    return seedtimestr;
}  /* End of ms_btime2seedtimestr() */


/***************************************************************************
 * ms_hptime2btime:
 *
 * Convert a high precision epoch time to a SEED binary time
 * structure.  The microseconds beyond the 1/10000 second range are
 * truncated and *not* rounded, this is intentional and necessary.
 *
 * Returns 0 on success and -1 on error.
 ***************************************************************************/
int
ms_hptime2btime (hptime_t hptime, BTime *btime)
{
  struct tm tms;
  int64_t isec;
  int ifract;
  int bfract;
  
  if ( btime == NULL )
    return -1;
  
  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec = MS_HPTIME2EPOCH(hptime);
  ifract = (int)(hptime - (isec * HPTMODULUS));
  
  /* BTime only has 1/10000 second precision */
  bfract = ifract / (HPTMODULUS / 10000);
  
  /* Adjust for negative epoch times, round back when needed */
  if ( hptime < 0 && ifract != 0 )
    {
      /* Isolate microseconds between 1e-4 and 1e-6 precision and adjust bfract if not zero */
      if ( ifract - bfract * (HPTMODULUS / 10000) )
	bfract -= 1;
      
      isec -= 1;
      bfract = 10000 - (-bfract);
    }

  if ( ! (ms_gmtime_r (&isec, &tms)) )
    return -1;
  
  btime->year   = tms.tm_year + 1900;
  btime->day    = tms.tm_yday + 1;
  btime->hour   = tms.tm_hour;
  btime->min    = tms.tm_min;
  btime->sec    = tms.tm_sec;
  btime->unused = 0;
  btime->fract  = (uint16_t) bfract;
  
  return 0;
}  /* End of ms_hptime2btime() */


/***************************************************************************
 * ms_hptime2isotimestr:
 *
 * Build a time string in ISO recommended format from a high precision
 * epoch time.
 *
 * The provided isostimestr must have enough room for the resulting time
 * string of 27 characters, i.e. '2001-07-29T12:38:00.000000' + NULL.
 *
 * The 'subseconds' flag controls whenther the sub second portion of the
 * time is included or not.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
ms_hptime2isotimestr (hptime_t hptime, char *isotimestr, flag subseconds)
{
  struct tm tms;
  int64_t isec;
  int ifract;
  int ret;

  if ( isotimestr == NULL )
    return NULL;

  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec = MS_HPTIME2EPOCH(hptime);
  ifract = (int)(hptime - (isec * HPTMODULUS));
  
  /* Adjust for negative epoch times */
  if ( hptime < 0 && ifract != 0 )
    {
      isec -= 1;
      ifract = HPTMODULUS - (-ifract);
    }
  
  if ( ! (ms_gmtime_r (&isec, &tms)) )
    return NULL;
  
  if ( subseconds )
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (isotimestr, 27, "%4d-%02d-%02dT%02d:%02d:%02d.%06d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    ret = snprintf (isotimestr, 20, "%4d-%02d-%02dT%02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);

  if ( ret != 26 && ret != 19 )
    return NULL;
  else
    return isotimestr;
}  /* End of ms_hptime2isotimestr() */


/***************************************************************************
 * ms_hptime2mdtimestr:
 *
 * Build a time string in month-day format from a high precision
 * epoch time.
 *
 * The provided mdtimestr must have enough room for the resulting time
 * string of 27 characters, i.e. '2001-07-29 12:38:00.000000' + NULL.
 *
 * The 'subseconds' flag controls whenther the sub second portion of the
 * time is included or not.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
ms_hptime2mdtimestr (hptime_t hptime, char *mdtimestr, flag subseconds)
{
  struct tm tms;
  int64_t isec;
  int ifract;
  int ret;
  
  if ( mdtimestr == NULL )
    return NULL;

  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec = MS_HPTIME2EPOCH(hptime);
  ifract = (int)(hptime - (isec * HPTMODULUS));

  /* Adjust for negative epoch times */
  if ( hptime < 0 && ifract != 0 )
    {
      isec -= 1;
      ifract = HPTMODULUS - (-ifract);
    }
  
  if ( ! (ms_gmtime_r (&isec, &tms)) )
    return NULL;

  if ( subseconds )
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (mdtimestr, 27, "%4d-%02d-%02d %02d:%02d:%02d.%06d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    ret = snprintf (mdtimestr, 20, "%4d-%02d-%02d %02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);

  if ( ret != 26 && ret != 19 )
    return NULL;
  else
    return mdtimestr;
}  /* End of ms_hptime2mdtimestr() */


/***************************************************************************
 * ms_hptime2seedtimestr:
 *
 * Build a SEED time string from a high precision epoch time.
 *
 * The provided seedtimestr must have enough room for the resulting time
 * string of 25 characters, i.e. '2001,195,12:38:00.000000\n'.
 *
 * The 'subseconds' flag controls whenther the sub second portion of the
 * time is included or not.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
ms_hptime2seedtimestr (hptime_t hptime, char *seedtimestr, flag subseconds)
{
  struct tm tms;
  int64_t isec;
  int ifract;
  int ret;
  
  if ( seedtimestr == NULL )
    return NULL;
  
  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec = MS_HPTIME2EPOCH(hptime);
  ifract = (int)(hptime - (isec * HPTMODULUS));
  
  /* Adjust for negative epoch times */
  if ( hptime < 0 && ifract != 0 )
    {
      isec -= 1;
      ifract = HPTMODULUS - (-ifract);
    }
  
  if ( ! (ms_gmtime_r (&isec, &tms)) )
    return NULL;
  
  if ( subseconds )
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (seedtimestr, 25, "%4d,%03d,%02d:%02d:%02d.%06d",
		    tms.tm_year + 1900, tms.tm_yday + 1,
		    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (seedtimestr, 18, "%4d,%03d,%02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_yday + 1,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);
  
  if ( ret != 24 && ret != 17 )
    return NULL;
  else
    return seedtimestr;
}  /* End of ms_hptime2seedtimestr() */


/***************************************************************************
 * ms_time2hptime_int:
 *
 * Convert specified time values to a high precision epoch time.  This
 * is an internal version which does no range checking, it is assumed
 * that checking the range for each value has already been done.
 *
 * Returns epoch time on success and HPTERROR on error.
 ***************************************************************************/
static hptime_t
ms_time2hptime_int (int year, int day, int hour, int min, int sec, int usec)
{
  BTime btime;
  hptime_t hptime;
  
  memset (&btime, 0, sizeof(BTime));
  btime.day = 1;
  
  /* Convert integer seconds using ms_btime2hptime */
  btime.year = (int16_t) year;
  btime.day = (int16_t) day;
  btime.hour = (uint8_t) hour;
  btime.min = (uint8_t) min;
  btime.sec = (uint8_t) sec;
  btime.fract = 0;

  hptime = ms_btime2hptime (&btime);
  
  if ( hptime == HPTERROR )
    {
      ms_log (2, "ms_time2hptime(): Error converting with ms_btime2hptime()\n");
      return HPTERROR;
    }
  
  /* Add the microseconds */
  hptime += (hptime_t) usec * (1000000 / HPTMODULUS);
  
  return hptime;
}  /* End of ms_time2hptime_int() */


/***************************************************************************
 * ms_time2hptime:
 *
 * Convert specified time values to a high precision epoch time.  This
 * is essentially a frontend for ms_time2hptime that does range
 * checking for each input value.
 *
 * Expected ranges:
 * year : 1800 - 5000
 * day  : 1 - 366
 * hour : 0 - 23
 * min  : 0 - 59
 * sec  : 0 - 60
 * usec : 0 - 999999
 *
 * Returns epoch time on success and HPTERROR on error.
 ***************************************************************************/
hptime_t
ms_time2hptime (int year, int day, int hour, int min, int sec, int usec)
{
  if ( year < 1800 || year > 5000 )
    {
      ms_log (2, "ms_time2hptime(): Error with year value: %d\n", year);
      return HPTERROR;
    }
  
  if ( day < 1 || day > 366 )
    {
      ms_log (2, "ms_time2hptime(): Error with day value: %d\n", day);
      return HPTERROR;
    }
  
  if ( hour < 0 || hour > 23 )
    {
      ms_log (2, "ms_time2hptime(): Error with hour value: %d\n", hour);
      return HPTERROR;
    }
  
  if ( min < 0 || min > 59 )
    {
      ms_log (2, "ms_time2hptime(): Error with minute value: %d\n", min);
      return HPTERROR;
    }
  
  if ( sec < 0 || sec > 60 )
    {
      ms_log (2, "ms_time2hptime(): Error with second value: %d\n", sec);
      return HPTERROR;
    }
  
  if ( usec < 0 || usec > 999999 )
    {
      ms_log (2, "ms_time2hptime(): Error with microsecond value: %d\n", usec);
      return HPTERROR;
    }
  
  return ms_time2hptime_int (year, day, hour, min, sec, usec);
}  /* End of ms_time2hptime() */


/***************************************************************************
 * ms_seedtimestr2hptime:
 * 
 * Convert a SEED time string to a high precision epoch time.  SEED
 * time format is "YYYY[,DDD,HH,MM,SS.FFFFFF]", the delimiter can be a
 * comma [,], colon [:] or period [.] except for the fractional
 * seconds which must start with a period [.].
 *
 * The time string can be "short" in which case the omitted values are
 * assumed to be zero (with the exception of DDD which is assumed to
 * be 1): "YYYY,DDD,HH" assumes MM, SS and FFFF are 0.  The year is
 * required, otherwise there wouldn't be much for a date.
 *
 * Ranges are checked for each value.
 *
 * Returns epoch time on success and HPTERROR on error.
 ***************************************************************************/
hptime_t
ms_seedtimestr2hptime (char *seedtimestr)
{
  int fields;
  int year = 0;
  int day  = 1;
  int hour = 0;
  int min  = 0;
  int sec  = 0;
  float fusec = 0.0;
  int usec = 0;
  
  fields = sscanf (seedtimestr, "%d%*[,:.]%d%*[,:.]%d%*[,:.]%d%*[,:.]%d%f",
		   &year, &day, &hour, &min, &sec, &fusec);
  
  /* Convert fractional seconds to microseconds */
  if ( fusec != 0.0 )
    {
      usec = (int) (fusec * 1000000.0 + 0.5);
    }
  
  if ( fields < 1 )
    {
      ms_log (2, "ms_seedtimestr2hptime(): Error converting time string: %s\n", seedtimestr);
      return HPTERROR;
    }
  
  if ( year < 1800 || year > 5000 )
    {
      ms_log (2, "ms_seedtimestr2hptime(): Error with year value: %d\n", year);
      return HPTERROR;
    }

  if ( day < 1 || day > 366 )
    {
      ms_log (2, "ms_seedtimestr2hptime(): Error with day value: %d\n", day);
      return HPTERROR;
    }
  
  if ( hour < 0 || hour > 23 )
    {
      ms_log (2, "ms_seedtimestr2hptime(): Error with hour value: %d\n", hour);
      return HPTERROR;
    }
  
  if ( min < 0 || min > 59 )
    {
      ms_log (2, "ms_seedtimestr2hptime(): Error with minute value: %d\n", min);
      return HPTERROR;
    }
  
  if ( sec < 0 || sec > 60 )
    {
      ms_log (2, "ms_seedtimestr2hptime(): Error with second value: %d\n", sec);
      return HPTERROR;
    }
  
  if ( usec < 0 || usec > 999999 )
    {
      ms_log (2, "ms_seedtimestr2hptime(): Error with fractional second value: %d\n", usec);
      return HPTERROR;
    }
  
  return ms_time2hptime_int (year, day, hour, min, sec, usec);
}  /* End of ms_seedtimestr2hptime() */


/***************************************************************************
 * ms_timestr2hptime:
 * 
 * Convert a generic time string to a high precision epoch time.
 * SEED time format is "YYYY[/MM/DD HH:MM:SS.FFFF]", the delimiter can
 * be a dash [-], slash [/], colon [:], or period [.] and between the
 * date and time a 'T' or a space may be used.  The fracttional
 * seconds must begin with a period [.].
 *
 * The time string can be "short" in which case the omitted values are
 * assumed to be zero (with the exception of month and day which are
 * assumed to be 1): "YYYY/MM/DD" assumes HH, MM, SS and FFFF are 0.
 * The year is required, otherwise there wouldn't be much for a date.
 *
 * Ranges are checked for each value.
 *
 * Returns epoch time on success and HPTERROR on error.
 ***************************************************************************/
hptime_t
ms_timestr2hptime (char *timestr)
{
  int fields;
  int year = 0;
  int mon  = 1;
  int mday = 1;
  int day  = 1;
  int hour = 0;
  int min  = 0;
  int sec  = 0;
  float fusec = 0.0;
  int usec = 0;
  
  fields = sscanf (timestr, "%d%*[-/:.]%d%*[-/:.]%d%*[-/:.Tt ]%d%*[-/:.]%d%*[- /:.]%d%f",
		   &year, &mon, &mday, &hour, &min, &sec, &fusec);
  
  /* Convert fractional seconds to microseconds */
  if ( fusec != 0.0 )
    {
      usec = (int) (fusec * 1000000.0 + 0.5);
    }

  if ( fields < 1 )
    {
      ms_log (2, "ms_timestr2hptime(): Error converting time string: %s\n", timestr);
      return HPTERROR;
    }
  
  if ( year < 1800 || year > 5000 )
    {
      ms_log (2, "ms_timestr2hptime(): Error with year value: %d\n", year);
      return HPTERROR;
    }
  
  if ( mon < 1 || mon > 12 )
    {
      ms_log (2, "ms_timestr2hptime(): Error with month value: %d\n", mon);
      return HPTERROR;
    }

  if ( mday < 1 || mday > 31 )
    {
      ms_log (2, "ms_timestr2hptime(): Error with day value: %d\n", mday);
      return HPTERROR;
    }

  /* Convert month and day-of-month to day-of-year */
  if ( ms_md2doy (year, mon, mday, &day) )
    {
      return HPTERROR;
    }
  
  if ( hour < 0 || hour > 23 )
    {
      ms_log (2, "ms_timestr2hptime(): Error with hour value: %d\n", hour);
      return HPTERROR;
    }
  
  if ( min < 0 || min > 59 )
    {
      ms_log (2, "ms_timestr2hptime(): Error with minute value: %d\n", min);
      return HPTERROR;
    }
  
  if ( sec < 0 || sec > 60 )
    {
      ms_log (2, "ms_timestr2hptime(): Error with second value: %d\n", sec);
      return HPTERROR;
    }
  
  if ( usec < 0 || usec > 999999 )
    {
      ms_log (2, "ms_timestr2hptime(): Error with fractional second value: %d\n", usec);
      return HPTERROR;
    }
  
  return ms_time2hptime_int (year, day, hour, min, sec, usec);
}  /* End of ms_timestr2hptime() */


/***************************************************************************
 * ms_nomsamprate:
 *
 * Calculate a sample rate from SEED sample rate factor and multiplier
 * as stored in the fixed section header of data records.
 * 
 * Returns the positive sample rate.
 ***************************************************************************/
double
ms_nomsamprate (int factor, int multiplier)
{
  double samprate = 0.0;
  
  if ( factor > 0 )
    samprate = (double) factor;
  else if ( factor < 0 )
    samprate = -1.0 / (double) factor;
  if ( multiplier > 0 )
    samprate = samprate * (double) multiplier;
  else if ( multiplier < 0 )
    samprate = -1.0 * (samprate / (double) multiplier);
  
  return samprate;
}  /* End of ms_nomsamprate() */


/***************************************************************************
 * ms_genfactmult:
 *
 * Generate an approriate SEED sample rate factor and multiplier from
 * a double precision sample rate.
 * 
 * Returns 0 on success and -1 on error.
 ***************************************************************************/
int
ms_genfactmult (double samprate, int16_t *factor, int16_t *multiplier)
{
  int num, den;
  
  /* This routine does not support very high or negative sample rates,
     even though high rates are possible in Mini-SEED */
  if ( samprate > 32767.0 || samprate < 0.0 )
    {
      ms_log (2, "ms_genfactmult(): samprate out of range: %g\n", samprate);
      return -1;
    }
  
  /* If the sample rate is integer set the factor and multipler in the
     obvious way, otherwise derive a (potentially approximate)
     numerator and denominator for the given samprate */
  if ( (samprate - (int16_t) samprate) < 0.000001 )
    {
      *factor = (int16_t) samprate;
      if ( *factor )
	*multiplier = 1;
    }
  else
    {
      ms_ratapprox (samprate, &num, &den, 32767, 1e-12);
      
      /* Negate the multiplier to denote a division factor */
      *factor = (int16_t ) num;
      *multiplier = (int16_t) -den;
    }
  
  return 0;
}  /* End of ms_genfactmult() */


/***************************************************************************
 * ms_ratapprox:
 *
 * Find an approximate rational number for a real through continued
 * fraction expansion.  Given a double precsion 'real' find a
 * numerator (num) and denominator (den) whose absolute values are not
 * larger than 'maxval' while trying to reach a specified 'precision'.
 * 
 * Returns the number of iterations performed.
 ***************************************************************************/
int
ms_ratapprox (double real, int *num, int *den, int maxval, double precision)
{
  double realj, preal;
  char pos;  
  int pnum, pden;
  int iterations = 1;
  int Aj1, Aj2, Bj1, Bj2;
  int bj = 0;
  int Aj = 0;
  int Bj = 1;
  
  if ( real >= 0.0 ) { pos = 1; realj = real; }
  else               { pos = 0; realj = -real; }
  
  preal = realj;
  
  bj = (int) (realj + precision);
  realj = 1 / (realj - bj);
  Aj = bj; Aj1 = 1;
  Bj = 1;  Bj1 = 0;
  *num = pnum = Aj;
  *den = pden = Bj;
  if ( !pos ) *num = -*num;
  
  while ( ms_dabs(preal - (double)Aj/(double)Bj) > precision &&
	  Aj < maxval && Bj < maxval )
    {
      Aj2 = Aj1; Aj1 = Aj;
      Bj2 = Bj1; Bj1 = Bj;
      bj = (int) (realj + precision);
      realj = 1 / (realj - bj);
      Aj = bj * Aj1 + Aj2;
      Bj = bj * Bj1 + Bj2;
      *num = pnum;
      *den = pden;
      if ( !pos ) *num = -*num;
      pnum = Aj;
      pden = Bj;
      
      iterations++;
    }
  
  if ( pnum < maxval && pden < maxval )
    {
      *num = pnum;
      *den = pden;
      if ( !pos ) *num = -*num;
    }
  
  return iterations;
}


/***************************************************************************
 * ms_bigendianhost:
 *
 * Determine the byte order of the host machine.  Due to the lack of
 * portable defines to determine host byte order this run-time test is
 * provided.  The code below actually tests for little-endianess, the
 * only other alternative is assumed to be big endian.
 * 
 * Returns 0 if the host is little endian, otherwise 1.
 ***************************************************************************/
int
ms_bigendianhost ()
{
  int16_t host = 1;
  return !(*((int8_t *)(&host)));
}  /* End of ms_bigendianhost() */


/***************************************************************************
 * ms_dabs:
 *
 * Determine the absolute value of an input double, actually just test
 * if the input double is positive multiplying by -1.0 if not and
 * return it.
 * 
 * Returns the positive value of input double.
 ***************************************************************************/
double
ms_dabs (double val)
{
  if ( val < 0.0 )
    val *= -1.0;
  return val;
}  /* End of ms_dabs() */


/***************************************************************************
 * ms_gmtime_r:
 *
 * An internal version of gmtime_r() that is 64-bit compliant and
 * works with years beyond 2038.
 *
 * The original was called pivotal_gmtime_r() by Paul Sheer, all
 * required copyright and other hoohas are below.  Modifications were
 * made to integrate the original to this code base, avoid name
 * collisions and formatting so I could read it.
 * 
 * Returns a pointer to the populated tm struct on success and NULL on error.
 ***************************************************************************/

/* pivotal_gmtime_r - a replacement for gmtime/localtime/mktime
                      that works around the 2038 bug on 32-bit
                      systems. (Version 4)

   Copyright (C) 2009  Paul Sheer

   Redistribution and use in source form, with or without modification,
   is permitted provided that the above copyright notice, this list of
   conditions, the following disclaimer, and the following char array
   are retained.

   Redistribution and use in binary form must reproduce an
   acknowledgment: 'With software provided by http://2038bug.com/' in
   the documentation and/or other materials provided with the
   distribution, and wherever such acknowledgments are usually
   accessible in Your program.

   This software is provided "AS IS" and WITHOUT WARRANTY, either
   express or implied, including, without limitation, the warranties of
   NON-INFRINGEMENT, MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE. THE ENTIRE RISK AS TO THE QUALITY OF THIS SOFTWARE IS WITH
   YOU. Under no circumstances and under no legal theory, whether in
   tort (including negligence), contract, or otherwise, shall the
   copyright owners be liable for any direct, indirect, special,
   incidental, or consequential damages of any character arising as a
   result of the use of this software including, without limitation,
   damages for loss of goodwill, work stoppage, computer failure or
   malfunction, or any and all other commercial damages or losses. This
   limitation of liability shall not apply to liability for death or
   personal injury resulting from copyright owners' negligence to the
   extent applicable law prohibits such limitation. Some jurisdictions
   do not allow the exclusion or limitation of incidental or
   consequential damages, so this exclusion and limitation may not apply
   to You.

*/

const char pivotal_gmtime_r_stamp_lm[] =
  "pivotal_gmtime_r. Copyright (C) 2009  Paul Sheer. Terms and "
  "conditions apply. Visit http://2038bug.com/ for more info.";

static const int tm_days[4][13] = {
  {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
  {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
  {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
  {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366},
};

#define TM_LEAP_CHECK(n) ((!(((n) + 1900) % 400) || (!(((n) + 1900) % 4) && (((n) + 1900) % 100))) != 0)
#define TM_WRAP(a,b,m)   ((a) = ((a) <  0  ) ? ((b)--, (a) + (m)) : (a))

static struct tm *
ms_gmtime_r (int64_t *timep, struct tm *result)
{
  int v_tm_sec, v_tm_min, v_tm_hour, v_tm_mon, v_tm_wday, v_tm_tday;
  int leap;
  long m;
  int64_t tv;
  
  if ( ! timep || ! result )
    return NULL;
  
  tv = *timep;
  
  v_tm_sec = ((int64_t) tv % (int64_t) 60);
  tv /= 60;
  v_tm_min = ((int64_t) tv % (int64_t) 60);
  tv /= 60;
  v_tm_hour = ((int64_t) tv % (int64_t) 24);
  tv /= 24;
  v_tm_tday = (int)tv;
  
  TM_WRAP (v_tm_sec, v_tm_min, 60);
  TM_WRAP (v_tm_min, v_tm_hour, 60);
  TM_WRAP (v_tm_hour, v_tm_tday, 24);
  
  if ( (v_tm_wday = (v_tm_tday + 4) % 7) < 0 )
    v_tm_wday += 7;
  
  m = (long) v_tm_tday;
  
  if ( m >= 0 )
    {
      result->tm_year = 70;
      leap = TM_LEAP_CHECK (result->tm_year);
      
      while ( m >= (long) tm_days[leap + 2][12] )
	{
	  m -= (long) tm_days[leap + 2][12];
	  result->tm_year++;
	  leap = TM_LEAP_CHECK (result->tm_year);
	}
      
      v_tm_mon = 0;
      
      while ( m >= (long) tm_days[leap][v_tm_mon] )
	{
	  m -= (long) tm_days[leap][v_tm_mon];
	  v_tm_mon++;
	}
    }
  else
    {
      result->tm_year = 69;
      leap = TM_LEAP_CHECK (result->tm_year);
      
      while ( m < (long) -tm_days[leap + 2][12] )
	{
	  m += (long) tm_days[leap + 2][12];
	  result->tm_year--;
	  leap = TM_LEAP_CHECK (result->tm_year);
	}
      
      v_tm_mon = 11;
      
      while ( m < (long) -tm_days[leap][v_tm_mon] )
	{
	  m += (long) tm_days[leap][v_tm_mon];
	  v_tm_mon--;
	}
      
      m += (long) tm_days[leap][v_tm_mon];
    }
  
  result->tm_mday = (int) m + 1;
  result->tm_yday = tm_days[leap + 2][v_tm_mon] + m;
  result->tm_sec = v_tm_sec;
  result->tm_min = v_tm_min;
  result->tm_hour = v_tm_hour;
  result->tm_mon = v_tm_mon;
  result->tm_wday = v_tm_wday;
  
  return result;
}  /* End of ms_gmtime_r() */
/***************************************************************************
 * gswap.c:
 *
 * Functions for generalized, in-pace byte swapping between LSBF and
 * MSBF byte orders.
 *
 * Some standard integer types are needed, namely uint8_t and
 * uint32_t, (these are normally declared by including inttypes.h or
 * stdint.h).  Each function expects it's input to be a void pointer
 * to a quantity of the appropriate size.
 *
 * There are two versions of most routines, one that works on
 * quantities regardless of alignment (gswapX) and one that works on
 * memory aligned quantities (gswapXa).  The memory aligned versions
 * (gswapXa) are much faster than the other versions (gswapX), but the
 * memory *must* be aligned.
 *
 * Written by Chad Trabant,
 *   IRIS Data Management Center
 *
 * Version: 2010.006
 ***************************************************************************/

#include "lmplatform.h"

/* Swap routines that work on any (aligned or not) quantities */

void
ms_gswap2 ( void *data2 )
{
  uint8_t temp;
  
  union
  {
    uint8_t  c[2];
  } dat;
  
  memcpy( &dat, data2, 2 );
  temp     = dat.c[0];
  dat.c[0] = dat.c[1];
  dat.c[1] = temp;
  memcpy( data2, &dat, 2 );
}


void
ms_gswap3 ( void *data3 )
{
  uint8_t temp;
  
  union
  {
    uint8_t  c[3];
  } dat;
  
  memcpy( &dat, data3, 3 );
  temp     = dat.c[0];
  dat.c[0] = dat.c[2];
  dat.c[2] = temp;
  memcpy( data3, &dat, 3 );
}


void
ms_gswap4 ( void *data4 )
{
  uint8_t temp;

  union {
    uint8_t c[4];
  } dat;
  
  memcpy( &dat, data4, 4 );
  temp     = dat.c[0];
  dat.c[0] = dat.c[3];
  dat.c[3] = temp;
  temp     = dat.c[1];
  dat.c[1] = dat.c[2];
  dat.c[2] = temp;
  memcpy( data4, &dat, 4 );
}


void
ms_gswap8 ( void *data8 )
{
  uint8_t temp;
  
  union
  {
    uint8_t   c[8];
  } dat;
  
  memcpy( &dat, data8, 8 );
  temp     = dat.c[0];
  dat.c[0] = dat.c[7];
  dat.c[7] = temp;
  
  temp     = dat.c[1];
  dat.c[1] = dat.c[6];
  dat.c[6] = temp;
  
  temp     = dat.c[2];
  dat.c[2] = dat.c[5];
  dat.c[5] = temp;
  
  temp     = dat.c[3];
  dat.c[3] = dat.c[4];
  dat.c[4] = temp;
  memcpy( data8, &dat, 8 );
}

/* Swap routines that work on memory aligned quantities */

void
ms_gswap2a ( void *data2 )
{
  uint16_t *data = data2;
  
  *data=(((*data>>8)&0xff) | ((*data&0xff)<<8));
}


void
ms_gswap4a ( void *data4 )
{
  uint32_t *data = data4;
  
  *data=(((*data>>24)&0xff) | ((*data&0xff)<<24) |
	 ((*data>>8)&0xff00) | ((*data&0xff00)<<8));
}


void
ms_gswap8a ( void *data8 )
{
  uint32_t *data4 = data8;
  uint32_t h0, h1;
  
  h0 = data4[0];
  h0 = (((h0>>24)&0xff) | ((h0&0xff)<<24) |
	((h0>>8)&0xff00) | ((h0&0xff00)<<8));
  
  h1 = data4[1];
  h1 = (((h1>>24)&0xff) | ((h1&0xff)<<24) |
	((h1>>8)&0xff00) | ((h1&0xff00)<<8));
  
  data4[0] = h1;
  data4[1] = h0;
}
/***************************************************************************
 * lmplatform.c:
 * 
 * Platform portability routines.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License (GNU-LGPL) for more details.  The
 * GNU-LGPL and further information can be found here:
 * http://www.gnu.org/
 *
 * Written by Chad Trabant, IRIS Data Management Center
 *
 * modified: 2010.304
 ***************************************************************************/

/* Define _LARGEFILE_SOURCE to get ftello/fseeko on some systems (Linux) */
#define _LARGEFILE_SOURCE 1

#include "lmplatform.h"


/***************************************************************************
 * lmp_ftello:
 *
 * Return the current file position for the specified descriptor using
 * the system's closest match to the POSIX ftello.
 ***************************************************************************/
off_t
lmp_ftello (FILE *stream)
{
#if defined(LMP_WIN32)
  return (off_t) ftell (stream);

#else
  return (off_t) ftello (stream);

#endif
}  /* End of lmp_ftello() */


/***************************************************************************
 * lmp_fseeko:
 *
 * Seek to a specific file position for the specified descriptor using
 * the system's closest match to the POSIX fseeko.
 ***************************************************************************/
int
lmp_fseeko (FILE *stream, off_t offset, int whence)
{
#if defined(LMP_WIN32)
  return (int) fseek (stream, (long int) offset, whence);
  
#else
  return (int) fseeko (stream, offset, whence);
  
#endif
}  /* End of lmp_fseeko() */

/***************************************************************************
 * logging.c
 *
 * Log handling routines for libmseed
 *
 * Chad Trabant
 * IRIS Data Management Center
 *
 * modified: 2010.253
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "libmseed.h"

void ms_loginit_main (MSLogParam *logp,
		      void (*log_print)(char*), const char *logprefix,
		      void (*diag_print)(char*), const char *errprefix);

int ms_log_main (MSLogParam *logp, int level, va_list *varlist);

/* Initialize the global logging parameters */
MSLogParam gMSLogParam = {NULL, NULL, NULL, NULL};


/***************************************************************************
 * ms_loginit:
 *
 * Initialize the global logging parameters.
 *
 * See ms_loginit_main() description for usage.
 ***************************************************************************/
void
ms_loginit (void (*log_print)(char*), const char *logprefix,
	    void (*diag_print)(char*), const char *errprefix)
{
  ms_loginit_main(&gMSLogParam, log_print, logprefix, diag_print, errprefix);
}  /* End of ms_loginit() */


/***************************************************************************
 * ms_loginit_l:
 *
 * Initialize MSLogParam specific logging parameters.  If the logging parameters
 * have not been initialized (log == NULL) new parameter space will
 * be allocated.
 *
 * See ms_loginit_main() description for usage.
 *
 * Returns a pointer to the created/re-initialized MSLogParam struct
 * on success and NULL on error.
 ***************************************************************************/
MSLogParam *
ms_loginit_l (MSLogParam *logp,
	      void (*log_print)(char*), const char *logprefix,
	      void (*diag_print)(char*), const char *errprefix)
{
  MSLogParam *llog;
  
  if ( logp == NULL )
    {
      llog = (MSLogParam *) malloc (sizeof(MSLogParam));
      
      if ( llog == NULL )
        {
          ms_log (2, "ms_loginit_l(): Cannot allocate memory\n");
          return NULL;
        }
      
      llog->log_print = NULL;
      llog->logprefix = NULL;
      llog->diag_print = NULL;
      llog->errprefix = NULL;
    }
  else
    {
      llog = logp;
    }
  
  ms_loginit_main (llog, log_print, logprefix, diag_print, errprefix);

  return llog;
}  /* End of ms_loginit_l() */


/***************************************************************************
 * ms_loginit_main:
 *
 * Initialize the logging subsystem.  Given values determine how ms_log()
 * and ms_log_l() emit messages.
 *
 * This function modifies the logging parameters in the passed MSLogParam.
 *
 * Any log/error printing functions indicated must except a single
 * argument, namely a string (const char *).  The ms_log() and
 * ms_log_r() functions format each message and then pass the result
 * on to the log/error printing functions.
 *
 * If the log/error prefixes have been set they will be pre-pended to the
 * message.
 *
 * Use NULL for the function pointers or the prefixes if they should not
 * be changed from previously set or default values.  The default behavior
 * of the logging subsystem is given in the example below.
 *
 * Example: ms_loginit_main (0, (void*)&printf, NULL, (void*)&printf, "error: ");
 ***************************************************************************/
void
ms_loginit_main (MSLogParam *logp,
		 void (*log_print)(char*), const char *logprefix,
		 void (*diag_print)(char*), const char *errprefix)
{
  if ( ! logp )
    return;

  if ( log_print )
    logp->log_print = log_print;
  
  if ( logprefix )
    {
      if ( strlen(logprefix) >= MAX_LOG_MSG_LENGTH )
	{
	  ms_log_l (logp, 2, 0, "log message prefix is too large\n");
	}
      else
	{
	  logp->logprefix = logprefix;
	}
    }
  
  if ( diag_print )
    logp->diag_print = diag_print;
  
  if ( errprefix )
    {
      if ( strlen(errprefix) >= MAX_LOG_MSG_LENGTH )
	{
	  ms_log_l (logp, 2, 0, "error message prefix is too large\n");
	}
      else
	{
	  logp->errprefix = errprefix;
	}
    }
  
  return;
}  /* End of ms_loginit_main() */


/***************************************************************************
 * ms_log:
 *
 * A wrapper to ms_log_main() that uses the global logging parameters.
 *
 * See ms_log_main() description for return values.
 ***************************************************************************/
int
ms_log (int level, ...)
{
  int retval;
  va_list varlist;
  
  va_start (varlist, level);

  retval = ms_log_main (&gMSLogParam, level, &varlist);

  va_end (varlist);

  return retval;
}  /* End of ms_log() */


/***************************************************************************
 * ms_log_l:
 *
 * A wrapper to ms_log_main() that uses the logging parameters in a
 * supplied MSLogParam.  If the supplied pointer is NULL the global logging
 * parameters will be used.
 *
 * See ms_log_main() description for return values.
 ***************************************************************************/
int
ms_log_l (MSLogParam *logp, int level, ...)
{
  int retval;
  va_list varlist;
  MSLogParam *llog;

  if ( ! logp )
    llog = &gMSLogParam;
  else
    llog = logp;
  
  va_start (varlist, level);
  
  retval = ms_log_main (llog, level, &varlist);

  va_end (varlist);

  return retval;
}  /* End of ms_log_l() */


/***************************************************************************
 * ms_log_main:
 *
 * A standard logging/printing routine.
 *
 * The function uses logging parameters specified in the supplied
 * MSLogParam.
 * 
 * This function expects 2+ arguments: message level, fprintf format,
 * and fprintf arguments. 
 *
 * Three levels are recognized:
 * 0  : Normal log messages, printed using log_print with logprefix
 * 1  : Diagnostic messages, printed using diag_print with logprefix
 * 2+ : Error messagess, printed using diag_print with errprefix
 *
 * This function builds the log/error message and passes to it as a
 * string (const char *) to the functions defined with ms_loginit() or
 * ms_loginit_l().  If the log/error printing functions have not been
 * defined messages will be printed with fprintf, log messages to
 * stdout and error messages to stderr.
 *
 * If the log/error prefix's have been set with ms_loginit() or
 * ms_loginit_l() they will be pre-pended to the message.
 *
 * All messages will be truncated to the MAX_LOG_MSG_LENGTH, this includes
 * any set prefix.
 *
 * Returns the number of characters formatted on success, and a
 * a negative value on error.
 ***************************************************************************/
int
ms_log_main (MSLogParam *logp, int level, va_list *varlist)
{
  static char message[MAX_LOG_MSG_LENGTH];
  int retvalue = 0;
  int presize;
  const char *format;
  
  if ( ! logp )
    {
      fprintf(stderr, "ms_log_main() called without specifying log parameters");
      return -1;
    }
  
  message[0] = '\0';

  format = va_arg (*varlist, const char *);

  if ( level >= 2 )  /* Error message */
    {
      if ( logp->errprefix != NULL )
        {
          strncpy (message, logp->errprefix, MAX_LOG_MSG_LENGTH);
        }
      else
        {
          strncpy (message, "Error: ", MAX_LOG_MSG_LENGTH);
        }
      
      presize = strlen(message);
      retvalue = vsnprintf (&message[presize],
   			    MAX_LOG_MSG_LENGTH - presize,
			    format, *varlist);
      
      message[MAX_LOG_MSG_LENGTH - 1] = '\0';

      if ( logp->diag_print != NULL )
        {
          logp->diag_print ((const char *) message);
        }
      else
        {
          fprintf(stderr, "%s", message);
        }
    }
  else if ( level == 1 )  /* Diagnostic message */
    {
      if ( logp->logprefix != NULL )
        {
          strncpy (message, logp->logprefix, MAX_LOG_MSG_LENGTH);
        }
      
      presize = strlen(message);
      retvalue = vsnprintf (&message[presize],
		            MAX_LOG_MSG_LENGTH - presize,
			    format, *varlist);
      
      message[MAX_LOG_MSG_LENGTH - 1] = '\0';
      
      if ( logp->diag_print != NULL )
        {
          logp->diag_print ((const char *) message);
        }
      else
        {
          fprintf(stderr, "%s", message);
        }
    }
  else if ( level == 0 )  /* Normal log message */
    {
      if ( logp->logprefix != NULL )
        {
          strncpy (message, logp->logprefix, MAX_LOG_MSG_LENGTH);
        }
      
      presize = strlen(message);
      retvalue = vsnprintf (&message[presize],
			    MAX_LOG_MSG_LENGTH - presize,
			    format, *varlist);
      
      message[MAX_LOG_MSG_LENGTH - 1] = '\0';
      
      if ( logp->log_print != NULL )
	{
           logp->log_print ((const char *) message);
	}
      else
	{
	  fprintf(stdout, "%s", message);
	}
    }
  
  return retvalue;
}  /* End of ms_log_main() */
/***************************************************************************
 * lookup.c:
 *
 * Generic lookup routines for Mini-SEED information.
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *
 * modified: 2006.346
 ***************************************************************************/

#include <string.h>

#include "libmseed.h"

/***************************************************************************
 * ms_samplesize():
 * 
 * Returns the sample size based on type code or 0 for unknown.
 ***************************************************************************/
uint8_t
ms_samplesize (const char sampletype)
{
  switch (sampletype)
    {
    case 'a':
      return 1;
    case 'i':
    case 'f':
      return 4;
    case 'd':
      return 8;
    default:
      return 0;
    }  /* end switch */
  
}  /* End of ms_samplesize() */


/***************************************************************************
 * ms_encodingstr():
 * 
 * Returns a string describing a data encoding format.
 ***************************************************************************/
char *
ms_encodingstr (const char encoding)
{
  switch (encoding)
    {
    case 0:
      return "ASCII text";
    case 1:
      return "16 bit integers";
    case 2:
      return "24 bit integers";
    case 3:
      return "32 bit integers";
    case 4:
      return "IEEE floating point";
    case 5:
      return "IEEE double precision float";
    case 10:
      return "STEIM 1 Compression";
    case 11:
      return "STEIM 2 Compression";
    case 12:
      return "GEOSCOPE Muxed 24 bit int";
    case 13:
      return "GEOSCOPE Muxed 16/3 bit gain/exp";
    case 14:
      return "GEOSCOPE Muxed 16/4 bit gain/exp";
    case 15:
      return "US National Network compression";
    case 16:
      return "CDSN 16 bit gain ranged";
    case 17:
      return "Graefenberg 16 bit gain ranged";
    case 18:
      return "IPG - Strasbourg 16 bit gain";
    case 19:
      return "STEIM 3 Compression";
    case 30:
      return "SRO Gain Ranged Format";
    case 31:
      return "HGLP Format";
    case 32:
      return "DWWSSN Format";
    case 33:
      return "RSTN 16 bit gain ranged";
    default:
      return "Unknown format code";
    }  /* end switch */

}  /* End of ms_encodingstr() */


/***************************************************************************
 * ms_blktdesc():
 *
 * Return a string describing a given blockette type or NULL if the
 * type is unknown.
 ***************************************************************************/
char *
ms_blktdesc (uint16_t blkttype)
{
  switch (blkttype)
    {
    case 100:
      return "Sample Rate";
    case 200:
      return "Generic Event Detection";
    case 201:
      return "Murdock Event Detection";
    case 300:
      return "Step Calibration";
    case 310:
      return "Sine Calibration";
    case 320:
      return "Pseudo-random Calibration";
    case 390:
      return "Generic Calibration";
    case 395:
      return "Calibration Abort";
    case 400:
      return "Beam";
    case 500:
      return "Timing";
    case 1000:
      return "Data Only SEED";
    case 1001:
      return "Data Extension";
    case 2000:
      return "Opaque Data";
    }  /* end switch */

  return NULL;
  
}  /* End of ms_blktdesc() */


/***************************************************************************
 * ms_blktlen():
 *
 * Returns the total length of a given blockette type in bytes or 0 if
 * type unknown.
 ***************************************************************************/
uint16_t
ms_blktlen (uint16_t blkttype, const char *blkt, flag swapflag)
{
  uint16_t blktlen = 0;
  
  switch (blkttype)
    {
    case 100:  /* Sample Rate */
      blktlen = 12;
      break;
    case 200:  /* Generic Event Detection */
      blktlen = 28;
      break;
    case 201:  /* Murdock Event Detection */
      blktlen = 36;
      break;
    case 300:  /* Step Calibration */
      blktlen = 32;
      break;
    case 310:  /* Sine Calibration */
      blktlen = 32;
      break;
    case 320:  /* Pseudo-random Calibration */
      blktlen = 28;
      break;
    case 390:  /* Generic Calibration */
      blktlen = 28;
      break;
    case 395:  /* Calibration Abort */
      blktlen = 16;
      break;
    case 400:  /* Beam */
      blktlen = 16;
      break;
    case 500:  /* Timing */
      blktlen = 8;
      break;
    case 1000: /* Data Only SEED */
      blktlen = 8;
      break;
    case 1001: /* Data Extension */
      blktlen = 8;
      break;
    case 2000: /* Opaque Data */
      /* First 2-byte field after the blockette header is the length */
      if ( blkt )
	{
	  memcpy ((void *) &blktlen, blkt+4, sizeof (int16_t));
	  if ( swapflag ) ms_gswap2 (&blktlen);
	}
      break;
    }  /* end switch */
  
  return blktlen;
  
}  /* End of ms_blktlen() */


/***************************************************************************
 * ms_errorstr():
 *
 * Return a string describing a given libmseed error code or NULL if the
 * code is unknown.
 ***************************************************************************/
char *
ms_errorstr (int errorcode)
{
  switch (errorcode)
    {
    case MS_ENDOFFILE:
      return "End of file reached";
    case MS_NOERROR:
      return "No error";
    case MS_GENERROR:
      return "Generic error";
    case MS_NOTSEED:
      return "No SEED data detected";
    case MS_WRONGLENGTH:
      return "Length of data read does not match record length";
    case MS_OUTOFRANGE:
      return "SEED record length out of range";
    case MS_UNKNOWNFORMAT:
      return "Unknown data encoding format";
    case MS_STBADCOMPFLAG:
      return "Bad Steim compression flag(s) detected";
    }				/* end switch */
  
  return NULL;

}  /* End of ms_blktdesc() */
/***************************************************************************
 * msrutils.c:
 *
 * Generic routines to operate on Mini-SEED records.
 *
 * Written by Chad Trabant
 *   ORFEUS/EC-Project MEREDIAN
 *   IRIS Data Management Center
 *
 * modified: 2012.088
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "libmseed.h"


/***************************************************************************
 * msr_init:
 *
 * Initialize and return an MSRecord struct, allocating memory if
 * needed.  If memory for the fsdh and datasamples fields has been
 * allocated the pointers will be retained for reuse.  If a blockette
 * chain is present all associated memory will be released.
 *
 * Returns a pointer to a MSRecord struct on success or NULL on error.
 ***************************************************************************/
MSRecord *
msr_init ( MSRecord *msr )
{
  void *fsdh = 0;
  void *datasamples = 0;
  
  if ( ! msr )
    {
      msr = (MSRecord *) malloc (sizeof(MSRecord));
    }
  else
    {
      fsdh = msr->fsdh;
      datasamples = msr->datasamples;
      
      if ( msr->blkts )
        msr_free_blktchain (msr);

      if ( msr->ststate )
	free (msr->ststate);
    }
  
  if ( msr == NULL )
    {
      ms_log (2, "msr_init(): Cannot allocate memory\n");
      return NULL;
    }
  
  memset (msr, 0, sizeof (MSRecord));
  
  msr->fsdh = fsdh;
  msr->datasamples = datasamples;
  
  msr->reclen = -1;
  msr->samplecnt = -1;
  msr->byteorder = -1;
  msr->encoding = -1;
  
  return msr;
} /* End of msr_init() */


/***************************************************************************
 * msr_free:
 *
 * Free all memory associated with a MSRecord struct.
 ***************************************************************************/
void
msr_free ( MSRecord **ppmsr )
{
  if ( ppmsr != NULL && *ppmsr != 0 )
    {      
      /* Free fixed section header if populated */
      if ( (*ppmsr)->fsdh )
        free ((*ppmsr)->fsdh);
      
      /* Free blockette chain if populated */
      if ( (*ppmsr)->blkts )
        msr_free_blktchain (*ppmsr);
      
      /* Free datasamples if present */
      if ( (*ppmsr)->datasamples )
	free ((*ppmsr)->datasamples);
      
      /* Free stream processing state if present */
      if ( (*ppmsr)->ststate )
        free ((*ppmsr)->ststate);

      free (*ppmsr);
      
      *ppmsr = NULL;
    }
} /* End of msr_free() */


/***************************************************************************
 * msr_free_blktchain:
 *
 * Free all memory associated with a blockette chain in a MSRecord
 * struct and set MSRecord->blkts to NULL.  Also reset the shortcut
 * blockette pointers.
 ***************************************************************************/
void
msr_free_blktchain ( MSRecord *msr )
{
  if ( msr )
    {
      if ( msr->blkts )
        {
          BlktLink *bc = msr->blkts;
          BlktLink *nb = NULL;
          
          while ( bc )
	    {
	      nb = bc->next;
	      
	      if ( bc->blktdata )
		free (bc->blktdata);
	      
	      free (bc);
	      
	      bc = nb;
	    }
          
          msr->blkts = 0;
        }

      msr->Blkt100  = 0;
      msr->Blkt1000 = 0;
      msr->Blkt1001 = 0;      
    }
} /* End of msr_free_blktchain() */


/***************************************************************************
 * msr_addblockette:
 *
 * Add a blockette to the blockette chain of an MSRecord.  'blktdata'
 * should be the body of the blockette type 'blkttype' of 'length'
 * bytes without the blockette header (type and next offsets).  The
 * 'chainpos' value controls which end of the chain the blockette is
 * added to.  If 'chainpos' is 0 the blockette will be added to the
 * end of the chain (last blockette), other wise it will be added to
 * the beginning of the chain (first blockette).
 *
 * Returns a pointer to the BlktLink added to the chain on success and
 * NULL on error.
 ***************************************************************************/
BlktLink *
msr_addblockette (MSRecord *msr, char *blktdata, int length, int blkttype,
		  int chainpos)
{
  BlktLink *blkt;
  
  if ( ! msr )
    return NULL;
  
  blkt = msr->blkts;
  
  if ( blkt )
    {
      if ( chainpos != 0 )
	{
	  blkt = (BlktLink *) malloc (sizeof(BlktLink));
	  
	  blkt->next = msr->blkts;
	  msr->blkts = blkt;
	}
      else
	{
	  /* Find the last blockette */
	  while ( blkt && blkt->next )
	    {
	      blkt = blkt->next;
	    }
	  
	  blkt->next = (BlktLink *) malloc (sizeof(BlktLink));
	  
	  blkt = blkt->next;
	  blkt->next = 0;
	}
      
      if ( blkt == NULL )
	{
	  ms_log (2, "msr_addblockette(): Cannot allocate memory\n");
	  return NULL;
	}
    }
  else
    {
      msr->blkts = (BlktLink *) malloc (sizeof(BlktLink));
      
      if ( msr->blkts == NULL )
	{
	  ms_log (2, "msr_addblockette(): Cannot allocate memory\n");
	  return NULL;
	}
      
      blkt = msr->blkts;
      blkt->next = 0;
    }
  
  blkt->blktoffset = 0;
  blkt->blkt_type = blkttype;
  blkt->next_blkt = 0;
  
  blkt->blktdata = (char *) malloc (length);
  
  if ( blkt->blktdata == NULL )
    {
      ms_log (2, "msr_addblockette(): Cannot allocate memory\n");
      return NULL;
    }
  
  memcpy (blkt->blktdata, blktdata, length);
  blkt->blktdatalen = length;
  
  /* Setup the shortcut pointer for common blockettes */
  switch ( blkttype )
    {
    case 100:
      msr->Blkt100 = blkt->blktdata;
      break;
    case 1000:
      msr->Blkt1000 = blkt->blktdata;
      break;
    case 1001:
      msr->Blkt1001 = blkt->blktdata;
      break;
    }
  
  return blkt;
} /* End of msr_addblockette() */


/***************************************************************************
 * msr_normalize_header:
 *
 * Normalize header values between the MSRecord struct and the
 * associated fixed-section of the header and blockettes.  Essentially
 * this updates the SEED structured data in the MSRecord.fsdh struct
 * and MSRecord.blkts chain with values stored at the MSRecord level.
 *
 * Returns the header length in bytes on success or -1 on error.
 ***************************************************************************/
int
msr_normalize_header ( MSRecord *msr, flag verbose )
{
  struct blkt_link_s *cur_blkt;
  char seqnum[7];
  int offset = 0;
  int blktcnt = 0;
  int reclenexp = 0;
  int reclenfind;
  
  if ( ! msr )
    return -1;
  
  /* Update values in fixed section of data header */
  if ( msr->fsdh )
    {
      if ( verbose > 2 )
	ms_log (1, "Normalizing fixed section of data header\n");
      
      /* Roll-over sequence number if necessary */
      if ( msr->sequence_number > 999999 )
	msr->sequence_number = 1;
      
      /* Update values in the MSRecord.fsdh struct */
      snprintf (seqnum, 7, "%06d", msr->sequence_number);
      memcpy (msr->fsdh->sequence_number, seqnum, 6);
      msr->fsdh->dataquality = msr->dataquality;
      msr->fsdh->reserved = ' ';
      ms_strncpopen (msr->fsdh->network, msr->network, 2);
      ms_strncpopen (msr->fsdh->station, msr->station, 5);
      ms_strncpopen (msr->fsdh->location, msr->location, 2);
      ms_strncpopen (msr->fsdh->channel, msr->channel, 3);
      ms_hptime2btime (msr->starttime, &(msr->fsdh->start_time));
      
      /* When the sampling rate is <= 32767 Hertz determine the factor
       * and multipler through rational approximation.  For higher rates
       * set the factor and multiplier to 0. */
      if ( msr->samprate <= 32767.0 )
	{
	  ms_genfactmult (msr->samprate, &(msr->fsdh->samprate_fact), &(msr->fsdh->samprate_mult));
	}
      else
	{
	  if ( verbose > 1 )
	    ms_log (1, "Sampling rate too high to approximate factor & multiplier: %g\n",
		    msr->samprate);
	  msr->fsdh->samprate_fact = 0;
	  msr->fsdh->samprate_mult = 0;
	}
      
      offset += 48;
      
      if ( msr->blkts )
	msr->fsdh->blockette_offset = offset;
      else
	msr->fsdh->blockette_offset = 0;
    }
  
  /* Traverse blockette chain and performs necessary updates*/
  cur_blkt = msr->blkts;
  
  if ( cur_blkt && verbose > 2 )
    ms_log (1, "Normalizing blockette chain\n");
  
  while ( cur_blkt )
    {
      offset += 4;
      
      if ( cur_blkt->blkt_type == 100 && msr->Blkt100 )
	{
	  msr->Blkt100->samprate = (float)msr->samprate;
	  offset += sizeof (struct blkt_100_s);
	}
      else if ( cur_blkt->blkt_type == 1000 && msr->Blkt1000 )
	{
	  msr->Blkt1000->byteorder = msr->byteorder;
	  msr->Blkt1000->encoding = msr->encoding;
	  
	  /* Calculate the record length as an exponent of 2 */
	  for (reclenfind=1, reclenexp=1; reclenfind <= MAXRECLEN; reclenexp++)
	    {
	      reclenfind *= 2;
	      if ( reclenfind == msr->reclen ) break;
	    }
	  
	  if ( reclenfind != msr->reclen )
	    {
	      ms_log (2, "msr_normalize_header(): Record length %d is not a power of 2\n",
		      msr->reclen);
	      return -1;
	    }
	  
	  msr->Blkt1000->reclen = reclenexp;
	  
	  offset += sizeof (struct blkt_1000_s);
	}
      
      else if ( cur_blkt->blkt_type == 1001 )
	{
	  hptime_t sec, usec;
	  
	  /* Insert microseconds offset */
	  sec = msr->starttime / (HPTMODULUS / 10000);
	  usec = msr->starttime - (sec * (HPTMODULUS / 10000));
	  usec /= (HPTMODULUS / 1000000);
	  
	  msr->Blkt1001->usec = (int8_t) usec;
	  offset += sizeof (struct blkt_1001_s);
	}
      
      blktcnt++;
      cur_blkt = cur_blkt->next;
    }

  if ( msr->fsdh )
    msr->fsdh->numblockettes = blktcnt;
  
  return offset;
} /* End of msr_normalize_header() */


/***************************************************************************
 * msr_duplicate:
 *
 * Duplicate an MSRecord struct
 * including the fixed-section data
 * header and blockette chain.  If
 * the datadup flag is true and the
 * source MSRecord has associated
 * data samples copy them as well.
 *
 * Returns a pointer to a new MSRecord on success and NULL on error.
 ***************************************************************************/
MSRecord *
msr_duplicate (MSRecord *msr, flag datadup)
{
  MSRecord *dupmsr = 0;
  int samplesize = 0;
  
  if ( ! msr )
    return NULL;
  
  /* Allocate target MSRecord structure */
  if ( (dupmsr = msr_init (NULL)) == NULL )
    return NULL;
  
  /* Copy MSRecord structure */
  memcpy (dupmsr, msr, sizeof(MSRecord));
  
  /* Copy fixed-section data header structure */
  if ( msr->fsdh )
    {
      /* Allocate memory for new FSDH structure */
      if ( (dupmsr->fsdh = (struct fsdh_s *) malloc (sizeof(struct fsdh_s))) == NULL )
	{
	  ms_log (2, "msr_duplicate(): Error allocating memory\n");
	  free (dupmsr);
	  return NULL;
	}
      
      /* Copy the contents */
      memcpy (dupmsr->fsdh, msr->fsdh, sizeof(struct fsdh_s));
    }
  
  /* Copy the blockette chain */
  if ( msr->blkts )
    {
      BlktLink *blkt = msr->blkts;
      BlktLink *next = NULL;
      
      dupmsr->blkts = 0;
      while ( blkt )
	{
	  next = blkt->next;
	  
	  /* Add blockette to chain of new MSRecord */
	  if ( msr_addblockette (dupmsr, blkt->blktdata, blkt->blktdatalen,
				 blkt->blkt_type, 0) == NULL )
	    {
	      ms_log (2, "msr_duplicate(): Error adding blockettes\n");
	      msr_free (&dupmsr);
	      return NULL;
	    }
	  
	  blkt = next;
	}
    }
  
  /* Copy data samples if requested and available */
  if ( datadup && msr->datasamples )
    {
      /* Determine size of samples in bytes */
      samplesize = ms_samplesize (msr->sampletype);
      
      if ( samplesize == 0 )
	{
	  ms_log (2, "msr_duplicate(): unrecognized sample type: '%c'\n",
		  msr->sampletype);
	  free (dupmsr);
	  return NULL;
	}
      
      /* Allocate memory for new data array */
      if ( (dupmsr->datasamples = (void *) malloc ((size_t)(msr->numsamples * samplesize))) == NULL )
	{
	  ms_log (2, "msr_duplicate(): Error allocating memory\n");
	  free (dupmsr);
	  return NULL;
	}
      
      /* Copy the data array */
      memcpy (dupmsr->datasamples, msr->datasamples, ((size_t)(msr->numsamples * samplesize)));
    }
  /* Otherwise make sure the sample array and count are zero */
  else
    {
      dupmsr->datasamples = 0;
      dupmsr->numsamples = 0;
    }
  
  return dupmsr;
} /* End of msr_duplicate() */


/***************************************************************************
 * msr_samprate:
 *
 * Calculate and return a double precision sample rate for the
 * specified MSRecord.  If a Blockette 100 was included and parsed,
 * the "Actual sample rate" (field 3) will be returned, otherwise a
 * nominal sample rate will be calculated from the sample rate factor
 * and multiplier in the fixed section data header.
 *
 * Returns the positive sample rate on success and -1.0 on error.
 ***************************************************************************/
double
msr_samprate (MSRecord *msr)
{
  if ( ! msr )
    return -1.0;
  
  if ( msr->Blkt100 )
    return (double) msr->Blkt100->samprate;
  else
    return msr_nomsamprate (msr);  
} /* End of msr_samprate() */


/***************************************************************************
 * msr_nomsamprate:
 *
 * Calculate a double precision nominal sample rate from the sample
 * rate factor and multiplier in the FSDH struct of the specified
 * MSRecord.
 *
 * Returns the positive sample rate on success and -1.0 on error.
 ***************************************************************************/
double
msr_nomsamprate (MSRecord *msr)
{
  if ( ! msr )
    return -1.0;
  
  return ms_nomsamprate (msr->fsdh->samprate_fact, msr->fsdh->samprate_mult);
} /* End of msr_nomsamprate() */


/***************************************************************************
 * msr_starttime:
 *
 * Convert a btime struct of a FSDH struct of a MSRecord (the record
 * start time) into a high precision epoch time and apply time
 * corrections if any are specified in the header and bit 1 of the
 * activity flags indicates that it has not already been applied.  If
 * a Blockette 1001 is included and has been parsed the microseconds
 * of field 4 are also applied.
 *
 * Returns a high precision epoch time on success and HPTERROR on
 * error.
 ***************************************************************************/
hptime_t
msr_starttime (MSRecord *msr)
{
  hptime_t starttime = msr_starttime_uc (msr);
  
  if ( ! msr || starttime == HPTERROR )
    return HPTERROR;
  
  /* Check if a correction is included and if it has been applied,
     bit 1 of activity flags indicates if it has been appiled */
  
  if ( msr->fsdh->time_correct != 0 &&
       ! (msr->fsdh->act_flags & 0x02) )
    {
      starttime += (hptime_t) msr->fsdh->time_correct * (HPTMODULUS / 10000);
    }
  
  /* Apply microsecond precision in a parsed Blockette 1001 */
  if ( msr->Blkt1001 )
    {
      starttime += (hptime_t) msr->Blkt1001->usec * (HPTMODULUS / 1000000);
    }
  
  return starttime;
} /* End of msr_starttime() */


/***************************************************************************
 * msr_starttime_uc:
 *
 * Convert a btime struct of a FSDH struct of a MSRecord (the record
 * start time) into a high precision epoch time.  This time has no
 * correction(s) applied to it.
 *
 * Returns a high precision epoch time on success and HPTERROR on
 * error.
 ***************************************************************************/
hptime_t
msr_starttime_uc (MSRecord *msr)
{
  if ( ! msr )
    return HPTERROR;

  if ( ! msr->fsdh )
    return HPTERROR;
  
  return ms_btime2hptime (&msr->fsdh->start_time);
} /* End of msr_starttime_uc() */


/***************************************************************************
 * msr_endtime:
 *
 * Calculate the time of the last sample in the record; this is the
 * actual last sample time and *not* the time "covered" by the last
 * sample.
 *
 * Returns the time of the last sample as a high precision epoch time
 * on success and HPTERROR on error.
 ***************************************************************************/
hptime_t
msr_endtime (MSRecord *msr)
{
  hptime_t span = 0;
  
  if ( ! msr )
    return HPTERROR;

  if ( msr->samprate > 0.0 && msr->samplecnt > 0 )
    span = (hptime_t)(((double) (msr->samplecnt - 1) / msr->samprate * HPTMODULUS) + 0.5);
  
  return (msr->starttime + span);
} /* End of msr_endtime() */


/***************************************************************************
 * msr_srcname:
 *
 * Generate a source name string for a specified MSRecord in the
 * format: 'NET_STA_LOC_CHAN' or, if the quality flag is true:
 * 'NET_STA_LOC_CHAN_QUAL'.  The passed srcname must have enough room
 * for the resulting string.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
msr_srcname (MSRecord *msr, char *srcname, flag quality)
{
  char *src = srcname;
  char *cp = srcname;
  
  if ( ! msr || ! srcname )
    return NULL;
  
  /* Build the source name string */
  cp = msr->network;
  while ( *cp ) { *src++ = *cp++; }
  *src++ = '_';
  cp = msr->station;
  while ( *cp ) { *src++ = *cp++; }  
  *src++ = '_';
  cp = msr->location;
  while ( *cp ) { *src++ = *cp++; }  
  *src++ = '_';
  cp = msr->channel;
  while ( *cp ) { *src++ = *cp++; }  
  
  if ( quality )
    {
      *src++ = '_';
      *src++ = msr->dataquality;
    }
  
  *src = '\0';
  
  return srcname;
} /* End of msr_srcname() */


/***************************************************************************
 * msr_print:
 *
 * Prints header values in an MSRecord struct, if 'details' is greater
 * than 0 then detailed information about each blockette is printed.
 * If 'details' is greater than 1 very detailed information is
 * printed.  If no FSDH (msr->fsdh) is present only a single line with
 * basic information is printed.
 ***************************************************************************/
void
msr_print (MSRecord *msr, flag details)
{
  double nomsamprate;
  char srcname[50];
  char time[25];
  char b;
  int idx;
  
  if ( ! msr )
    return;
  
  /* Generate a source name string */
  srcname[0] = '\0';
  msr_srcname (msr, srcname, 0);
  
  /* Generate a start time string */
  ms_hptime2seedtimestr (msr->starttime, time, 1);
  
  /* Report information in the fixed header */
  if ( details > 0 && msr->fsdh )
    {
      nomsamprate = msr_nomsamprate (msr);
      
      ms_log (0, "%s, %06d, %c\n", srcname, msr->sequence_number, msr->dataquality);
      ms_log (0, "             start time: %s\n", time);
      ms_log (0, "      number of samples: %d\n", msr->fsdh->numsamples);
      ms_log (0, "     sample rate factor: %d  (%.10g samples per second)\n",
	      msr->fsdh->samprate_fact, nomsamprate);
      ms_log (0, " sample rate multiplier: %d\n", msr->fsdh->samprate_mult);
      
      if ( details > 1 )
	{
	  /* Activity flags */
	  b = msr->fsdh->act_flags;
	  ms_log (0, "         activity flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] Calibration signals present\n");
	  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Time correction applied\n");
	  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Beginning of an event, station trigger\n");
	  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] End of an event, station detrigger\n");
	  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] A positive leap second happened in this record\n");
	  if ( b & 0x20 ) ms_log (0, "                         [Bit 5] A negative leap second happened in this record\n");
	  if ( b & 0x40 ) ms_log (0, "                         [Bit 6] Event in progress\n");
	  if ( b & 0x80 ) ms_log (0, "                         [Bit 7] Undefined bit set\n");

	  /* I/O and clock flags */
	  b = msr->fsdh->io_flags;
	  ms_log (0, "    I/O and clock flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] Station volume parity error possibly present\n");
	  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Long record read (possibly no problem)\n");
	  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Short record read (record padded)\n");
	  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Start of time series\n");
	  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] End of time series\n");
	  if ( b & 0x20 ) ms_log (0, "                         [Bit 5] Clock locked\n");
	  if ( b & 0x40 ) ms_log (0, "                         [Bit 6] Undefined bit set\n");
	  if ( b & 0x80 ) ms_log (0, "                         [Bit 7] Undefined bit set\n");

	  /* Data quality flags */
	  b = msr->fsdh->dq_flags;
	  ms_log (0, "     data quality flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] Amplifier saturation detected\n");
	  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Digitizer clipping detected\n");
	  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Spikes detected\n");
	  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Glitches detected\n");
	  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] Missing/padded data present\n");
	  if ( b & 0x20 ) ms_log (0, "                         [Bit 5] Telemetry synchronization error\n");
	  if ( b & 0x40 ) ms_log (0, "                         [Bit 6] A digital filter may be charging\n");
	  if ( b & 0x80 ) ms_log (0, "                         [Bit 7] Time tag is questionable\n");
	}

      ms_log (0, "   number of blockettes: %d\n", msr->fsdh->numblockettes);
      ms_log (0, "        time correction: %ld\n", (long int) msr->fsdh->time_correct);
      ms_log (0, "            data offset: %d\n", msr->fsdh->data_offset);
      ms_log (0, " first blockette offset: %d\n", msr->fsdh->blockette_offset);
    }
  else
    {
      ms_log (0, "%s, %06d, %c, %d, %lld samples, %-.10g Hz, %s\n",
	      srcname, msr->sequence_number, msr->dataquality,
	      msr->reclen, (long long int) msr->samplecnt, msr->samprate, time);
    }

  /* Report information in the blockette chain */
  if ( details > 0 && msr->blkts )
    {
      BlktLink *cur_blkt = msr->blkts;
      
      while ( cur_blkt )
	{
	  if ( cur_blkt->blkt_type == 100 )
	    {
	      struct blkt_100_s *blkt_100 = (struct blkt_100_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "          actual sample rate: %.10g\n", blkt_100->samprate);
	      
	      if ( details > 1 )
		{
		  b = blkt_100->flags;
		  ms_log (0, "             undefined flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  
		  ms_log (0, "          reserved bytes (3): %u,%u,%u\n",
			  blkt_100->reserved[0], blkt_100->reserved[1], blkt_100->reserved[2]);
		}
	    }

	  else if ( cur_blkt->blkt_type == 200 )
	    {
	      struct blkt_200_s *blkt_200 = (struct blkt_200_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "            signal amplitude: %g\n", blkt_200->amplitude);
	      ms_log (0, "               signal period: %g\n", blkt_200->period);
	      ms_log (0, "         background estimate: %g\n", blkt_200->background_estimate);
	      
	      if ( details > 1 )
		{
		  b = blkt_200->flags;
		  ms_log (0, "       event detection flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] 1: Dilatation wave\n");
		  else            ms_log (0, "                         [Bit 0] 0: Compression wave\n");
		  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] 1: Units after deconvolution\n");
		  else            ms_log (0, "                         [Bit 1] 0: Units are digital counts\n");
		  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Bit 0 is undetermined\n");
		  ms_log (0, "               reserved byte: %u\n", blkt_200->reserved);
		}
	      
	      ms_btime2seedtimestr (&blkt_200->time, time);
	      ms_log (0, "           signal onset time: %s\n", time);
	      ms_log (0, "               detector name: %.24s\n", blkt_200->detector);
	    }

	  else if ( cur_blkt->blkt_type == 201 )
	    {
	      struct blkt_201_s *blkt_201 = (struct blkt_201_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "            signal amplitude: %g\n", blkt_201->amplitude);
	      ms_log (0, "               signal period: %g\n", blkt_201->period);
	      ms_log (0, "         background estimate: %g\n", blkt_201->background_estimate);
	      
	      b = blkt_201->flags;
	      ms_log (0, "       event detection flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	      if ( b & 0x01 ) ms_log (0, "                         [Bit 0] 1: Dilation wave\n");
	      else            ms_log (0, "                         [Bit 0] 0: Compression wave\n");

	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_201->reserved);	      
	      ms_btime2seedtimestr (&blkt_201->time, time);
	      ms_log (0, "           signal onset time: %s\n", time);
	      ms_log (0, "                  SNR values: ");
	      for (idx=0; idx < 6; idx++) ms_log (0, "%u  ", blkt_201->snr_values[idx]);
	      ms_log (0, "\n");
	      ms_log (0, "              loopback value: %u\n", blkt_201->loopback);
	      ms_log (0, "              pick algorithm: %u\n", blkt_201->pick_algorithm);
	      ms_log (0, "               detector name: %.24s\n", blkt_201->detector);
	    }

	  else if ( cur_blkt->blkt_type == 300 )
	    {
	      struct blkt_300_s *blkt_300 = (struct blkt_300_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_btime2seedtimestr (&blkt_300->time, time);
	      ms_log (0, "      calibration start time: %s\n", time);
	      ms_log (0, "      number of calibrations: %u\n", blkt_300->numcalibrations);
	      
	      b = blkt_300->flags;
	      ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	      if ( b & 0x01 ) ms_log (0, "                         [Bit 0] First pulse is positive\n");
	      if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Calibration's alternate sign\n");
	      if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
	      if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
	      
	      ms_log (0, "               step duration: %u\n", blkt_300->step_duration);
	      ms_log (0, "           interval duration: %u\n", blkt_300->interval_duration);
	      ms_log (0, "            signal amplitude: %g\n", blkt_300->amplitude);
	      ms_log (0, "        input signal channel: %.3s", blkt_300->input_channel);
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_300->reserved);
	      ms_log (0, "         reference amplitude: %u\n", blkt_300->reference_amplitude);
	      ms_log (0, "                    coupling: %.12s\n", blkt_300->coupling);
	      ms_log (0, "                     rolloff: %.12s\n", blkt_300->rolloff);
	    }
	  
	  else if ( cur_blkt->blkt_type == 310 )
	    {
	      struct blkt_310_s *blkt_310 = (struct blkt_310_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_btime2seedtimestr (&blkt_310->time, time);
	      ms_log (0, "      calibration start time: %s\n", time);
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_310->reserved1);
	      
	      b = blkt_310->flags;
	      ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	      if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
	      if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
	      if ( b & 0x10 ) ms_log (0, "                         [Bit 4] Peak-to-peak amplitude\n");
	      if ( b & 0x20 ) ms_log (0, "                         [Bit 5] Zero-to-peak amplitude\n");
	      if ( b & 0x40 ) ms_log (0, "                         [Bit 6] RMS amplitude\n");
	      
	      ms_log (0, "        calibration duration: %u\n", blkt_310->duration);
	      ms_log (0, "               signal period: %g\n", blkt_310->period);
	      ms_log (0, "            signal amplitude: %g\n", blkt_310->amplitude);
	      ms_log (0, "        input signal channel: %.3s", blkt_310->input_channel);
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_310->reserved2);	      
	      ms_log (0, "         reference amplitude: %u\n", blkt_310->reference_amplitude);
	      ms_log (0, "                    coupling: %.12s\n", blkt_310->coupling);
	      ms_log (0, "                     rolloff: %.12s\n", blkt_310->rolloff);
	    }

	  else if ( cur_blkt->blkt_type == 320 )
	    {
	      struct blkt_320_s *blkt_320 = (struct blkt_320_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_btime2seedtimestr (&blkt_320->time, time);
	      ms_log (0, "      calibration start time: %s\n", time);
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_320->reserved1);
	      
	      b = blkt_320->flags;
	      ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	      if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
	      if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
	      if ( b & 0x10 ) ms_log (0, "                         [Bit 4] Random amplitudes\n");
	      
	      ms_log (0, "        calibration duration: %u\n", blkt_320->duration);
	      ms_log (0, "      peak-to-peak amplitude: %g\n", blkt_320->ptp_amplitude);
	      ms_log (0, "        input signal channel: %.3s", blkt_320->input_channel);
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_320->reserved2);
	      ms_log (0, "         reference amplitude: %u\n", blkt_320->reference_amplitude);
	      ms_log (0, "                    coupling: %.12s\n", blkt_320->coupling);
	      ms_log (0, "                     rolloff: %.12s\n", blkt_320->rolloff);
	      ms_log (0, "                  noise type: %.8s\n", blkt_320->noise_type);
	    }
	  
	  else if ( cur_blkt->blkt_type == 390 )
	    {
	      struct blkt_390_s *blkt_390 = (struct blkt_390_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_btime2seedtimestr (&blkt_390->time, time);
	      ms_log (0, "      calibration start time: %s\n", time);
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_390->reserved1);
	      
	      b = blkt_390->flags;
	      ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	      if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
	      if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
	      
	      ms_log (0, "        calibration duration: %u\n", blkt_390->duration);
	      ms_log (0, "            signal amplitude: %g\n", blkt_390->amplitude);
	      ms_log (0, "        input signal channel: %.3s", blkt_390->input_channel);
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_390->reserved2);
	    }

	  else if ( cur_blkt->blkt_type == 395 )
	    {
	      struct blkt_395_s *blkt_395 = (struct blkt_395_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_btime2seedtimestr (&blkt_395->time, time);
	      ms_log (0, "        calibration end time: %s\n", time);
	      if ( details > 1 )
		ms_log (0, "          reserved bytes (2): %u,%u\n",
			blkt_395->reserved[0], blkt_395->reserved[1]);
	    }

	  else if ( cur_blkt->blkt_type == 400 )
	    {
	      struct blkt_400_s *blkt_400 = (struct blkt_400_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "      beam azimuth (degrees): %g\n", blkt_400->azimuth);
	      ms_log (0, "  beam slowness (sec/degree): %g\n", blkt_400->slowness);
	      ms_log (0, "               configuration: %u\n", blkt_400->configuration);
	      if ( details > 1 )
		ms_log (0, "          reserved bytes (2): %u,%u\n",
			blkt_400->reserved[0], blkt_400->reserved[1]);
	    }

	  else if ( cur_blkt->blkt_type == 405 )
	    {
	      struct blkt_405_s *blkt_405 = (struct blkt_405_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s, incomplete)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "           first delay value: %u\n", blkt_405->delay_values[0]);
	    }

	  else if ( cur_blkt->blkt_type == 500 )
	    {
	      struct blkt_500_s *blkt_500 = (struct blkt_500_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "              VCO correction: %g%%\n", blkt_500->vco_correction);
	      ms_btime2seedtimestr (&blkt_500->time, time);
	      ms_log (0, "           time of exception: %s\n", time);
	      ms_log (0, "                        usec: %d\n", blkt_500->usec);
	      ms_log (0, "           reception quality: %u%%\n", blkt_500->reception_qual);
	      ms_log (0, "             exception count: %u\n", blkt_500->exception_count);
	      ms_log (0, "              exception type: %.16s\n", blkt_500->exception_type);
	      ms_log (0, "                 clock model: %.32s\n", blkt_500->clock_model);
	      ms_log (0, "                clock status: %.128s\n", blkt_500->clock_status);
	    }
	  
	  else if ( cur_blkt->blkt_type == 1000 )
	    {
	      struct blkt_1000_s *blkt_1000 = (struct blkt_1000_s *) cur_blkt->blktdata;
	      int recsize;
	      char order[40];
	      
	      /* Calculate record size in bytes as 2^(blkt_1000->rec_len) */
	      recsize = (unsigned int) 1 << blkt_1000->reclen;
	      
	      /* Big or little endian? */
	      if (blkt_1000->byteorder == 0)
		strncpy (order, "Little endian", sizeof(order)-1);
	      else if (blkt_1000->byteorder == 1)
		strncpy (order, "Big endian", sizeof(order)-1);
	      else
		strncpy (order, "Unknown value", sizeof(order)-1);
	      
	      ms_log (0, "         BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "                    encoding: %s (val:%u)\n",
		      (char *) ms_encodingstr (blkt_1000->encoding), blkt_1000->encoding);
	      ms_log (0, "                  byte order: %s (val:%u)\n",
		      order, blkt_1000->byteorder);
	      ms_log (0, "               record length: %d (val:%u)\n",
		      recsize, blkt_1000->reclen);
	      
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_1000->reserved);
	    }
	  
	  else if ( cur_blkt->blkt_type == 1001 )
	    {
	      struct blkt_1001_s *blkt_1001 = (struct blkt_1001_s *) cur_blkt->blktdata;
	      
	      ms_log (0, "         BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "              timing quality: %u%%\n", blkt_1001->timing_qual);
	      ms_log (0, "                micro second: %d\n", blkt_1001->usec);
	      
	      if ( details > 1 )
		ms_log (0, "               reserved byte: %u\n", blkt_1001->reserved);
	      
	      ms_log (0, "                 frame count: %u\n", blkt_1001->framecnt);
	    }

	  else if ( cur_blkt->blkt_type == 2000 )
	    {
	      struct blkt_2000_s *blkt_2000 = (struct blkt_2000_s *) cur_blkt->blktdata;
	      char order[40];
	      
	      /* Big or little endian? */
	      if (blkt_2000->byteorder == 0)
		strncpy (order, "Little endian", sizeof(order)-1);
	      else if (blkt_2000->byteorder == 1)
		strncpy (order, "Big endian", sizeof(order)-1);
	      else
		strncpy (order, "Unknown value", sizeof(order)-1);
	      
	      ms_log (0, "         BLOCKETTE %u: (%s)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	      ms_log (0, "            blockette length: %u\n", blkt_2000->length);
	      ms_log (0, "                 data offset: %u\n", blkt_2000->data_offset);
	      ms_log (0, "               record number: %u\n", blkt_2000->recnum);
	      ms_log (0, "                  byte order: %s (val:%u)\n",
		      order, blkt_2000->byteorder);
	      b = blkt_2000->flags;
	      ms_log (0, "                  data flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));

	      if ( details > 1 )
		{
		  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] 1: Stream oriented\n");
		  else            ms_log (0, "                         [Bit 0] 0: Record oriented\n");
		  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] 1: Blockette 2000s may NOT be packaged\n");
		  else            ms_log (0, "                         [Bit 1] 0: Blockette 2000s may be packaged\n");
		  if ( ! (b & 0x04) && ! (b & 0x08) )
		                  ms_log (0, "                      [Bits 2-3] 00: Complete blockette\n");
		  else if ( ! (b & 0x04) && (b & 0x08) )
		                  ms_log (0, "                      [Bits 2-3] 01: First blockette in span\n");
		  else if ( (b & 0x04) && (b & 0x08) )
		                  ms_log (0, "                      [Bits 2-3] 11: Continuation blockette in span\n");
		  else if ( (b & 0x04) && ! (b & 0x08) )
		                  ms_log (0, "                      [Bits 2-3] 10: Final blockette in span\n");
		  if ( ! (b & 0x10) && ! (b & 0x20) )
		                  ms_log (0, "                      [Bits 4-5] 00: Not file oriented\n");
		  else if ( ! (b & 0x10) && (b & 0x20) )
		                  ms_log (0, "                      [Bits 4-5] 01: First blockette of file\n");
		  else if ( (b & 0x10) && ! (b & 0x20) )
		                  ms_log (0, "                      [Bits 4-5] 10: Continuation of file\n");
		  else if ( (b & 0x10) && (b & 0x20) )
		                  ms_log (0, "                      [Bits 4-5] 11: Last blockette of file\n");
		}
	      
	      ms_log (0, "           number of headers: %u\n", blkt_2000->numheaders);
	      
	      /* Crude display of the opaque data headers */
	      if ( details > 1 )
		ms_log (0, "                     headers: %.*s\n",
			(blkt_2000->data_offset - 15), blkt_2000->payload);
	    }
	  
	  else
	    {
	      ms_log (0, "         BLOCKETTE %u: (%s, not parsed)\n", cur_blkt->blkt_type,
		      ms_blktdesc(cur_blkt->blkt_type));
	      ms_log (0, "              next blockette: %u\n", cur_blkt->next_blkt);
	    }
	  
	  cur_blkt = cur_blkt->next;
	}
    }
} /* End of msr_print() */


/***************************************************************************
 * msr_host_latency:
 *
 * Calculate the latency based on the host time in UTC accounting for
 * the time covered using the number of samples and sample rate; in
 * other words, the difference between the host time and the time of
 * the last sample in the specified Mini-SEED record.
 *
 * Double precision is returned, but the true precision is dependent
 * on the accuracy of the host system clock among other things.
 *
 * Returns seconds of latency or 0.0 on error (indistinguishable from
 * 0.0 latency).
 ***************************************************************************/
double
msr_host_latency (MSRecord *msr)
{
  double span = 0.0;            /* Time covered by the samples */
  double epoch;                 /* Current epoch time */
  double latency = 0.0;
  time_t tv;

  if ( msr == NULL )
    return 0.0;
  
  /* Calculate the time covered by the samples */
  if ( msr->samprate > 0.0 && msr->samplecnt > 0 )
    span = (1.0 / msr->samprate) * (msr->samplecnt - 1);
  
  /* Grab UTC time according to the system clock */
  epoch = (double) time(&tv);
  
  /* Now calculate the latency */
  latency = epoch - ((double) msr->starttime / HPTMODULUS) - span;
  
  return latency;
} /* End of msr_host_latency() */
/***************************************************************************
 * pack.c:
 *
 * Generic routines to pack Mini-SEED records using an MSrecord as a
 * header template and data source.
 *
 * Written by Chad Trabant,
 *   IRIS Data Management Center
 *
 * modified: 2012.090
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "libmseed.h"
#include "packdata.h"

/* Function(s) internal to this file */
static int msr_pack_header_raw (MSRecord *msr, char *rawrec, int maxheaderlen,
				flag swapflag, flag normalize,
				struct blkt_1001_s **blkt1001, flag verbose);
static int msr_update_header (MSRecord * msr, char *rawrec, flag swapflag,
			      struct blkt_1001_s *blkt1001, flag verbose);   
static int msr_pack_data (void *dest, void *src, int maxsamples, int maxdatabytes,
			  int *packsamples, int32_t *lastintsample, flag comphistory,
			  char sampletype, flag encoding, flag swapflag,
			  flag verbose);

/* Header and data byte order flags controlled by environment variables */
/* -2 = not checked, -1 = checked but not set, or 0 = LE and 1 = BE */
flag packheaderbyteorder = -2;
flag packdatabyteorder = -2;

/* A pointer to the srcname of the record being packed */
char *PACK_SRCNAME = NULL;


/***************************************************************************
 * msr_pack:
 *
 * Pack data into SEED data records.  Using the record header values
 * in the MSRecord as a template the common header fields are packed
 * into the record header, blockettes in the blockettes chain are
 * packed and data samples are packed in the encoding format indicated
 * by the MSRecord->encoding field.  A Blockette 1000 will be added if
 * one is not present.
 *
 * The MSRecord->datasamples array and MSRecord->numsamples value will
 * not be changed by this routine.  It is the responsibility of the
 * calling routine to adjust the data buffer if desired.
 *
 * As each record is filled and finished they are passed to
 * record_handler which expects 1) a char * to the record, 2) the
 * length of the record and 3) a pointer supplied by the original
 * caller containing optional private data (handlerdata).  It is the
 * responsibility of record_handler to process the record, the memory
 * will be re-used or freed when record_handler returns.
 *
 * If the flush flag != 0 all of the data will be packed into data
 * records even though the last one will probably not be filled.
 *
 * Default values are: data record & quality indicator = 'D', record
 * length = 4096, encoding = 11 (Steim2) and byteorder = 1 (MSBF).
 * The defaults are triggered when the the msr->dataquality is 0 or
 * msr->reclen, msr->encoding and msr->byteorder are -1 respectively.
 *
 * Returns the number of records created on success and -1 on error.
 ***************************************************************************/
int
msr_pack ( MSRecord * msr, void (*record_handler) (char *, int, void *),
	   void *handlerdata, int64_t *packedsamples, flag flush, flag verbose )
{
  uint16_t *HPnumsamples;
  uint16_t *HPdataoffset;
  struct blkt_1001_s *HPblkt1001 = NULL;
  
  char *rawrec;
  char *envvariable;
  char srcname[50];
  
  flag headerswapflag = 0;
  flag dataswapflag = 0;
  flag packret;
  
  int samplesize;
  int headerlen;
  int dataoffset;
  int maxdatabytes;
  int maxsamples;
  int recordcnt = 0;
  int packsamples, packoffset;
  int64_t totalpackedsamples;
  hptime_t segstarttime;
  
  if ( ! msr )
    return -1;
  
  if ( ! record_handler )
    {
      ms_log (2, "msr_pack(): record_handler() function pointer not set!\n");
      return -1;
    }

  /* Allocate stream processing state space if needed */
  if ( ! msr->ststate )
    {
      msr->ststate = (StreamState *) malloc (sizeof(StreamState));
      if ( ! msr->ststate )
        {
          ms_log (2, "msr_pack(): Could not allocate memory for StreamState\n");
          return -1;
        }
      memset (msr->ststate, 0, sizeof(StreamState));
    }

  /* Generate source name for MSRecord */
  if ( msr_srcname (msr, srcname, 1) == NULL )
    {
      ms_log (2, "msr_unpack_data(): Cannot generate srcname\n");
      return MS_GENERROR;
    }
  
  /* Set shared srcname pointer to source name */
  PACK_SRCNAME = &srcname[0];
  
  /* Track original segment start time for new start time calculation */
  segstarttime = msr->starttime;

  /* Read possible environmental variables that force byteorder */
  if ( packheaderbyteorder == -2 )
    {
      if ( (envvariable = getenv("PACK_HEADER_BYTEORDER")) )
	{
	  if ( *envvariable != '0' && *envvariable != '1' )
	    {
	      ms_log (2, "Environment variable PACK_HEADER_BYTEORDER must be set to '0' or '1'\n");
	      return -1;
	    }
	  else if ( *envvariable == '0' )
	    {
	      packheaderbyteorder = 0;
	      if ( verbose > 2 )
		ms_log (1, "PACK_HEADER_BYTEORDER=0, packing little-endian header\n");
	    }
	  else
	    {
	      packheaderbyteorder = 1;
	      if ( verbose > 2 )
		ms_log (1, "PACK_HEADER_BYTEORDER=1, packing big-endian header\n");
	    }
	}
      else
	{
	  packheaderbyteorder = -1;
	}
    }
  if ( packdatabyteorder == -2 )
    {
      if ( (envvariable = getenv("PACK_DATA_BYTEORDER")) )
	{
	  if ( *envvariable != '0' && *envvariable != '1' )
	    {
	      ms_log (2, "Environment variable PACK_DATA_BYTEORDER must be set to '0' or '1'\n");
	      return -1;
	    }
	  else if ( *envvariable == '0' )
	    {
	      packdatabyteorder = 0;
	      if ( verbose > 2 )
		ms_log (1, "PACK_DATA_BYTEORDER=0, packing little-endian data samples\n");
	    }
	  else
	    {
	      packdatabyteorder = 1;
	      if ( verbose > 2 )
		ms_log (1, "PACK_DATA_BYTEORDER=1, packing big-endian data samples\n");
	    }
	}
      else
	{
	  packdatabyteorder = -1;
	}
    }

  /* Set default indicator, record length, byte order and encoding if needed */
  if ( msr->dataquality == 0 ) msr->dataquality = 'D';
  if ( msr->reclen == -1 ) msr->reclen = 4096;
  if ( msr->byteorder == -1 )  msr->byteorder = 1;
  if ( msr->encoding == -1 ) msr->encoding = DE_STEIM2;
  
  /* Cleanup/reset sequence number */
  if ( msr->sequence_number <= 0 || msr->sequence_number > 999999)
    msr->sequence_number = 1;
  
  if ( msr->reclen < MINRECLEN || msr->reclen > MAXRECLEN )
    {
      ms_log (2, "msr_pack(%s): Record length is out of range: %d\n",
	      PACK_SRCNAME, msr->reclen);
      return -1;
    }
  
  if ( msr->numsamples <= 0 )
    {
      ms_log (2, "msr_pack(%s): No samples to pack\n", PACK_SRCNAME);
      return -1;
    }
  
  samplesize = ms_samplesize (msr->sampletype);
  
  if ( ! samplesize )
    {
      ms_log (2, "msr_pack(%s): Unknown sample type '%c'\n",
	      PACK_SRCNAME, msr->sampletype);
      return -1;
    }
  
  /* Sanity check for msr/quality indicator */
  if ( ! MS_ISDATAINDICATOR(msr->dataquality) )
    {
      ms_log (2, "msr_pack(%s): Record header & quality indicator unrecognized: '%c'\n",
	      PACK_SRCNAME, msr->dataquality);
      ms_log (2, "msr_pack(%s): Packing failed.\n", PACK_SRCNAME);
      return -1;
    }
  
  /* Allocate space for data record */
  rawrec = (char *) malloc (msr->reclen);
  
  if ( rawrec == NULL )
    {
      ms_log (2, "msr_pack(%s): Cannot allocate memory\n", PACK_SRCNAME);
      return -1;
    }
  
  /* Set header pointers to known offsets into FSDH */
  HPnumsamples = (uint16_t *) (rawrec + 30);
  HPdataoffset = (uint16_t *) (rawrec + 44);
  
  /* Check to see if byte swapping is needed */
  if ( msr->byteorder != ms_bigendianhost() )
    headerswapflag = dataswapflag = 1;
  
  /* Check if byte order is forced */
  if ( packheaderbyteorder >= 0 )
    {
      headerswapflag = ( msr->byteorder != packheaderbyteorder ) ? 1 : 0;
    }
  
  if ( packdatabyteorder >= 0 )
    {
      dataswapflag = ( msr->byteorder != packdatabyteorder ) ? 1 : 0;
    }
  
  if ( verbose > 2 )
    {
      if ( headerswapflag && dataswapflag )
	ms_log (1, "%s: Byte swapping needed for packing of header and data samples\n", PACK_SRCNAME);
      else if ( headerswapflag )
	ms_log (1, "%s: Byte swapping needed for packing of header\n", PACK_SRCNAME);
      else if ( dataswapflag )
	ms_log (1, "%s: Byte swapping needed for packing of data samples\n", PACK_SRCNAME);
      else
	ms_log (1, "%s: Byte swapping NOT needed for packing\n", PACK_SRCNAME);
    }
  
  /* Add a blank 1000 Blockette if one is not present, the blockette values
     will be populated in msr_pack_header_raw()/msr_normalize_header() */
  if ( ! msr->Blkt1000 )
    {
      struct blkt_1000_s blkt1000;
      memset (&blkt1000, 0, sizeof (struct blkt_1000_s));
      
      if ( verbose > 2 )
	ms_log (1, "%s: Adding 1000 Blockette\n", PACK_SRCNAME);
      
      if ( ! msr_addblockette (msr, (char *) &blkt1000, sizeof(struct blkt_1000_s), 1000, 0) )
	{
	  ms_log (2, "msr_pack(%s): Error adding 1000 Blockette\n", PACK_SRCNAME);
	  return -1;
	}
    }
  
  headerlen = msr_pack_header_raw (msr, rawrec, msr->reclen, headerswapflag, 1, &HPblkt1001, verbose);
  
  if ( headerlen == -1 )
    {
      ms_log (2, "msr_pack(%s): Error packing header\n", PACK_SRCNAME);
      return -1;
    }
  
  /* Determine offset to encoded data */
  if ( msr->encoding == DE_STEIM1 || msr->encoding == DE_STEIM2 )
    {
      dataoffset = 64;
      while ( dataoffset < headerlen )
	dataoffset += 64;
      
      /* Zero memory between blockettes and data if any */
      memset (rawrec + headerlen, 0, dataoffset - headerlen);
    }
  else
    {
      dataoffset = headerlen;
    }
  
  *HPdataoffset = (uint16_t) dataoffset;
  if ( headerswapflag ) ms_gswap2 (HPdataoffset);
  
  /* Determine the max data bytes and sample count */
  maxdatabytes = msr->reclen - dataoffset;
  
  if ( msr->encoding == DE_STEIM1 )
    {
      maxsamples = (int) (maxdatabytes/64) * STEIM1_FRAME_MAX_SAMPLES;
    }
  else if ( msr->encoding == DE_STEIM2 )
    {
      maxsamples = (int) (maxdatabytes/64) * STEIM2_FRAME_MAX_SAMPLES;
    }
  else
    {
      maxsamples = maxdatabytes / samplesize;
    }
  
  /* Pack samples into records */
  *HPnumsamples = 0;
  totalpackedsamples = 0;
  if ( packedsamples ) *packedsamples = 0;
  packoffset = 0;
  
  while ( (msr->numsamples - totalpackedsamples) > maxsamples || flush )
    {
      packret = msr_pack_data (rawrec + dataoffset,
			       (char *) msr->datasamples + packoffset,
			       (int)(msr->numsamples - totalpackedsamples), maxdatabytes,
			       &packsamples, &msr->ststate->lastintsample, msr->ststate->comphistory,
			       msr->sampletype, msr->encoding, dataswapflag, verbose);
      
      if ( packret )
	{
	  ms_log (2, "msr_pack(%s): Error packing record\n", PACK_SRCNAME);
	  return -1;
	}
      
      packoffset += packsamples * samplesize;
      
      /* Update number of samples */
      *HPnumsamples = (uint16_t) packsamples;
      if ( headerswapflag ) ms_gswap2 (HPnumsamples);
      
      if ( verbose > 0 )
	ms_log (1, "%s: Packed %d samples\n", PACK_SRCNAME, packsamples);
      
      /* Send record to handler */
      record_handler (rawrec, msr->reclen, handlerdata);
      
      totalpackedsamples += packsamples;
      if ( packedsamples ) *packedsamples = totalpackedsamples;
      msr->ststate->packedsamples += packsamples;
      
      /* Update record header for next record */
      msr->sequence_number = ( msr->sequence_number >= 999999 ) ? 1 : msr->sequence_number + 1;
      if ( msr->samprate > 0 )
	msr->starttime = segstarttime + (hptime_t)(totalpackedsamples / msr->samprate * HPTMODULUS + 0.5);
      
      msr_update_header (msr, rawrec, headerswapflag, HPblkt1001, verbose);
      
      recordcnt++;
      msr->ststate->packedrecords++;
      
      /* Set compression history flag for subsequent records (Steim encodings) */
      if ( ! msr->ststate->comphistory )
        msr->ststate->comphistory = 1;
     
      if ( totalpackedsamples >= msr->numsamples )
	break;
    }
  
  if ( verbose > 2 )
    ms_log (1, "%s: Packed %d total samples\n", PACK_SRCNAME, totalpackedsamples);
  
  free (rawrec);
  
  return recordcnt;
} /* End of msr_pack() */


/***************************************************************************
 * msr_pack_header:
 *
 * Pack data header/blockettes into the SEED record at
 * MSRecord->record.  Unlike msr_pack no default values are applied,
 * the header structures are expected to be self describing and no
 * Blockette 1000 will be added.  This routine is only useful for
 * re-packing a record header.
 *
 * Returns the header length in bytes on success and -1 on error.
 ***************************************************************************/
int
msr_pack_header ( MSRecord *msr, flag normalize, flag verbose )
{
  char srcname[50];
  char *envvariable;
  flag headerswapflag = 0;
  int headerlen;
  int maxheaderlen;
  
  if ( ! msr )
    return -1;
  
  /* Generate source name for MSRecord */
  if ( msr_srcname (msr, srcname, 1) == NULL )
    {
      ms_log (2, "msr_unpack_data(): Cannot generate srcname\n");
      return MS_GENERROR;
    }
  
  /* Set shared srcname pointer to source name */
  PACK_SRCNAME = &srcname[0];

  /* Read possible environmental variables that force byteorder */
  if ( packheaderbyteorder == -2 )
    {
      if ( (envvariable = getenv("PACK_HEADER_BYTEORDER")) )
	{
	  if ( *envvariable != '0' && *envvariable != '1' )
	    {
	      ms_log (2, "Environment variable PACK_HEADER_BYTEORDER must be set to '0' or '1'\n");
	      return -1;
	    }
	  else if ( *envvariable == '0' )
	    {
	      packheaderbyteorder = 0;
	      if ( verbose > 2 )
		ms_log (1, "PACK_HEADER_BYTEORDER=0, packing little-endian header\n");
	    }
	  else
	    {
	      packheaderbyteorder = 1;
	      if ( verbose > 2 )
		ms_log (1, "PACK_HEADER_BYTEORDER=1, packing big-endian header\n");
	    }
	}
      else
	{
	  packheaderbyteorder = -1;
	}
    }

  if ( msr->reclen < MINRECLEN || msr->reclen > MAXRECLEN )
    {
      ms_log (2, "msr_pack_header(%s): record length is out of range: %d\n",
	      PACK_SRCNAME, msr->reclen);
      return -1;
    }
  
  if ( msr->byteorder != 0 && msr->byteorder != 1 )
    {
      ms_log (2, "msr_pack_header(%s): byte order is not defined correctly: %d\n",
	      PACK_SRCNAME, msr->byteorder);
      return -1;
    }
    
  if ( msr->fsdh )
    {
      maxheaderlen = (msr->fsdh->data_offset > 0) ?
	msr->fsdh->data_offset :
	msr->reclen;
    }
  else
    {
      maxheaderlen = msr->reclen;
    }
    
  /* Check to see if byte swapping is needed */
  if ( msr->byteorder != ms_bigendianhost() )
    headerswapflag = 1;
  
  /* Check if byte order is forced */
  if ( packheaderbyteorder >= 0 )
    {
      headerswapflag = ( msr->byteorder != packheaderbyteorder ) ? 1: 0;
    }
  
  if ( verbose > 2 )
    {
      if ( headerswapflag )
	ms_log (1, "%s: Byte swapping needed for packing of header\n", PACK_SRCNAME);
      else
	ms_log (1, "%s: Byte swapping NOT needed for packing of header\n", PACK_SRCNAME);
    }
  
  headerlen = msr_pack_header_raw (msr, msr->record, maxheaderlen,
				   headerswapflag, normalize, NULL, verbose);
  
  return headerlen;
}  /* End of msr_pack_header() */


/***************************************************************************
 * msr_pack_header_raw:
 *
 * Pack data header/blockettes into the specified SEED data record.
 *
 * Returns the header length in bytes on success or -1 on error.
 ***************************************************************************/
static int
msr_pack_header_raw ( MSRecord *msr, char *rawrec, int maxheaderlen,
		      flag swapflag, flag normalize, 
		      struct blkt_1001_s **blkt1001, flag verbose )
{
  struct blkt_link_s *cur_blkt;
  struct fsdh_s *fsdh;
  int16_t offset;
  int blktcnt = 0;
  int nextoffset;

  if ( ! msr || ! rawrec )
    return -1;
  
  /* Make sure a fixed section of data header is available */
  if ( ! msr->fsdh )
    {
      msr->fsdh = (struct fsdh_s *) calloc (1, sizeof (struct fsdh_s));
      
      if ( msr->fsdh == NULL )
	{
	  ms_log (2, "msr_pack_header_raw(%s): Cannot allocate memory\n",
		  PACK_SRCNAME);
	  return -1;
	}
    }
  
  /* Update the SEED structures associated with the MSRecord */
  if ( normalize )
    if ( msr_normalize_header (msr, verbose) < 0 )
      {
	ms_log (2, "msr_pack_header_raw(%s): error normalizing header values\n",
		PACK_SRCNAME);
	return -1;
      }
  
  if ( verbose > 2 )
    ms_log (1, "%s: Packing fixed section of data header\n", PACK_SRCNAME);
  
  if ( maxheaderlen > msr->reclen )
    {
      ms_log (2, "msr_pack_header_raw(%s): maxheaderlen of %d is beyond record length of %d\n",
	      PACK_SRCNAME, maxheaderlen, msr->reclen);
      return -1;
    }
  
  if ( maxheaderlen < sizeof(struct fsdh_s) )
    {
      ms_log (2, "msr_pack_header_raw(%s): maxheaderlen of %d is too small, must be >= %d\n",
	      PACK_SRCNAME, maxheaderlen, sizeof(struct fsdh_s));
      return -1;
    }
  
  fsdh = (struct fsdh_s *) rawrec;
  offset = 48;
  
  /* Roll-over sequence number if necessary */
  if ( msr->sequence_number > 999999 )
    msr->sequence_number = 1;
  
  /* Copy FSDH associated with the MSRecord into the record */  
  memcpy (fsdh, msr->fsdh, sizeof(struct fsdh_s));
  
  /* Swap byte order? */
  if ( swapflag )
    {
      MS_SWAPBTIME (&fsdh->start_time);
      ms_gswap2 (&fsdh->numsamples);
      ms_gswap2 (&fsdh->samprate_fact);
      ms_gswap2 (&fsdh->samprate_mult);
      ms_gswap4 (&fsdh->time_correct);
      ms_gswap2 (&fsdh->data_offset);
      ms_gswap2 (&fsdh->blockette_offset);
    }
  
  /* Traverse blockette chain and pack blockettes at 'offset' */
  cur_blkt = msr->blkts;
  
  while ( cur_blkt && offset < maxheaderlen )
    {
      /* Check that the blockette fits */
      if ( (offset + 4 + cur_blkt->blktdatalen) > maxheaderlen )
	{
	  ms_log (2, "msr_pack_header_raw(%s): header exceeds maxheaderlen of %d\n",
		  PACK_SRCNAME, maxheaderlen);
	  break;
	}
      
      /* Pack blockette type and leave space for next offset */
      memcpy (rawrec + offset, &cur_blkt->blkt_type, 2);
      if ( swapflag ) ms_gswap2 (rawrec + offset);
      nextoffset = offset + 2;
      offset += 4;
      
      if ( cur_blkt->blkt_type == 100 )
	{
	  struct blkt_100_s *blkt_100 = (struct blkt_100_s *) (rawrec + offset);
	  memcpy (blkt_100, cur_blkt->blktdata, sizeof (struct blkt_100_s));
	  offset += sizeof (struct blkt_100_s);
	  
	  if ( swapflag )
	    {
	      ms_gswap4 (&blkt_100->samprate);
	    }
	}
      
      else if ( cur_blkt->blkt_type == 200 )
	{
	  struct blkt_200_s *blkt_200 = (struct blkt_200_s *) (rawrec + offset);
	  memcpy (blkt_200, cur_blkt->blktdata, sizeof (struct blkt_200_s));
	  offset += sizeof (struct blkt_200_s);
	  
	  if ( swapflag )
	    {
	      ms_gswap4 (&blkt_200->amplitude);
	      ms_gswap4 (&blkt_200->period);
	      ms_gswap4 (&blkt_200->background_estimate);
	      MS_SWAPBTIME (&blkt_200->time);
	    }
	}
      
      else if ( cur_blkt->blkt_type == 201 )
	{
	  struct blkt_201_s *blkt_201 = (struct blkt_201_s *) (rawrec + offset);
	  memcpy (blkt_201, cur_blkt->blktdata, sizeof (struct blkt_201_s));
	  offset += sizeof (struct blkt_201_s);
	  
	  if ( swapflag )
	    {
	      ms_gswap4 (&blkt_201->amplitude);
	      ms_gswap4 (&blkt_201->period);
	      ms_gswap4 (&blkt_201->background_estimate);
	      MS_SWAPBTIME (&blkt_201->time);
	    }
	}

      else if ( cur_blkt->blkt_type == 300 )
	{
	  struct blkt_300_s *blkt_300 = (struct blkt_300_s *) (rawrec + offset);
	  memcpy (blkt_300, cur_blkt->blktdata, sizeof (struct blkt_300_s));
	  offset += sizeof (struct blkt_300_s);
	  
	  if ( swapflag )
	    {
	      MS_SWAPBTIME (&blkt_300->time);
	      ms_gswap4 (&blkt_300->step_duration);
	      ms_gswap4 (&blkt_300->interval_duration);
	      ms_gswap4 (&blkt_300->amplitude);
	      ms_gswap4 (&blkt_300->reference_amplitude);
	    }
	}

      else if ( cur_blkt->blkt_type == 310 )
	{
	  struct blkt_310_s *blkt_310 = (struct blkt_310_s *) (rawrec + offset);
	  memcpy (blkt_310, cur_blkt->blktdata, sizeof (struct blkt_310_s));
	  offset += sizeof (struct blkt_310_s);
	  
	  if ( swapflag )
	    {
	      MS_SWAPBTIME (&blkt_310->time);
	      ms_gswap4 (&blkt_310->duration);
	      ms_gswap4 (&blkt_310->period);
	      ms_gswap4 (&blkt_310->amplitude);
	      ms_gswap4 (&blkt_310->reference_amplitude);
	    }
	}
      
      else if ( cur_blkt->blkt_type == 320 )
	{
	  struct blkt_320_s *blkt_320 = (struct blkt_320_s *) (rawrec + offset);
	  memcpy (blkt_320, cur_blkt->blktdata, sizeof (struct blkt_320_s));
	  offset += sizeof (struct blkt_320_s);
	  
	  if ( swapflag )
	    {
	      MS_SWAPBTIME (&blkt_320->time);
	      ms_gswap4 (&blkt_320->duration);
	      ms_gswap4 (&blkt_320->ptp_amplitude);
	      ms_gswap4 (&blkt_320->reference_amplitude);
	    }
	}

      else if ( cur_blkt->blkt_type == 390 )
	{
	  struct blkt_390_s *blkt_390 = (struct blkt_390_s *) (rawrec + offset);
	  memcpy (blkt_390, cur_blkt->blktdata, sizeof (struct blkt_390_s));
	  offset += sizeof (struct blkt_390_s);
	  
	  if ( swapflag )
	    {
	      MS_SWAPBTIME (&blkt_390->time);
	      ms_gswap4 (&blkt_390->duration);
	      ms_gswap4 (&blkt_390->amplitude);
	    }
	}
      
      else if ( cur_blkt->blkt_type == 395 )
	{
	  struct blkt_395_s *blkt_395 = (struct blkt_395_s *) (rawrec + offset);
	  memcpy (blkt_395, cur_blkt->blktdata, sizeof (struct blkt_395_s));
	  offset += sizeof (struct blkt_395_s);
	  
	  if ( swapflag )
	    {
	      MS_SWAPBTIME (&blkt_395->time);
	    }
	}

      else if ( cur_blkt->blkt_type == 400 )
	{
	  struct blkt_400_s *blkt_400 = (struct blkt_400_s *) (rawrec + offset);
	  memcpy (blkt_400, cur_blkt->blktdata, sizeof (struct blkt_400_s));
	  offset += sizeof (struct blkt_400_s);
	  
	  if ( swapflag )
	    {
	      ms_gswap4 (&blkt_400->azimuth);
	      ms_gswap4 (&blkt_400->slowness);
	      ms_gswap2 (&blkt_400->configuration);
	    }
	}

      else if ( cur_blkt->blkt_type == 405 )
	{
	  struct blkt_405_s *blkt_405 = (struct blkt_405_s *) (rawrec + offset);
	  memcpy (blkt_405, cur_blkt->blktdata, sizeof (struct blkt_405_s));
	  offset += sizeof (struct blkt_405_s);
	  
	  if ( swapflag )
	    {
	      ms_gswap2 (&blkt_405->delay_values);
	    }

	  if ( verbose > 0 )
	    {
	      ms_log (1, "msr_pack_header_raw(%s): WARNING Blockette 405 cannot be fully supported\n",
		      PACK_SRCNAME);
	    }
	}

      else if ( cur_blkt->blkt_type == 500 )
	{
	  struct blkt_500_s *blkt_500 = (struct blkt_500_s *) (rawrec + offset);
	  memcpy (blkt_500, cur_blkt->blktdata, sizeof (struct blkt_500_s));
	  offset += sizeof (struct blkt_500_s);
	  
	  if ( swapflag )
	    {
	      ms_gswap4 (&blkt_500->vco_correction);
	      MS_SWAPBTIME (&blkt_500->time);
	      ms_gswap4 (&blkt_500->exception_count);
	    }
	}
      
      else if ( cur_blkt->blkt_type == 1000 )
	{
	  struct blkt_1000_s *blkt_1000 = (struct blkt_1000_s *) (rawrec + offset);
	  memcpy (blkt_1000, cur_blkt->blktdata, sizeof (struct blkt_1000_s));
	  offset += sizeof (struct blkt_1000_s);
	  
	  /* This guarantees that the byte order is in sync with msr_pack() */
	  if ( packdatabyteorder >= 0 )
	    blkt_1000->byteorder = packdatabyteorder;
	}
      
      else if ( cur_blkt->blkt_type == 1001 )
	{
	  struct blkt_1001_s *blkt_1001 = (struct blkt_1001_s *) (rawrec + offset);
	  memcpy (blkt_1001, cur_blkt->blktdata, sizeof (struct blkt_1001_s));
	  offset += sizeof (struct blkt_1001_s);
	  
	  /* Track location of Blockette 1001 if requested */
	  if ( blkt1001 )
	    *blkt1001 = blkt_1001;
	}
      
      else if ( cur_blkt->blkt_type == 2000 )
	{
	  struct blkt_2000_s *blkt_2000 = (struct blkt_2000_s *) (rawrec + offset);
	  memcpy (blkt_2000, cur_blkt->blktdata, cur_blkt->blktdatalen);
	  offset += cur_blkt->blktdatalen;
	  
	  if ( swapflag )
	    {
	      ms_gswap2 (&blkt_2000->length);
	      ms_gswap2 (&blkt_2000->data_offset);
	      ms_gswap4 (&blkt_2000->recnum);
	    }
	  
	  /* Nothing done to pack the opaque headers and data, they should already
	     be packed into the blockette payload */
	}
      
      else
	{
	  memcpy (rawrec + offset, cur_blkt->blktdata, cur_blkt->blktdatalen);
	  offset += cur_blkt->blktdatalen;
	}
      
      /* Pack the offset to the next blockette */
      if ( cur_blkt->next )
	{
	  memcpy (rawrec + nextoffset, &offset, 2);
	  if ( swapflag ) ms_gswap2 (rawrec + nextoffset);
	}
      else
	{
	  memset (rawrec + nextoffset, 0, 2);
	}
      
      blktcnt++;
      cur_blkt = cur_blkt->next;
    }
  
  fsdh->numblockettes = blktcnt;
  
  if ( verbose > 2 )
    ms_log (1, "%s: Packed %d blockettes\n", PACK_SRCNAME, blktcnt);
  
  return offset;
}  /* End of msr_pack_header_raw() */


/***************************************************************************
 * msr_update_header:
 *
 * Update the header values that change between records: start time,
 * sequence number, etc.
 *
 * Returns 0 on success or -1 on error.
 ***************************************************************************/
static int
msr_update_header ( MSRecord *msr, char *rawrec, flag swapflag,
		    struct blkt_1001_s *blkt1001, flag verbose )
{
  struct fsdh_s *fsdh;
  char seqnum[7];
  
  if ( ! msr || ! rawrec )
    return -1;
  
  if ( verbose > 2 )
    ms_log (1, "%s: Updating fixed section of data header\n", PACK_SRCNAME);
  
  fsdh = (struct fsdh_s *) rawrec;
  
  /* Pack values into the fixed section of header */
  snprintf (seqnum, 7, "%06d", msr->sequence_number);
  memcpy (fsdh->sequence_number, seqnum, 6);
  
  /* Update fixed-section start time */
  ms_hptime2btime (msr->starttime, &(fsdh->start_time));
  
  /* Swap byte order? */
  if ( swapflag )
    {
      MS_SWAPBTIME (&fsdh->start_time);
    }
  
  /* Update microseconds if Blockette 1001 is present */
  if ( msr->Blkt1001 && blkt1001 )
    {
      hptime_t sec, usec;
      
      /* Calculate microseconds offset */
      sec = msr->starttime / (HPTMODULUS / 10000);
      usec = msr->starttime - (sec * (HPTMODULUS / 10000));
      usec /= (HPTMODULUS / 1000000);
      
      /* Update microseconds offset in blockette chain entry */
      msr->Blkt1001->usec = (int8_t) usec;
      
      /* Update microseconds offset in packed header */
      blkt1001->usec = (int8_t) usec;
    }
  
  return 0;
}  /* End of msr_update_header() */


/************************************************************************
 *  msr_pack_data:
 *
 *  Pack Mini-SEED data samples.  The input data samples specified as
 *  'src' will be packed with 'encoding' format and placed in 'dest'.
 *  
 *  If a pointer to a 32-bit integer sample is provided in the
 *  argument 'lastintsample' and 'comphistory' is true the sample
 *  value will be used to seed the difference buffer for Steim1/2
 *  encoding and provide a compression history.  It will also be
 *  updated with the last sample packed in order to be used with a
 *  subsequent call to this routine.
 *
 *  The number of samples packed will be placed in 'packsamples' and
 *  the number of bytes packed will be placed in 'packbytes'.
 *
 *  Return 0 on success and a negative number on error.
 ************************************************************************/
static int
msr_pack_data (void *dest, void *src, int maxsamples, int maxdatabytes,
	       int *packsamples, int32_t *lastintsample, flag comphistory,
	       char sampletype, flag encoding, flag swapflag, flag verbose)
{
  int retval;
  int nframes;
  int npacked;
  int32_t *intbuff;
  int32_t d0;
  
  /* Decide if this is a format that we can decode */
  switch (encoding)
    {
      
    case DE_ASCII:
      if ( sampletype != 'a' )
	{
	  ms_log (2, "%s: Sample type must be ascii (a) for ASCII encoding not '%c'\n",
		  PACK_SRCNAME, sampletype);
	  return -1;
	}
      
      if ( verbose > 1 )
	ms_log (1, "%s: Packing ASCII data\n", PACK_SRCNAME);
      
      retval = msr_pack_text (dest, src, maxsamples, maxdatabytes, 1,
			      &npacked, packsamples);
      
      break;
      
    case DE_INT16:
      if ( sampletype != 'i' )
	{
	  ms_log (2, "%s: Sample type must be integer (i) for integer-16 encoding not '%c'\n",
		  PACK_SRCNAME, sampletype);
	  return -1;
	}
      
      if ( verbose > 1 )
	ms_log (1, "%s: Packing INT-16 data samples\n", PACK_SRCNAME);
      
      retval = msr_pack_int_16 (dest, src, maxsamples, maxdatabytes, 1,
				&npacked, packsamples, swapflag);
      
      break;
      
    case DE_INT32:
      if ( sampletype != 'i' )
	{
	  ms_log (2, "%s: Sample type must be integer (i) for integer-32 encoding not '%c'\n",
		  PACK_SRCNAME, sampletype);
	  return -1;
	}

      if ( verbose > 1 )
	ms_log (1, "%s: Packing INT-32 data samples\n", PACK_SRCNAME);
      
      retval = msr_pack_int_32 (dest, src, maxsamples, maxdatabytes, 1,
				&npacked, packsamples, swapflag);
      
      break;
      
    case DE_FLOAT32:
      if ( sampletype != 'f' )
	{
	  ms_log (2, "%s: Sample type must be float (f) for float-32 encoding not '%c'\n",
		  PACK_SRCNAME, sampletype);
	  return -1;
	}

      if ( verbose > 1 )
	ms_log (1, "%s: Packing FLOAT-32 data samples\n", PACK_SRCNAME);
      
      retval = msr_pack_float_32 (dest, src, maxsamples, maxdatabytes, 1,
				  &npacked, packsamples, swapflag);

      break;
      
    case DE_FLOAT64:
      if ( sampletype != 'd' )
	{
	  ms_log (2, "%s: Sample type must be double (d) for float-64 encoding not '%c'\n",
		  PACK_SRCNAME, sampletype);
	  return -1;
	}
      
      if ( verbose > 1 )
	ms_log (1, "%s: Packing FLOAT-64 data samples\n", PACK_SRCNAME);
      
      retval = msr_pack_float_64 (dest, src, maxsamples, maxdatabytes, 1,
				  &npacked, packsamples, swapflag);
      
      break;
      
    case DE_STEIM1:
      if ( sampletype != 'i' )
	{
	  ms_log (2, "%s: Sample type must be integer (i) for Steim-1 compression not '%c'\n",
		  PACK_SRCNAME, sampletype);
	  return -1;
	}
      
      intbuff = (int32_t *) src;
      
      /* If a previous sample is supplied use it for compression history otherwise cold-start */
      d0 = ( lastintsample && comphistory ) ? (intbuff[0] - *lastintsample) : 0;
      
      if ( verbose > 1 )
	ms_log (1, "%s: Packing Steim-1 data frames\n", PACK_SRCNAME);
      
      nframes = maxdatabytes / 64;
      
      retval = msr_pack_steim1 (dest, src, d0, maxsamples, nframes, 1,
				&npacked, packsamples, swapflag);
      
      /* If a previous sample is supplied update it with the last sample value */
      if ( lastintsample && retval == 0 )
	*lastintsample = intbuff[*packsamples-1];
      
      break;
      
    case DE_STEIM2:
      if ( sampletype != 'i' )
	{
	  ms_log (2, "%s: Sample type must be integer (i) for Steim-2 compression not '%c'\n",
		  PACK_SRCNAME, sampletype);
	  return -1;
	}
      
      intbuff = (int32_t *) src;
      
      /* If a previous sample is supplied use it for compression history otherwise cold-start */
      d0 = ( lastintsample && comphistory ) ? (intbuff[0] - *lastintsample) : 0;
      
      if ( verbose > 1 )
	ms_log (1, "%s: Packing Steim-2 data frames\n", PACK_SRCNAME);
      
      nframes = maxdatabytes / 64;
      
      retval = msr_pack_steim2 (dest, src, d0, maxsamples, nframes, 1,
				&npacked, packsamples, swapflag);
      
      /* If a previous sample is supplied update it with the last sample value */
      if ( lastintsample && retval == 0 )
	*lastintsample = intbuff[*packsamples-1];
      
      break;
      
    default:
      ms_log (2, "%s: Unable to pack format %d\n", PACK_SRCNAME, encoding);
      
      return -1;
    }
    
  return retval;
} /* End of msr_pack_data() */
/***********************************************************************
 *  Routines for packing INT_32, INT_16, FLOAT_32, FLOAT_64,
 *  STEIM1 and STEIM2 data records.
 *
 *	Douglas Neuhauser						
 *	Seismological Laboratory					
 *	University of California, Berkeley				
 *	doug@seismo.berkeley.edu					
 *
 *
 * modified Aug 2008:
 *  - Optimize Steim 1 & 2 packing routines using small, re-used buffers.
 *
 * modified Sep 2004:
 *  - Reworked and cleaned routines for use within libmseed.
 *  - Added float32 and float64 packing routines.
 *
 * Modified by Chad Trabant, IRIS Data Management Center
 *
 * modified: 2009.111
 ************************************************************************/

/*
 * Copyright (c) 1996-2004 The Regents of the University of California.
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for educational, research and non-profit purposes,
 * without fee, and without a written agreement is hereby granted,
 * provided that the above copyright notice, this paragraph and the
 * following three paragraphs appear in all copies.
 * 
 * Permission to incorporate this software into commercial products may
 * be obtained from the Office of Technology Licensing, 2150 Shattuck
 * Avenue, Suite 510, Berkeley, CA  94704.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES,
 * INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND
 * ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE
 * PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
 * CALIFORNIA HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT,
 * UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include "libmseed.h"
#include "packdata.h"

static int pad_steim_frame (DFRAMES*, int, int, int, int, int);

#define	EMPTY_BLOCK(fn,wn) (fn+wn == 0)

#define	X0  dframes->f[0].w[0].fw
#define	XN  dframes->f[0].w[1].fw

#define	BIT4PACK(i,points_remaining)		   \
  (points_remaining >= 7 &&			   \
   (minbits[i] <= 4) && (minbits[i+1] <= 4) &&	   \
   (minbits[i+2] <= 4) && (minbits[i+3] <= 4) &&   \
   (minbits[i+4] <= 4) && (minbits[i+5] <= 4) &&   \
   (minbits[i+6] <= 4))

#define	BIT5PACK(i,points_remaining)		   \
  (points_remaining >= 6 &&			   \
   (minbits[i] <= 5) && (minbits[i+1] <= 5) &&	   \
   (minbits[i+2] <= 5) && (minbits[i+3] <= 5) &&   \
   (minbits[i+4] <= 5) && (minbits[i+5] <= 5))

#define	BIT6PACK(i,points_remaining)		   \
  (points_remaining >= 5 &&			   \
   (minbits[i] <= 6) && (minbits[i+1] <= 6) &&	   \
   (minbits[i+2] <= 6) && (minbits[i+3] <= 6) &&   \
   (minbits[i+4] <= 6))

#define	BYTEPACK(i,points_remaining)		 \
  (points_remaining >= 4 &&			 \
   (minbits[i] <= 8) && (minbits[i+1] <= 8) &&	 \
   (minbits[i+2] <= 8) && (minbits[i+3] <= 8))

#define	BIT10PACK(i,points_remaining)		   \
  (points_remaining >= 3 &&			   \
   (minbits[i] <= 10) && (minbits[i+1] <= 10) &&   \
   (minbits[i+2] <= 10))

#define	BIT15PACK(i,points_remaining)		\
  (points_remaining >= 2 &&			\
   (minbits[i] <= 15) && (minbits[i+1] <= 15))

#define	HALFPACK(i,points_remaining)					\
  (points_remaining >= 2 && (minbits[i] <= 16) && (minbits[i+1] <= 16))

#define	BIT30PACK(i,points_remaining)  \
  (points_remaining >= 1 &&	       \
   (minbits[i] <= 30))

#define	MINBITS(diff,minbits)					       \
  if (diff >= -8 && diff < 8) minbits = 4;			       \
  else if (diff >= -16 && diff < 16) minbits = 5;		       \
  else if (diff >= -32 && diff < 32) minbits = 6;		       \
  else if (diff >= -128 && diff < 128) minbits = 8;		       \
  else if (diff >= -512 && diff < 512) minbits = 10;		       \
  else if (diff >= -16384 && diff < 16384) minbits = 15;	       \
  else if (diff >= -32768 && diff < 32768) minbits = 16;	       \
  else if (diff >= -536870912 && diff < 536870912) minbits = 30;       \
  else minbits = 32;

#define PACK(bits,n,m1,m2)  {			\
    int i = 0;					\
    unsigned int val = 0;			\
    for (i=0;i<n;i++) {				\
      val = (val<<bits) | (diff[i]&m1); 	\
    }						\
    val |= ((unsigned int)m2 << 30);		\
    dframes->f[fn].w[wn].fw = val; }


/************************************************************************
 *  msr_pack_int_16:							*
 *	Pack integer data into INT_16 format.				*
 *	Return: 0 on success, -1 on failure.				*
 ************************************************************************/
int msr_pack_int_16
 (int16_t    *packed,           /* output data array - packed           */
  int32_t    *data,             /* input data array                     */
  int         ns,               /* desired number of samples to pack    */
  int         max_bytes,        /* max # of bytes for output buffer     */
  int         pad,              /* flag to specify padding to max_bytes */
  int        *pnbytes,          /* number of bytes actually packed      */
  int        *pnsamples,        /* number of samples actually packed    */
  int         swapflag)         /* if data should be swapped            */
{
  int points_remaining = ns;    /* number of samples remaining to pack  */
  int i = 0;
  
  while (points_remaining > 0 && max_bytes >= 2)
    {  /* Pack the next available data into INT_16 format */
      if ( data[i] < -32768 || data[i] > 32767 )
	ms_log (2, "msr_pack_int_16(%s): input sample out of range: %d\n",
		PACK_SRCNAME, data[i]);
      
      *packed = data[i];      
      if ( swapflag ) ms_gswap2 (packed);
      
      packed++;
      max_bytes -= 2;
      points_remaining--;
      i++;
    }
  
  *pnbytes = (ns - points_remaining) * 2;
  
  /* Pad miniSEED block if necessary */
  if (pad)
    {
      memset ((void *)packed, 0, max_bytes);
      *pnbytes += max_bytes;
    }

  *pnsamples = ns - points_remaining;

  return 0;
}


/************************************************************************
 *  msr_pack_int_32:							*
 *	Pack integer data into INT_32 format.				*
 *	Return: 0 on success, -1 on failure.				*
 ************************************************************************/
int msr_pack_int_32 
 (int32_t  *packed,          /* output data array - packed              */
  int32_t  *data,            /* input data array - unpacked             */
  int       ns,              /* desired number of samples to pack       */
  int       max_bytes,       /* max # of bytes for output buffer        */
  int       pad,             /* flag to specify padding to max_bytes    */
  int      *pnbytes,         /* number of bytes actually packed         */
  int      *pnsamples,       /* number of samples actually packed       */
  int       swapflag)        /* if data should be swapped               */
{
  int points_remaining = ns; /* number of samples remaining to pack */
  int i = 0;

  while (points_remaining > 0 && max_bytes >= 4)
    { /* Pack the next available data into INT_32 format */
      *packed = data[i];
      if ( swapflag ) ms_gswap4 (packed);
      
      packed++;
      max_bytes -= 4;
      points_remaining--;
      i++;
    }
  
  *pnbytes = (ns - points_remaining) * 4;
  
  /* Pad miniSEED block if necessary */
  if (pad)
    {
      memset ((void *)packed, 0, max_bytes);
      *pnbytes += max_bytes;
    }
  
  *pnsamples = ns - points_remaining;

  return 0;
}


/************************************************************************
 *  msr_pack_float_32:							*
 *	Pack float data into FLOAT32 format.				*
 *	Return: 0 on success, -1 on error.				*
 ************************************************************************/
int msr_pack_float_32 
 (float    *packed,          /* output data array - packed              */
  float    *data,            /* input data array - unpacked             */
  int       ns,              /* desired number of samples to pack       */
  int       max_bytes,       /* max # of bytes for output buffer        */
  int       pad,             /* flag to specify padding to max_bytes    */
  int      *pnbytes,         /* number of bytes actually packed         */
  int      *pnsamples,       /* number of samples actually packed       */
  int       swapflag)        /* if data should be swapped               */
{
  int points_remaining = ns; /* number of samples remaining to pack */
  int i = 0;
  
  while (points_remaining > 0 && max_bytes >= 4)
    {
      *packed = data[i];
      if ( swapflag ) ms_gswap4 (packed);
      
      packed++;
      max_bytes -= 4;
      points_remaining--;
      i++;
    }
  
  *pnbytes = (ns - points_remaining) * 4;
  
  /* Pad miniSEED block if necessary */
  if (pad)
    {
      memset ((void *)packed, 0, max_bytes);
      *pnbytes += max_bytes;
    }
  
  *pnsamples = ns - points_remaining;
  
  return 0;
}


/************************************************************************
 *  msr_pack_float_64:							*
 *	Pack double data into FLOAT64 format.				*
 *	Return: 0 on success, -1 on error.				*
 ************************************************************************/
int msr_pack_float_64 
 (double   *packed,          /* output data array - packed              */
  double   *data,            /* input data array - unpacked             */
  int       ns,              /* desired number of samples to pack       */
  int       max_bytes,       /* max # of bytes for output buffer        */
  int       pad,             /* flag to specify padding to max_bytes    */
  int      *pnbytes,         /* number of bytes actually packed         */
  int      *pnsamples,       /* number of samples actually packed       */
  int       swapflag)        /* if data should be swapped               */
{
  int points_remaining = ns; /* number of samples remaining to pack */
  int i = 0;
  
  while (points_remaining > 0 && max_bytes >= 8)
    {
      *packed = data[i];
      if ( swapflag ) ms_gswap8 (packed);
      
      packed++;
      max_bytes -= 8;
      points_remaining--;
      i++;
    }
  
  *pnbytes = (ns - points_remaining) * 8;
  
  /* Pad miniSEED block if necessary */
  if (pad)
    {
      memset ((void *)packed, 0, max_bytes);
      *pnbytes += max_bytes;
    }
  
  *pnsamples = ns - points_remaining;
  
  return 0;
}


/************************************************************************
 *  msr_pack_steim1:							*
 *	Pack data into STEIM1 data frames.				*
 *  return:								*
 *	0 on success.							*
 *	-1 on error.		           	         	        *
 ************************************************************************/
int msr_pack_steim1
 (DFRAMES      *dframes,       	/* ptr to data frames                   */
  int32_t      *data,		/* ptr to unpacked data array           */
  int32_t       d0,		/* first difference value               */
  int		ns,		/* number of samples to pack            */
  int		nf,		/* total number of data frames          */
  int		pad,		/* flag to specify padding to nf        */
  int	       *pnframes,	/* number of frames actually packed     */
  int	       *pnsamples,	/* number of samples actually packed    */
  int           swapflag)       /* if data should be swapped            */
{
  int		points_remaining = ns;
  int           points_packed = 0;
  int32_t       diff[4];        /* array of differences                 */
  uint8_t       minbits[4];     /* array of minimum bits for diffs      */
  int		i, j;
  int		mask;
  int		ipt = 0;	/* index of initial data to pack.	*/
  int		fn = 0;		/* index of initial frame to pack.	*/
  int		wn = 2;		/* index of initial word to pack.	*/
  int32_t      	itmp;
  int16_t	stmp;
  
  /* Calculate initial difference and minbits buffers */
  diff[0] = d0;
  MINBITS(diff[0],minbits[0]);
  for (i=1; i < 4 && i < ns; i++)
    {
      diff[i] = data[i] - data[i-1];
      MINBITS(diff[i],minbits[i]);
    }
  
  dframes->f[fn].ctrl = 0;
  
  /* Set X0 and XN values in first frame */
  X0 = data[0];
  if ( swapflag ) ms_gswap4 (&X0);
  dframes->f[0].ctrl = (dframes->f[0].ctrl<<2) | STEIM1_SPECIAL_MASK;
  XN = data[ns-1];
  if ( swapflag ) ms_gswap4 (&XN);
  dframes->f[0].ctrl = (dframes->f[0].ctrl<<2) | STEIM1_SPECIAL_MASK;
  
  while (points_remaining > 0)
    {
      points_packed = 0;
      
      /* Pack the next available data into the most compact form */
      if (BYTEPACK(0,points_remaining))
	{
	  mask = STEIM1_BYTE_MASK;
	  for (j=0; j<4; j++) { dframes->f[fn].w[wn].byte[j] = diff[j]; }
	  points_packed = 4;
	}
      else if (HALFPACK(0,points_remaining))
	{
	  mask = STEIM1_HALFWORD_MASK;
	  for (j=0; j<2; j++)
	    {
	      stmp = diff[j];
	      if ( swapflag ) ms_gswap2 (&stmp);
	      dframes->f[fn].w[wn].hw[j] = stmp;
	    }
	  points_packed = 2;
	}
      else
	{
	  mask = STEIM1_FULLWORD_MASK;
	  itmp = diff[0];
	  if ( swapflag ) ms_gswap4 (&itmp);
	  dframes->f[fn].w[wn].fw = itmp;
	  points_packed = 1;
	}
      
      /* Append mask for this word to current mask */
      dframes->f[fn].ctrl = (dframes->f[fn].ctrl<<2) | mask;
      
      points_remaining -= points_packed;
      ipt += points_packed;
      
      /* Check for full frame or full block */
      if (++wn >= VALS_PER_FRAME)
	{
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].ctrl);
	  /* Reset output index to beginning of frame */
	  wn = 0;
	  /* If block is full, output block and reinitialize */
	  if (++fn >= nf) break;
	  dframes->f[fn].ctrl = 0;
	}
      
      /* Shift and re-fill difference and minbits buffers */
      for ( i=points_packed; i < 4; i++ )
	{
	  /* Shift remaining buffer entries */
	  diff[i-points_packed] = diff[i];
	  minbits[i-points_packed] = minbits[i];
	}
      for ( i=4-points_packed,j=ipt+(4-points_packed); i < 4 && j < ns; i++,j++ )
	{
	  /* Re-fill entries */
	  diff[i] = data[j] - data[j-1];
	  MINBITS(diff[i],minbits[i]);
	}
    }
  
  /* Update XN value in first frame */
  XN = data[(ns-1)-points_remaining];
  if ( swapflag ) ms_gswap4 (&XN);
  
  /* End of data.  Pad current frame and optionally rest of block */
  /* Do not pad and output a completely empty block */
  if ( ! EMPTY_BLOCK(fn,wn) )
    {
      *pnframes = pad_steim_frame (dframes, fn, wn, nf, swapflag, pad);
    }
  else
    {
      *pnframes = 0;
    }
  
  *pnsamples = ns - points_remaining;
  
  return 0;
}


/************************************************************************
 *  msr_pack_steim2:							*
 *	Pack data into STEIM1 data frames.				*
 *  return:								*
 *	0 on success.							*
 *	-1 on error.                                                    *
 ************************************************************************/
int msr_pack_steim2
 (DFRAMES      *dframes,	/* ptr to data frames                   */
  int32_t      *data,		/* ptr to unpacked data array           */
  int32_t       d0,		/* first difference value               */
  int		ns,		/* number of samples to pack            */
  int		nf,		/* total number of data frames to pack  */
  int		pad,		/* flag to specify padding to nf        */
  int	       *pnframes,	/* number of frames actually packed     */
  int	       *pnsamples,	/* number of samples actually packed    */
  int           swapflag)       /* if data should be swapped            */
{
  int		points_remaining = ns;
  int           points_packed = 0;
  int32_t       diff[7];        /* array of differences                 */
  uint8_t       minbits[7];     /* array of minimum bits for diffs      */
  int		i, j;
  int		mask;
  int		ipt = 0;	/* index of initial data to pack.	*/
  int		fn = 0;		/* index of initial frame to pack.	*/
  int		wn = 2;		/* index of initial word to pack.	*/
  
  /* Calculate initial difference and minbits buffers */
  diff[0] = d0;
  MINBITS(diff[0],minbits[0]);
  for (i=1; i < 7 && i < ns; i++)
    {
      diff[i] = data[i] - data[i-1];
      MINBITS(diff[i],minbits[i]);
    }
  
  dframes->f[fn].ctrl = 0;
  
  /* Set X0 and XN values in first frame */
  X0 = data[0];
  if ( swapflag ) ms_gswap4 (&X0);
  dframes->f[0].ctrl = (dframes->f[0].ctrl<<2) | STEIM2_SPECIAL_MASK;
  XN = data[ns-1];
  if ( swapflag ) ms_gswap4 (&XN);
  dframes->f[0].ctrl = (dframes->f[0].ctrl<<2) | STEIM2_SPECIAL_MASK;
  
  while (points_remaining > 0)
    {
      points_packed = 0;
      
      /* Pack the next available datapoints into the most compact form */
      if (BIT4PACK(0,points_remaining))
	{
	  PACK(4,7,0x0000000f,02)
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].w[wn].fw);
	  mask = STEIM2_567_MASK;
	  points_packed = 7;
	}
      else if (BIT5PACK(0,points_remaining))
	{
	  PACK(5,6,0x0000001f,01)
	    if ( swapflag ) ms_gswap4 (&dframes->f[fn].w[wn].fw);
	  mask = STEIM2_567_MASK;
	  points_packed = 6;
	}
      else if (BIT6PACK(0,points_remaining))
	{
	  PACK(6,5,0x0000003f,00)
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].w[wn].fw);
	  mask = STEIM2_567_MASK;
	  points_packed = 5;
	}
      else if (BYTEPACK(0,points_remaining))
	{
	  mask = STEIM2_BYTE_MASK;
	  for (j=0; j<4; j++) dframes->f[fn].w[wn].byte[j] = diff[j];
	  points_packed = 4;
	}
      else if (BIT10PACK(0,points_remaining))
	{
	  PACK(10,3,0x000003ff,03)
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].w[wn].fw);
	  mask = STEIM2_123_MASK;
	  points_packed = 3;
	}
      else if (BIT15PACK(0,points_remaining))
	{
	  PACK(15,2,0x00007fff,02)
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].w[wn].fw);
	  mask = STEIM2_123_MASK;
	  points_packed = 2;
	}
      else if (BIT30PACK(0,points_remaining))
	{
	  PACK(30,1,0x3fffffff,01)
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].w[wn].fw);
	  mask = STEIM2_123_MASK;
	  points_packed = 1;
	}
      else
	{
	  ms_log (2, "msr_pack_steim2(%s): Unable to represent difference in <= 30 bits\n",
		  PACK_SRCNAME);
	  return -1;
	}
      
      /* Append mask for this word to current mask */
      dframes->f[fn].ctrl = (dframes->f[fn].ctrl<<2) | mask;
      
      points_remaining -= points_packed;
      ipt += points_packed;
      
      /* Check for full frame or full block */
      if (++wn >= VALS_PER_FRAME)
	{
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].ctrl);
	  /* Reset output index to beginning of frame */
	  wn = 0;
	  /* If block is full, output block and reinitialize */
	  if (++fn >= nf) break;
	  dframes->f[fn].ctrl = 0;
	}
      
      /* Shift and re-fill difference and minbits buffers */
      for ( i=points_packed; i < 7; i++ )
	{
	  /* Shift remaining buffer entries */
	  diff[i-points_packed] = diff[i];
	  minbits[i-points_packed] = minbits[i];
	}
      for ( i=7-points_packed,j=ipt+(7-points_packed); i < 7 && j < ns; i++,j++ )
	{
	  /* Re-fill entries */
	  diff[i] = data[j] - data[j-1];
	  MINBITS(diff[i],minbits[i]);
	}
    }
  
  /* Update XN value in first frame */
  XN = data[(ns-1)-points_remaining];
  if ( swapflag ) ms_gswap4 (&XN);
  
  /* End of data.  Pad current frame and optionally rest of block */
  /* Do not pad and output a completely empty block */
  if ( ! EMPTY_BLOCK(fn,wn) )
    {
      *pnframes = pad_steim_frame (dframes, fn, wn, nf, swapflag, pad);
    }
  else
    {
      *pnframes = 0;
    }
  
  *pnsamples = ns - points_remaining;
  
  return 0;
}


/************************************************************************
 *  pad_steim_frame:							*
 *	Pad the rest of the data record with null values,		*
 *	and optionally the rest of the total number of frames.		*
 *  return:								*
 *	total number of frames in record.				*
 ************************************************************************/
static int pad_steim_frame
 (DFRAMES      *dframes,
  int		fn,	    	/* current frame number.		*/
  int	    	wn,		/* current work number.			*/
  int	    	nf,		/* total number of data frames.		*/
  int		swapflag,	/* flag to swap byte order of data.	*/
  int	    	pad)		/* flag to pad # frames to nf.		*/
{
  /* Finish off the current frame */
  if (wn < VALS_PER_FRAME && fn < nf)
    {
      for (; wn < VALS_PER_FRAME; wn++)
	{
	  dframes->f[fn].w[wn].fw = 0;
	  dframes->f[fn].ctrl = (dframes->f[fn].ctrl<<2) | STEIM1_SPECIAL_MASK;
	}
      if ( swapflag ) ms_gswap4 (&dframes->f[fn].ctrl);
      fn++;
    }
  
  /* Fill the remaining frames in the block */
  if (pad)
    {
      for (; fn<nf; fn++)
	{
	  dframes->f[fn].ctrl = STEIM1_SPECIAL_MASK;	/* mask for ctrl */
	  for (wn=0; wn<VALS_PER_FRAME; wn++)
	    {
	      dframes->f[fn].w[wn].fw = 0;
	      dframes->f[fn].ctrl = (dframes->f[fn].ctrl<<2) | STEIM1_SPECIAL_MASK;
	    }
	  
	  if ( swapflag ) ms_gswap4 (&dframes->f[fn].ctrl);
	}
    }
  
  return fn;
}


/************************************************************************
 *  msr_pack_text:						       	*
 *	Pack text data into text format.  Split input data on line	*
*	breaks so as to not split lines between records.		* 
*	Return: 0 on success, -1 on error.				*
 ************************************************************************/
int msr_pack_text
 (char 	       *packed,         /* output data array - packed.		*/
  char	       *data,		/* input data array - unpacked.		*/
  int		ns,		/* desired number of samples to pack.	*/
  int		max_bytes,	/* max # of bytes for output buffer.	*/
  int		pad,		/* flag to specify padding to max_bytes.*/
  int	       *pnbytes,	/* number of bytes actually packed.	*/
  int	       *pnsamples)	/* number of samples actually packed.	*/
{
  int points_remaining = ns;	/* number of samples remaining to pack.	*/
  int		last = -1;
  int		nbytes;
  int		i;
  
  /* Split lines only if a single line will not fit in 1 record */
  if (points_remaining > max_bytes)
    {
      /* Look for the last newline that will fit in output buffer */
      for (i=max_bytes-1; i>=0; i--)
	{
	  if (data[i] == '\n') {
	    last = i;
	    break;
	  }
	}
      if (last < 0) last = max_bytes - 1;
    }
  
  if (last < 0) last = points_remaining - 1;
  nbytes = last + 1;
  memcpy (packed, data, nbytes);
  packed += nbytes;
  max_bytes -= nbytes;
  *pnbytes = nbytes;
  *pnsamples = nbytes;
  points_remaining -= nbytes;
  
  /* Pad miniSEED block if necessary */
  if (pad)
    {
      memset ((void *)packed, 0, max_bytes);
      *pnbytes += max_bytes;
    }
  
  *pnsamples = ns - points_remaining;
  
  return 0;
}
/***************************************************************************
 *
 * Routines to parse Mini-SEED.
 *
 * Written by Chad Trabant
 *   IRIS Data Management Center
 *
 * modified: 2012.105
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#include "libmseed.h"


/**********************************************************************
 * msr_parse:
 *
 * This routine will attempt to parse (detect and unpack) a Mini-SEED
 * record from a specified memory buffer and populate a supplied
 * MSRecord structure.
 *
 * If reclen is less than or equal to 0 the length of record is
 * automatically detected otherwise reclen should be the correct
 * record length.
 *
 * For auto detection of record length the record should include a
 * 1000 blockette or be followed by another record header in the
 * buffer.
 *
 * dataflag will be passed directly to msr_unpack().
 *
 * Return values:
 *   0 : Success, populates the supplied MSRecord.
 *  >0 : Data record detected but not enough data is present, the
 *       return value is a hint of how many more bytes are needed.
 *  <0 : libmseed error code (listed in libmseed.h) is returned.
 *********************************************************************/
int
msr_parse ( char *record, int recbuflen, MSRecord **ppmsr, int reclen,
	    flag dataflag, flag verbose )
{
  int detlen = 0;
  int retcode = 0;
  
  if ( ! ppmsr )
    return MS_GENERROR;
  
  if ( ! record )
    return MS_GENERROR;
  
  /* Sanity check: record length cannot be larger than buffer */
  if ( reclen > 0 && reclen > recbuflen )
    {
      ms_log (2, "ms_parse() Record length (%d) cannot be larger than buffer (%d)\n",
	      reclen, recbuflen);
      return MS_GENERROR;
    }
  
  /* Autodetect the record length */
  if ( reclen <= 0 )
    {
      detlen = ms_detect (record, recbuflen);
      
      /* No data record detected */
      if ( detlen < 0 )
	{
	  return MS_NOTSEED;
	}
      
      /* Found record but could not determine length */
      if ( detlen == 0 )
	{
	  return 256;
	}
      
      if ( verbose > 2 )
	{
	  ms_log (1, "Detected record length of %d bytes\n", detlen);
	}
      
      reclen = detlen;
    }
  
  /* Check that record length is in supported range */
  if ( reclen < MINRECLEN || reclen > MAXRECLEN )
    {
      ms_log (2, "Record length is out of range: %d (allowed: %d to %d)\n",
	      reclen, MINRECLEN, MAXRECLEN);
      
      return MS_OUTOFRANGE;
    }
  
  /* Check if more data is required, return hint */
  if ( reclen > recbuflen )
    {
      if ( verbose > 2 )
	ms_log (1, "Detected %d byte record, need %d more bytes\n",
		reclen, (reclen - recbuflen));
      
      return (reclen - recbuflen);
    }
  
  /* Unpack record */
  if ( (retcode = msr_unpack (record, reclen, ppmsr, dataflag, verbose)) != MS_NOERROR )
    {
      msr_free (ppmsr);
      
      return retcode;
    }
  
  return MS_NOERROR;
}  /* End of msr_parse() */


/********************************************************************
 * ms_detect:
 *
 * Determine SEED data record length with the following steps:
 *
 * 1) determine that the buffer contains a SEED data record by
 * verifying known signatures (fields with known limited values)
 *
 * 2) search the record up to recbuflen bytes for a 1000 blockette.
 *
 * 3) If no blockette 1000 is found search at 256-byte offsets for the
 * fixed section of the next header or blank/noise record, thereby
 * implying the record length.
 *
 * Returns:
 * -1 : data record not detected or error
 *  0 : data record detected but could not determine length
 * >0 : size of the record in bytes
 *********************************************************************/
int
ms_detect ( const char *record, int recbuflen )
{
  uint16_t blkt_offset;    /* Byte offset for next blockette */
  uint8_t swapflag  = 0;   /* Byte swapping flag */
  uint8_t foundlen = 0;    /* Found record length */
  int32_t reclen = -1;     /* Size of record in bytes */
  
  uint16_t blkt_type;
  uint16_t next_blkt;
  
  struct fsdh_s *fsdh;
  struct blkt_1000_s *blkt_1000;
  const char *nextfsdh;
  
  /* Buffer must be at least 48 bytes (the fixed section) */
  if ( recbuflen < 48 )
    return -1;

  /* Check for valid fixed section of header */
  if ( ! MS_ISVALIDHEADER(record) )
    return -1;
  
  fsdh = (struct fsdh_s *) record;
  
  /* Check to see if byte swapping is needed by checking for sane year */
  if ( (fsdh->start_time.year < 1900) ||
       (fsdh->start_time.year > 2050) )
    swapflag = 1;
  
  blkt_offset = fsdh->blockette_offset;
  
  /* Swap order of blkt_offset if needed */
  if ( swapflag ) ms_gswap2 (&blkt_offset);
  
  /* Loop through blockettes as long as number is non-zero and viable */
  while ( blkt_offset != 0 &&
	  blkt_offset <= recbuflen )
    {
      memcpy (&blkt_type, record + blkt_offset, 2);
      memcpy (&next_blkt, record + blkt_offset + 2, 2);
      
      if ( swapflag )
	{
	  ms_gswap2 (&blkt_type);
	  ms_gswap2 (&next_blkt);
	}
      
      /* Found a 1000 blockette, not truncated */
      if ( blkt_type == 1000  &&
	   (int)(blkt_offset + 4 + sizeof(struct blkt_1000_s)) <= recbuflen )
	{
          blkt_1000 = (struct blkt_1000_s *) (record + blkt_offset + 4);
	  
          foundlen = 1;
	  
          /* Calculate record size in bytes as 2^(blkt_1000->reclen) */
	  reclen = (unsigned int) 1 << blkt_1000->reclen;
	  
	  break;
        }
      
      /* Saftey check for invalid offset */
      if ( next_blkt != 0 && next_blkt < blkt_offset )
	{
	  ms_log (2, "Invalid blockette offset (%d) less than current offset (%d)\n",
		  next_blkt, blkt_offset);
	  return -1;
	}
      
      blkt_offset = next_blkt;
    }
  
  /* If record length was not determined by a 1000 blockette scan the buffer
   * and search for the next record */
  if ( reclen == -1 )
    {
      nextfsdh = record + 256;
      
      /* Check for record header or blank/noise record at 256 byte offsets */
      while ( ((nextfsdh - record) + 48) < recbuflen )
	{
	  if ( MS_ISVALIDHEADER(nextfsdh) || MS_ISVALIDBLANK(nextfsdh) )
            {
	      foundlen = 1;
	      reclen = nextfsdh - record;
	      break;
	    }
	  
	  nextfsdh += 256;
	}
    }
  
  if ( ! foundlen )
    return 0;
  else
    return reclen;
}  /* End of ms_detect() */


/***************************************************************************
 * ms_parse_raw:
 *
 * Parse and verify a SEED data record header (fixed section and
 * blockettes) at the lowest level, printing error messages for
 * invalid header values and optionally print raw header values.  The
 * memory at 'record' is assumed to be a Mini-SEED record.  Not every
 * possible test is performed, common errors and those causing
 * libmseed parsing to fail should be detected.
 *
 * The 'details' argument is interpreted as follows:
 *
 * details:
 *  0 = only print error messages for invalid header fields
 *  1 = print basic fields in addition to invalid field errors
 *  2 = print all fields in addition to invalid field errors
 *
 * The 'swapflag' argument is interpreted as follows:
 *
 * swapflag:
 *  1 = swap multibyte quantities
 *  0 = do no swapping
 * -1 = autodetect byte order using year test, swap if needed
 *
 * Any byte swapping performed by this routine is applied directly to
 * the memory reference by the record pointer.
 *
 * This routine is primarily intended to diagnose invalid Mini-SEED headers.
 *
 * Returns 0 when no errors were detected or a positive count of
 * errors detected.
 ***************************************************************************/
int
ms_parse_raw ( char *record, int maxreclen, flag details, flag swapflag )
{
  struct fsdh_s *fsdh;
  double nomsamprate;
  char srcname[50];
  char *X;
  char b;
  int retval = 0;
  int b1000encoding = -1;
  int b1000reclen = -1;
  int endofblockettes = -1;
  int idx;
  
  if ( ! record )
    return 1;
  
  /* Generate a source name string */
  srcname[0] = '\0';
  ms_recsrcname (record, srcname, 1);
  
  fsdh = (struct fsdh_s *) record;
  
  /* Check to see if byte swapping is needed by testing the year */
  if ( swapflag == -1 &&
       ((fsdh->start_time.year < 1900) ||
	(fsdh->start_time.year > 2050)) )
    swapflag = 1;
  else
    swapflag = 0;
  
  if ( details > 1 )
    {
      if ( swapflag == 1 )
	ms_log (0, "Swapping multi-byte quantities in header\n");
      else
	ms_log (0, "Not swapping multi-byte quantities in header\n");
    }
  
  /* Swap byte order */
  if ( swapflag )
    {
      MS_SWAPBTIME (&fsdh->start_time);
      ms_gswap2a (&fsdh->numsamples);
      ms_gswap2a (&fsdh->samprate_fact);
      ms_gswap2a (&fsdh->samprate_mult);
      ms_gswap4a (&fsdh->time_correct);
      ms_gswap2a (&fsdh->data_offset);
      ms_gswap2a (&fsdh->blockette_offset);
    }
  
  /* Validate fixed section header fields */
  X = record;  /* Pointer of convenience */
  
  /* Check record sequence number, 6 ASCII digits */
  if ( ! isdigit((unsigned char) *(X)) || ! isdigit ((unsigned char) *(X+1)) ||
       ! isdigit((unsigned char) *(X+2)) || ! isdigit ((unsigned char) *(X+3)) ||
       ! isdigit((unsigned char) *(X+4)) || ! isdigit ((unsigned char) *(X+5)) )
    {
      ms_log (2, "%s: Invalid sequence number: '%c%c%c%c%c%c'\n", srcname, X, X+1, X+2, X+3, X+4, X+5);
      retval++;
    }
  
  /* Check header/quality indicator */
  if ( ! MS_ISDATAINDICATOR(*(X+6)) )
    {
      ms_log (2, "%s: Invalid header indicator (DRQM): '%c'\n", srcname, X+6);
      retval++;
    }
  
  /* Check reserved byte, space or NULL */
  if ( ! (*(X+7) == ' ' || *(X+7) == '\0') )
    {
      ms_log (2, "%s: Invalid fixed section reserved byte (Space): '%c'\n", srcname, X+7);
      retval++;
    }
  
  /* Check station code, 5 alphanumerics or spaces */
  if ( ! (isalnum((unsigned char) *(X+8)) || *(X+8) == ' ') ||
       ! (isalnum((unsigned char) *(X+9)) || *(X+9) == ' ') ||
       ! (isalnum((unsigned char) *(X+10)) || *(X+10) == ' ') ||
       ! (isalnum((unsigned char) *(X+11)) || *(X+11) == ' ') ||
       ! (isalnum((unsigned char) *(X+12)) || *(X+12) == ' ') )
    {
      ms_log (2, "%s: Invalid station code: '%c%c%c%c%c'\n", srcname, X+8, X+9, X+10, X+11, X+12);
      retval++;
    }
  
  /* Check location ID, 2 alphanumerics or spaces */
  if ( ! (isalnum((unsigned char) *(X+13)) || *(X+13) == ' ') ||
       ! (isalnum((unsigned char) *(X+14)) || *(X+14) == ' ') )
    {
      ms_log (2, "%s: Invalid location ID: '%c%c'\n", srcname, X+13, X+14);
      retval++;
    }
  
  /* Check channel codes, 3 alphanumerics or spaces */
  if ( ! (isalnum((unsigned char) *(X+15)) || *(X+15) == ' ') ||
       ! (isalnum((unsigned char) *(X+16)) || *(X+16) == ' ') ||
       ! (isalnum((unsigned char) *(X+17)) || *(X+17) == ' ') )
    {
      ms_log (2, "%s: Invalid channel codes: '%c%c%c'\n", srcname, X+15, X+16, X+17);
      retval++;
    }
  
  /* Check network code, 2 alphanumerics or spaces */
  if ( ! (isalnum((unsigned char) *(X+18)) || *(X+18) == ' ') ||
       ! (isalnum((unsigned char) *(X+19)) || *(X+19) == ' ') )
    {
      ms_log (2, "%s: Invalid network code: '%c%c'\n", srcname, X+18, X+19);
      retval++;
    }
  
  /* Check start time fields */
  if ( fsdh->start_time.year < 1920 || fsdh->start_time.year > 2050 )
    {
      ms_log (2, "%s: Unlikely start year (1920-2050): '%d'\n", srcname, fsdh->start_time.year);
      retval++;
    }
  if ( fsdh->start_time.day < 1 || fsdh->start_time.day > 366 )
    {
      ms_log (2, "%s: Invalid start day (1-366): '%d'\n", srcname, fsdh->start_time.day);
      retval++;
    }
  if ( fsdh->start_time.hour > 23 )
    {
      ms_log (2, "%s: Invalid start hour (0-23): '%d'\n", srcname, fsdh->start_time.hour);
      retval++;
    }
  if ( fsdh->start_time.min > 59 )
    {
      ms_log (2, "%s: Invalid start minute (0-59): '%d'\n", srcname, fsdh->start_time.min);
      retval++;
    }
  if ( fsdh->start_time.sec > 60 )
    {
      ms_log (2, "%s: Invalid start second (0-60): '%d'\n", srcname, fsdh->start_time.sec);
      retval++;
    }
  if ( fsdh->start_time.fract > 9999 )
    {
      ms_log (2, "%s: Invalid start fractional seconds (0-9999): '%d'\n", srcname, fsdh->start_time.fract);
      retval++;
    }
  
  /* Check number of samples, max samples in 4096-byte Steim-2 encoded record: 6601 */
  if ( fsdh->numsamples > 20000 )
    {
      ms_log (2, "%s: Unlikely number of samples (>20000): '%d'\n", srcname, fsdh->numsamples);
      retval++;
    }
  
  /* Sanity check that there is space for blockettes when both data and blockettes are present */
  if ( fsdh->numsamples > 0 && fsdh->numblockettes > 0 && fsdh->data_offset <= fsdh->blockette_offset )
    {
      ms_log (2, "%s: No space for %d blockettes, data offset: %d, blockette offset: %d\n", srcname,
	      fsdh->numblockettes, fsdh->data_offset, fsdh->blockette_offset);
      retval++;
    }
  
  
  /* Print raw header details */
  if ( details >= 1 )
    {
      /* Determine nominal sample rate */
      nomsamprate = ms_nomsamprate (fsdh->samprate_fact, fsdh->samprate_mult);
  
      /* Print header values */
      ms_log (0, "RECORD -- %s\n", srcname);
      ms_log (0, "        sequence number: '%c%c%c%c%c%c'\n", fsdh->sequence_number[0], fsdh->sequence_number[1], fsdh->sequence_number[2],
	      fsdh->sequence_number[3], fsdh->sequence_number[4], fsdh->sequence_number[5]);
      ms_log (0, " data quality indicator: '%c'\n", fsdh->dataquality);
      if ( details > 0 )
        ms_log (0, "               reserved: '%c'\n", fsdh->reserved);
      ms_log (0, "           station code: '%c%c%c%c%c'\n", fsdh->station[0], fsdh->station[1], fsdh->station[2], fsdh->station[3], fsdh->station[4]);
      ms_log (0, "            location ID: '%c%c'\n", fsdh->location[0], fsdh->location[1]);
      ms_log (0, "          channel codes: '%c%c%c'\n", fsdh->channel[0], fsdh->channel[1], fsdh->channel[2]);
      ms_log (0, "           network code: '%c%c'\n", fsdh->network[0], fsdh->network[1]);
      ms_log (0, "             start time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", fsdh->start_time.year, fsdh->start_time.day,
	      fsdh->start_time.hour, fsdh->start_time.min, fsdh->start_time.sec, fsdh->start_time.fract, fsdh->start_time.unused);
      ms_log (0, "      number of samples: %d\n", fsdh->numsamples);
      ms_log (0, "     sample rate factor: %d  (%.10g samples per second)\n",
              fsdh->samprate_fact, nomsamprate);
      ms_log (0, " sample rate multiplier: %d\n", fsdh->samprate_mult);
      
      /* Print flag details if requested */
      if ( details > 1 )
        {
          /* Activity flags */
	  b = fsdh->act_flags;
	  ms_log (0, "         activity flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] Calibration signals present\n");
	  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Time correction applied\n");
	  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Beginning of an event, station trigger\n");
	  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] End of an event, station detrigger\n");
	  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] A positive leap second happened in this record\n");
	  if ( b & 0x20 ) ms_log (0, "                         [Bit 5] A negative leap second happened in this record\n");
	  if ( b & 0x40 ) ms_log (0, "                         [Bit 6] Event in progress\n");
	  if ( b & 0x80 ) ms_log (0, "                         [Bit 7] Undefined bit set\n");
	  
	  /* I/O and clock flags */
	  b = fsdh->io_flags;
	  ms_log (0, "    I/O and clock flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] Station volume parity error possibly present\n");
	  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Long record read (possibly no problem)\n");
	  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Short record read (record padded)\n");
	  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Start of time series\n");
	  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] End of time series\n");
	  if ( b & 0x20 ) ms_log (0, "                         [Bit 5] Clock locked\n");
	  if ( b & 0x40 ) ms_log (0, "                         [Bit 6] Undefined bit set\n");
	  if ( b & 0x80 ) ms_log (0, "                         [Bit 7] Undefined bit set\n");
	  
	  /* Data quality flags */
	  b = fsdh->dq_flags;
	  ms_log (0, "     data quality flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
		  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
		  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
	  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] Amplifier saturation detected\n");
	  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Digitizer clipping detected\n");
	  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Spikes detected\n");
	  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Glitches detected\n");
	  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] Missing/padded data present\n");
	  if ( b & 0x20 ) ms_log (0, "                         [Bit 5] Telemetry synchronization error\n");
	  if ( b & 0x40 ) ms_log (0, "                         [Bit 6] A digital filter may be charging\n");
	  if ( b & 0x80 ) ms_log (0, "                         [Bit 7] Time tag is questionable\n");
	}
      
      ms_log (0, "   number of blockettes: %d\n", fsdh->numblockettes);
      ms_log (0, "        time correction: %ld\n", (long int) fsdh->time_correct);
      ms_log (0, "            data offset: %d\n", fsdh->data_offset);
      ms_log (0, " first blockette offset: %d\n", fsdh->blockette_offset);
    } /* Done printing raw header details */
  
  
  /* Validate and report information in the blockette chain */
  if ( fsdh->blockette_offset > 46 && fsdh->blockette_offset < maxreclen )
    {
      int blkt_offset = fsdh->blockette_offset;
      int blkt_count = 0;
      int blkt_length;
      uint16_t blkt_type;
      uint16_t next_blkt;
      char *blkt_desc;
      
      /* Traverse blockette chain */
      while ( blkt_offset != 0 && blkt_offset < maxreclen )
	{
	  /* Every blockette has a similar 4 byte header: type and next */
	  memcpy (&blkt_type, record + blkt_offset, 2);
	  memcpy (&next_blkt, record + blkt_offset+2, 2);
	  
	  if ( swapflag )
	    {
	      ms_gswap2 (&blkt_type);
	      ms_gswap2 (&next_blkt);
	    }
	  
	  /* Print common header fields */
	  if ( details >= 1 )
	    {
	      blkt_desc =  ms_blktdesc(blkt_type);
	      ms_log (0, "          BLOCKETTE %u: (%s)\n", blkt_type, (blkt_desc) ? blkt_desc : "Unknown");
	      ms_log (0, "              next blockette: %u\n", next_blkt);
	    }
	  
	  blkt_length = ms_blktlen (blkt_type, record + blkt_offset, swapflag);
	  if ( blkt_length == 0 )
	    {
	      ms_log (2, "%s: Unknown blockette length for type %d\n", srcname, blkt_type);
	      retval++;
	    }
	  
	  /* Track end of blockette chain */
	  endofblockettes = blkt_offset + blkt_length - 1;
	  
	  /* Sanity check that the blockette is contained in the record */
	  if ( endofblockettes > maxreclen )
	    {
	      ms_log (2, "%s: Blockette type %d at offset %d with length %d does not fix in record (%d)\n",
		      srcname, blkt_type, blkt_offset, blkt_length, maxreclen);
	      retval++;
	      break;
	    }
	  
	  if ( blkt_type == 100 )
	    {
	      struct blkt_100_s *blkt_100 = (struct blkt_100_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		ms_gswap4 (&blkt_100->samprate);
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "          actual sample rate: %.10g\n", blkt_100->samprate);
		  
		  if ( details > 1 )
		    {
		      b = blkt_100->flags;
		      ms_log (0, "             undefined flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		      
		      ms_log (0, "          reserved bytes (3): %u,%u,%u\n",
			      blkt_100->reserved[0], blkt_100->reserved[1], blkt_100->reserved[2]);
		    }
		}
	    }
	  
	  else if ( blkt_type == 200 )
	    {
	      struct blkt_200_s *blkt_200 = (struct blkt_200_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  ms_gswap4 (&blkt_200->amplitude);
		  ms_gswap4 (&blkt_200->period);
		  ms_gswap4 (&blkt_200->background_estimate);
		  MS_SWAPBTIME (&blkt_200->time);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "            signal amplitude: %g\n", blkt_200->amplitude);
		  ms_log (0, "               signal period: %g\n", blkt_200->period);
		  ms_log (0, "         background estimate: %g\n", blkt_200->background_estimate);
		  
		  if ( details > 1 )
		    {
		      b = blkt_200->flags;
		      ms_log (0, "       event detection flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			      bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			      bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		      if ( b & 0x01 ) ms_log (0, "                         [Bit 0] 1: Dilatation wave\n");
		      else            ms_log (0, "                         [Bit 0] 0: Compression wave\n");
		      if ( b & 0x02 ) ms_log (0, "                         [Bit 1] 1: Units after deconvolution\n");
		      else            ms_log (0, "                         [Bit 1] 0: Units are digital counts\n");
		      if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Bit 0 is undetermined\n");
		      ms_log (0, "               reserved byte: %u\n", blkt_200->reserved);
		    }
		  
		  ms_log (0, "           signal onset time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_200->time.year, blkt_200->time.day,
			  blkt_200->time.hour, blkt_200->time.min, blkt_200->time.sec, blkt_200->time.fract, blkt_200->time.unused);
		  ms_log (0, "               detector name: %.24s\n", blkt_200->detector);
		}
	    }
	  
	  else if ( blkt_type == 201 )
	    {
	      struct blkt_201_s *blkt_201 = (struct blkt_201_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  ms_gswap4 (&blkt_201->amplitude);
		  ms_gswap4 (&blkt_201->period);
		  ms_gswap4 (&blkt_201->background_estimate);
		  MS_SWAPBTIME (&blkt_201->time);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "            signal amplitude: %g\n", blkt_201->amplitude);
		  ms_log (0, "               signal period: %g\n", blkt_201->period);
		  ms_log (0, "         background estimate: %g\n", blkt_201->background_estimate);
		  
		  b = blkt_201->flags;
		  ms_log (0, "       event detection flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] 1: Dilation wave\n");
		  else            ms_log (0, "                         [Bit 0] 0: Compression wave\n");
		  
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_201->reserved);
		  ms_log (0, "           signal onset time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_201->time.year, blkt_201->time.day,
			  blkt_201->time.hour, blkt_201->time.min, blkt_201->time.sec, blkt_201->time.fract, blkt_201->time.unused);
		  ms_log (0, "                  SNR values: ");
		  for (idx=0; idx < 6; idx++) ms_log (0, "%u  ", blkt_201->snr_values[idx]);
		  ms_log (0, "\n");
		  ms_log (0, "              loopback value: %u\n", blkt_201->loopback);
		  ms_log (0, "              pick algorithm: %u\n", blkt_201->pick_algorithm);
		  ms_log (0, "               detector name: %.24s\n", blkt_201->detector);
		}
	    }
	  
	  else if ( blkt_type == 300 )
	    {
	      struct blkt_300_s *blkt_300 = (struct blkt_300_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  MS_SWAPBTIME (&blkt_300->time);
		  ms_gswap4 (&blkt_300->step_duration);
		  ms_gswap4 (&blkt_300->interval_duration);
		  ms_gswap4 (&blkt_300->amplitude);
		  ms_gswap4 (&blkt_300->reference_amplitude);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "      calibration start time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_300->time.year, blkt_300->time.day,
			  blkt_300->time.hour, blkt_300->time.min, blkt_300->time.sec, blkt_300->time.fract, blkt_300->time.unused);
		  ms_log (0, "      number of calibrations: %u\n", blkt_300->numcalibrations);
		  
		  b = blkt_300->flags;
		  ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  if ( b & 0x01 ) ms_log (0, "                         [Bit 0] First pulse is positive\n");
		  if ( b & 0x02 ) ms_log (0, "                         [Bit 1] Calibration's alternate sign\n");
		  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
		  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
		  
		  ms_log (0, "               step duration: %u\n", blkt_300->step_duration);
		  ms_log (0, "           interval duration: %u\n", blkt_300->interval_duration);
		  ms_log (0, "            signal amplitude: %g\n", blkt_300->amplitude);
		  ms_log (0, "        input signal channel: %.3s", blkt_300->input_channel);
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_300->reserved);
		  ms_log (0, "         reference amplitude: %u\n", blkt_300->reference_amplitude);
		  ms_log (0, "                    coupling: %.12s\n", blkt_300->coupling);
		  ms_log (0, "                     rolloff: %.12s\n", blkt_300->rolloff);
		}
	    }
	  
	  else if ( blkt_type == 310 )
	    {
	      struct blkt_310_s *blkt_310 = (struct blkt_310_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  MS_SWAPBTIME (&blkt_310->time);
		  ms_gswap4 (&blkt_310->duration);
		  ms_gswap4 (&blkt_310->period);
		  ms_gswap4 (&blkt_310->amplitude);
		  ms_gswap4 (&blkt_310->reference_amplitude);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "      calibration start time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_310->time.year, blkt_310->time.day,
			  blkt_310->time.hour, blkt_310->time.min, blkt_310->time.sec, blkt_310->time.fract, blkt_310->time.unused);
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_310->reserved1);
		  
		  b = blkt_310->flags;
		  ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
		  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
		  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] Peak-to-peak amplitude\n");
		  if ( b & 0x20 ) ms_log (0, "                         [Bit 5] Zero-to-peak amplitude\n");
		  if ( b & 0x40 ) ms_log (0, "                         [Bit 6] RMS amplitude\n");
		  
		  ms_log (0, "        calibration duration: %u\n", blkt_310->duration);
		  ms_log (0, "               signal period: %g\n", blkt_310->period);
		  ms_log (0, "            signal amplitude: %g\n", blkt_310->amplitude);
		  ms_log (0, "        input signal channel: %.3s", blkt_310->input_channel);
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_310->reserved2);	      
		  ms_log (0, "         reference amplitude: %u\n", blkt_310->reference_amplitude);
		  ms_log (0, "                    coupling: %.12s\n", blkt_310->coupling);
		  ms_log (0, "                     rolloff: %.12s\n", blkt_310->rolloff);
		}
	    }
	  
	  else if ( blkt_type == 320 )
	    {
	      struct blkt_320_s *blkt_320 = (struct blkt_320_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  MS_SWAPBTIME (&blkt_320->time);
		  ms_gswap4 (&blkt_320->duration);
		  ms_gswap4 (&blkt_320->ptp_amplitude);
		  ms_gswap4 (&blkt_320->reference_amplitude);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "      calibration start time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_320->time.year, blkt_320->time.day,
			  blkt_320->time.hour, blkt_320->time.min, blkt_320->time.sec, blkt_320->time.fract, blkt_320->time.unused);
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_320->reserved1);
		  
		  b = blkt_320->flags;
		  ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
		  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
		  if ( b & 0x10 ) ms_log (0, "                         [Bit 4] Random amplitudes\n");
		  
		  ms_log (0, "        calibration duration: %u\n", blkt_320->duration);
		  ms_log (0, "      peak-to-peak amplitude: %g\n", blkt_320->ptp_amplitude);
		  ms_log (0, "        input signal channel: %.3s", blkt_320->input_channel);
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_320->reserved2);
		  ms_log (0, "         reference amplitude: %u\n", blkt_320->reference_amplitude);
		  ms_log (0, "                    coupling: %.12s\n", blkt_320->coupling);
		  ms_log (0, "                     rolloff: %.12s\n", blkt_320->rolloff);
		  ms_log (0, "                  noise type: %.8s\n", blkt_320->noise_type);
		}
	    }
	  
	  else if ( blkt_type == 390 )
	    {
	      struct blkt_390_s *blkt_390 = (struct blkt_390_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  MS_SWAPBTIME (&blkt_390->time);
		  ms_gswap4 (&blkt_390->duration);
		  ms_gswap4 (&blkt_390->amplitude);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "      calibration start time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_390->time.year, blkt_390->time.day,
			  blkt_390->time.hour, blkt_390->time.min, blkt_390->time.sec, blkt_390->time.fract, blkt_390->time.unused);
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_390->reserved1);
		  
		  b = blkt_390->flags;
		  ms_log (0, "           calibration flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  if ( b & 0x04 ) ms_log (0, "                         [Bit 2] Calibration was automatic\n");
		  if ( b & 0x08 ) ms_log (0, "                         [Bit 3] Calibration continued from previous record(s)\n");
		  
		  ms_log (0, "        calibration duration: %u\n", blkt_390->duration);
		  ms_log (0, "            signal amplitude: %g\n", blkt_390->amplitude);
		  ms_log (0, "        input signal channel: %.3s", blkt_390->input_channel);
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_390->reserved2);
		}
	    }

	  else if ( blkt_type == 395 )
	    {
	      struct blkt_395_s *blkt_395 = (struct blkt_395_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		MS_SWAPBTIME (&blkt_395->time);
	      
	      if ( details >= 1 )
		{ 
		  ms_log (0, "        calibration end time: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_395->time.year, blkt_395->time.day,
			  blkt_395->time.hour, blkt_395->time.min, blkt_395->time.sec, blkt_395->time.fract, blkt_395->time.unused);
		  if ( details > 1 )
		    ms_log (0, "          reserved bytes (2): %u,%u\n",
			    blkt_395->reserved[0], blkt_395->reserved[1]);
		}
	    }
	  
	  else if ( blkt_type == 400 )
	    {
	      struct blkt_400_s *blkt_400 = (struct blkt_400_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  ms_gswap4 (&blkt_400->azimuth);
		  ms_gswap4 (&blkt_400->slowness);
		  ms_gswap4 (&blkt_400->configuration);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "      beam azimuth (degrees): %g\n", blkt_400->azimuth);
		  ms_log (0, "  beam slowness (sec/degree): %g\n", blkt_400->slowness);
		  ms_log (0, "               configuration: %u\n", blkt_400->configuration);
		  if ( details > 1 )
		    ms_log (0, "          reserved bytes (2): %u,%u\n",
			    blkt_400->reserved[0], blkt_400->reserved[1]);
		}
	    }

	  else if ( blkt_type == 405 )
	    {
	      struct blkt_405_s *blkt_405 = (struct blkt_405_s *) (record + blkt_offset + 4);
	      uint16_t firstvalue = blkt_405->delay_values[0];  /* Work on a private copy */
	      
	      if ( swapflag )
		ms_gswap2 (&firstvalue);
	      
	      if ( details >= 1 )
		ms_log (0, "           first delay value: %u\n", firstvalue);
	    }
	  
	  else if ( blkt_type == 500 )
	    {
	      struct blkt_500_s *blkt_500 = (struct blkt_500_s *) (record + blkt_offset + 4);
	      
	      if ( swapflag )
		{
		  ms_gswap4 (&blkt_500->vco_correction);
		  MS_SWAPBTIME (&blkt_500->time);
		  ms_gswap4 (&blkt_500->exception_count);
		}
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "              VCO correction: %g%%\n", blkt_500->vco_correction);
		  ms_log (0, "           time of exception: %d,%d,%d:%d:%d.%04d (unused: %d)\n", blkt_500->time.year, blkt_500->time.day,
			  blkt_500->time.hour, blkt_500->time.min, blkt_500->time.sec, blkt_500->time.fract, blkt_500->time.unused);
		  ms_log (0, "                        usec: %d\n", blkt_500->usec);
		  ms_log (0, "           reception quality: %u%%\n", blkt_500->reception_qual);
		  ms_log (0, "             exception count: %u\n", blkt_500->exception_count);
		  ms_log (0, "              exception type: %.16s\n", blkt_500->exception_type);
		  ms_log (0, "                 clock model: %.32s\n", blkt_500->clock_model);
		  ms_log (0, "                clock status: %.128s\n", blkt_500->clock_status);
		}
	    }
	  
	  else if ( blkt_type == 1000 )
	    {
	      struct blkt_1000_s *blkt_1000 = (struct blkt_1000_s *) (record + blkt_offset + 4);
	      char order[40];
	      
	      /* Calculate record size in bytes as 2^(blkt_1000->rec_len) */
	      b1000reclen = (unsigned int) 1 << blkt_1000->reclen;
	      
	      /* Big or little endian? */
	      if (blkt_1000->byteorder == 0)
		strncpy (order, "Little endian", sizeof(order)-1);
	      else if (blkt_1000->byteorder == 1)
		strncpy (order, "Big endian", sizeof(order)-1);
	      else
		strncpy (order, "Unknown value", sizeof(order)-1);
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "                    encoding: %s (val:%u)\n",
			  (char *) ms_encodingstr (blkt_1000->encoding), blkt_1000->encoding);
		  ms_log (0, "                  byte order: %s (val:%u)\n",
			  order, blkt_1000->byteorder);
		  ms_log (0, "               record length: %d (val:%u)\n",
			  b1000reclen, blkt_1000->reclen);
		  
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_1000->reserved);
		}
	      
	      /* Save encoding format */
	      b1000encoding = blkt_1000->encoding;
	      
	      /* Sanity check encoding format */
	      if ( ! (b1000encoding >= 0 && b1000encoding <= 5) &&
		   ! (b1000encoding >= 10 && b1000encoding <= 19) &&
		   ! (b1000encoding >= 30 && b1000encoding <= 33) )
		{
		  ms_log (2, "%s: Blockette 1000 encoding format invalid (0-5,10-19,30-33): %d\n", srcname, b1000encoding);
		  retval++;
		}
	      
	      /* Sanity check byte order flag */
	      if ( blkt_1000->byteorder != 0 && blkt_1000->byteorder != 1 )
		{
		  ms_log (2, "%s: Blockette 1000 byte order flag invalid (0 or 1): %d\n", srcname, blkt_1000->byteorder);
		  retval++;
		}
	    }
	  
	  else if ( blkt_type == 1001 )
	    {
	      struct blkt_1001_s *blkt_1001 = (struct blkt_1001_s *) (record + blkt_offset + 4);
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "              timing quality: %u%%\n", blkt_1001->timing_qual);
		  ms_log (0, "                micro second: %d\n", blkt_1001->usec);
		  
		  if ( details > 1 )
		    ms_log (0, "               reserved byte: %u\n", blkt_1001->reserved);
		  
		  ms_log (0, "                 frame count: %u\n", blkt_1001->framecnt);
		}
	    }
	  
	  else if ( blkt_type == 2000 )
	    {
	      struct blkt_2000_s *blkt_2000 = (struct blkt_2000_s *) (record + blkt_offset + 4);
	      char order[40];
	      
	      if ( swapflag )
		{
		  ms_gswap2 (&blkt_2000->length);
		  ms_gswap2 (&blkt_2000->data_offset);
		  ms_gswap4 (&blkt_2000->recnum);
		}
	      
	      /* Big or little endian? */
	      if (blkt_2000->byteorder == 0)
		strncpy (order, "Little endian", sizeof(order)-1);
	      else if (blkt_2000->byteorder == 1)
		strncpy (order, "Big endian", sizeof(order)-1);
	      else
		strncpy (order, "Unknown value", sizeof(order)-1);
	      
	      if ( details >= 1 )
		{
		  ms_log (0, "            blockette length: %u\n", blkt_2000->length);
		  ms_log (0, "                 data offset: %u\n", blkt_2000->data_offset);
		  ms_log (0, "               record number: %u\n", blkt_2000->recnum);
		  ms_log (0, "                  byte order: %s (val:%u)\n",
			  order, blkt_2000->byteorder);
		  b = blkt_2000->flags;
		  ms_log (0, "                  data flags: [%u%u%u%u%u%u%u%u] 8 bits\n",
			  bit(b,0x01), bit(b,0x02), bit(b,0x04), bit(b,0x08),
			  bit(b,0x10), bit(b,0x20), bit(b,0x40), bit(b,0x80));
		  
		  if ( details > 1 )
		    {
		      if ( b & 0x01 ) ms_log (0, "                         [Bit 0] 1: Stream oriented\n");
		      else            ms_log (0, "                         [Bit 0] 0: Record oriented\n");
		      if ( b & 0x02 ) ms_log (0, "                         [Bit 1] 1: Blockette 2000s may NOT be packaged\n");
		      else            ms_log (0, "                         [Bit 1] 0: Blockette 2000s may be packaged\n");
		      if ( ! (b & 0x04) && ! (b & 0x08) )
			ms_log (0, "                      [Bits 2-3] 00: Complete blockette\n");
		      else if ( ! (b & 0x04) && (b & 0x08) )
			ms_log (0, "                      [Bits 2-3] 01: First blockette in span\n");
		      else if ( (b & 0x04) && (b & 0x08) )
			ms_log (0, "                      [Bits 2-3] 11: Continuation blockette in span\n");
		      else if ( (b & 0x04) && ! (b & 0x08) )
			ms_log (0, "                      [Bits 2-3] 10: Final blockette in span\n");
		      if ( ! (b & 0x10) && ! (b & 0x20) )
			ms_log (0, "                      [Bits 4-5] 00: Not file oriented\n");
		      else if ( ! (b & 0x10) && (b & 0x20) )
			ms_log (0, "                      [Bits 4-5] 01: First blockette of file\n");
		      else if ( (b & 0x10) && ! (b & 0x20) )
			ms_log (0, "                      [Bits 4-5] 10: Continuation of file\n");
		      else if ( (b & 0x10) && (b & 0x20) )
			ms_log (0, "                      [Bits 4-5] 11: Last blockette of file\n");
		    }
		  
		  ms_log (0, "           number of headers: %u\n", blkt_2000->numheaders);
		  
		  /* Crude display of the opaque data headers */
		  if ( details > 1 )
		    ms_log (0, "                     headers: %.*s\n",
			    (blkt_2000->data_offset - 15), blkt_2000->payload);
		}
	    }
	  
	  else
	    {
	      ms_log (2, "%s: Unrecognized blockette type: %d\n", srcname, blkt_type);
	      retval++;
	    }
	  
	  /* Sanity check the next blockette offset */
	  if ( next_blkt && next_blkt <= endofblockettes )
	    {
	      ms_log (2, "%s: Next blockette offset (%d) is within current blockette ending at byte %d\n",
		      srcname, next_blkt, endofblockettes);
	      blkt_offset = 0;
	    }
	  else
	    {
	      blkt_offset = next_blkt;
	    }
	  
	  blkt_count++;
	} /* End of looping through blockettes */
      
      /* Check that the blockette offset is within the maximum record size */
      if ( blkt_offset > maxreclen )
	{
	  ms_log (2, "%s: Blockette offset (%d) beyond maximum record length (%d)\n", srcname, blkt_offset, maxreclen);
	  retval++;
	}
      
      /* Check that the data and blockette offsets are within the record */
      if ( b1000reclen && fsdh->data_offset > b1000reclen )
	{
	  ms_log (2, "%s: Data offset (%d) beyond record length (%d)\n", srcname, fsdh->data_offset, b1000reclen);
	  retval++;
	}
      if ( b1000reclen && fsdh->blockette_offset > b1000reclen )
	{
	  ms_log (2, "%s: Blockette offset (%d) beyond record length (%d)\n", srcname, fsdh->blockette_offset, b1000reclen);
	  retval++;
	}
      
      /* Check that the data offset is beyond the end of the blockettes */
      if ( fsdh->numsamples && fsdh->data_offset <= endofblockettes )
	{
	  ms_log (2, "%s: Data offset (%d) is within blockette chain (end of blockettes: %d)\n", srcname, fsdh->data_offset, endofblockettes);
	  retval++;
	}
      
      /* Check that the correct number of blockettes were parsed */
      if ( fsdh->numblockettes != blkt_count )
	{
	  ms_log (2, "%s: Specified number of blockettes (%d) not equal to those parsed (%d)\n", srcname, fsdh->numblockettes, blkt_count);
	  retval++;
	}
    }
  
  return retval;
} /* End of ms_parse_raw() */
/***************************************************************************
 * selection.c:
 *
 * Generic routines to manage selection lists.
 *
 * Written by Chad Trabant unless otherwise noted
 *   IRIS Data Management Center
 *
 * modified: 2010.068
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>

#include "libmseed.h"

static int ms_globmatch (char *string, char *pattern);


/***************************************************************************
 * ms_matchselect:
 *
 * Test the specified parameters for a matching selection entry.  The
 * srcname parameter may contain globbing characters.  The NULL value
 * (matching any times) for the start and end times is HPTERROR.
 *
 * Return Selections pointer to matching entry on successful match and
 * NULL for no match or error.
 ***************************************************************************/
Selections *
ms_matchselect (Selections *selections, char *srcname, hptime_t starttime,
		hptime_t endtime, SelectTime **ppselecttime)
{
  Selections *findsl = NULL;
  SelectTime *findst = NULL;
  SelectTime *matchst = NULL;
  
  if ( selections )
    {
      findsl = selections;
      while ( findsl )
	{
	  if ( ms_globmatch (srcname, findsl->srcname) )
	    {
	      findst = findsl->timewindows;              
	      while ( findst )
		{
		  if ( starttime != HPTERROR && findst->starttime != HPTERROR &&
		       (starttime < findst->starttime && ! (starttime <= findst->starttime && endtime >= findst->starttime)) )
		    { findst = findst->next; continue; }
		  else if ( endtime != HPTERROR && findst->endtime != HPTERROR &&
			    (endtime > findst->endtime && ! (starttime <= findst->endtime && endtime >= findst->endtime)) )
		    { findst = findst->next; continue; }
		  
		  matchst = findst;
		  break;
		}
	    }
	  
	  if ( matchst )
	    break;
	  else
	    findsl = findsl->next;
	}
    }
  
  if ( ppselecttime )
    *ppselecttime = matchst;
  
  return ( matchst ) ? findsl : NULL;
} /* End of ms_matchselect() */


/***************************************************************************
 * msr_matchselect:
 *
 * A simple wrapper for calling ms_matchselect() using details from a
 * MSRecord struct.
 *
 * Return Selections pointer to matching entry on successful match and
 * NULL for no match or error.
 ***************************************************************************/
Selections *
msr_matchselect (Selections *selections, MSRecord *msr, SelectTime **ppselecttime)
{
  char srcname[50];
  hptime_t endtime;
  
  if ( ! selections || ! msr )
    return NULL;
  
  msr_srcname (msr, srcname, 1);
  endtime = msr_endtime (msr);
  
  return ms_matchselect (selections, srcname, msr->starttime, endtime,
			 ppselecttime);
} /* End of msr_matchselect() */


/***************************************************************************
 * ms_addselect:
 *
 * Add select parameters to a specified selection list.  The srcname
 * argument may contain globbing parameters.  The NULL value (matching
 * any value) for the start and end times is HPTERROR.
 *
 * Return 0 on success and -1 on error.
 ***************************************************************************/
int
ms_addselect (Selections **ppselections, char *srcname,
	      hptime_t starttime, hptime_t endtime)
{
  Selections *newsl = NULL;
  SelectTime *newst = NULL;
  
  if ( ! ppselections || ! srcname )
    return -1;
  
  /* Allocate new SelectTime and populate */
  if ( ! (newst = (SelectTime *) calloc (1, sizeof(SelectTime))) )
    {
      ms_log (2, "Cannot allocate memory\n");
      return -1;
    }
  
  newst->starttime = starttime;
  newst->endtime = endtime;
  
  /* Add new Selections struct to begining of list */
  if ( ! *ppselections )
    {
      /* Allocate new Selections and populate */
      if ( ! (newsl = (Selections *) calloc (1, sizeof(Selections))) )
	{
	  ms_log (2, "Cannot allocate memory\n");
	  return -1;
	}
      
      strncpy (newsl->srcname, srcname, sizeof(newsl->srcname));
      
      /* Add new Selections struct as first in list */
      *ppselections = newsl;
      newsl->timewindows = newst;
    }
  else
    {
      Selections *findsl = *ppselections;
      Selections *matchsl = 0;
      
      /* Search for matching Selectlink entry */
      while ( findsl )
	{
	  if ( ! strcmp (findsl->srcname, srcname) )
	    {
	      matchsl = findsl;
	      break;
	    }
	  
	  findsl = findsl->next;
	}
      
      if ( matchsl )
	{
	  /* Add time window selection to beginning of window list */
	  newst->next = matchsl->timewindows;
	  matchsl->timewindows = newst;
	}
      else
	{
	  /* Allocate new Selections and populate */
	  if ( ! (newsl = (Selections *) calloc (1, sizeof(Selections))) )
	    {
	      ms_log (2, "Cannot allocate memory\n");
	      return -1;
	    }
	  
	  strncpy (newsl->srcname, srcname, sizeof(newsl->srcname));
	  
	  /* Add new Selections to beginning of list */
	  newsl->next = *ppselections;
	  *ppselections = newsl;
	  newsl->timewindows = newst;
	}
    }
  
  return 0;
} /* End of ms_addselect() */


/***************************************************************************
 * ms_addselect_comp:
 *
 * Add select parameters to a specified selection list based on
 * separate name components.  The network, station, location, channel
 * and quality arguments may contain globbing parameters.  The NULL
 * value (matching any value) for the start and end times is HPTERROR.
 *
 * If any of the naming parameters are not supplied (pointer is NULL)
 * a wildcard for all matches is substituted.  As a special case, if
 * the location ID (loc) is set to "--" to match a space-space/blank
 * ID it will be translated to an empty string to match libmseed's
 * notation.
 *
 * Return 0 on success and -1 on error.
 ***************************************************************************/
int
ms_addselect_comp (Selections **ppselections, char *net, char* sta, char *loc,
		   char *chan, char *qual, hptime_t starttime, hptime_t endtime)
{
  char srcname[100];
  char selnet[20];
  char selsta[20];
  char selloc[20];
  char selchan[20];
  char selqual[20];
  
  if ( ! ppselections )
    return -1;
  
  if ( net )
    {
      strncpy (selnet, net, sizeof(selnet));
      selnet[sizeof(selnet)-1] = '\0';
    }
  else
    strcpy (selnet, "*");
  
  if ( sta )
    {
      strncpy (selsta, sta, sizeof(selsta));
      selsta[sizeof(selsta)-1] = '\0';
    }
  else
    strcpy (selsta, "*");
  
  if ( loc )
    {
      /* Test for special case blank location ID */
      if ( ! strcmp (loc, "--") )
	selloc[0] = '\0';
      else
	{
	  strncpy (selloc, loc, sizeof(selloc));
	  selloc[sizeof(selloc)-1] = '\0';
	}
    }
  else
    strcpy (selloc, "*");
  
  if ( chan )
    {
      strncpy (selchan, chan, sizeof(selchan));
      selchan[sizeof(selchan)-1] = '\0';
    }
  else
    strcpy (selchan, "*");
  
  if ( qual )
    {
      strncpy (selqual, qual, sizeof(selqual));
      selqual[sizeof(selqual)-1] = '\0';
    }
  else
    strcpy (selqual, "?");
  
  /* Create the srcname globbing match for this entry */
  snprintf (srcname, sizeof(srcname), "%s_%s_%s_%s_%s",
	    selnet, selsta, selloc, selchan, selqual);
  
  /* Add selection to list */
  if ( ms_addselect (ppselections, srcname, starttime, endtime) )
    return -1;
  
  return 0;
} /* End of ms_addselect_comp() */


/***************************************************************************
 * ms_readselectionsfile:
 *
 * Read a list of data selections from a file and them to the
 * specified selections list.  On errors this routine will leave
 * allocated memory unreachable (leaked), it is expected that this is
 * a program failing condition.
 *
 * As a special case if the filename is "-", selection lines will be
 * read from stdin.
 *
 * Returns count of selections added on success and -1 on error.
 ***************************************************************************/
int
ms_readselectionsfile (Selections **ppselections, char *filename)
{
  FILE *fp;
  hptime_t starttime;
  hptime_t endtime;
  char selectline[200];
  char *selnet;
  char *selsta;
  char *selloc;
  char *selchan;
  char *selqual;
  char *selstart;
  char *selend;
  char *cp;
  char next;
  int selectcount = 0;
  int linecount = 0;
  
  if ( ! ppselections || ! filename )
    return -1;
  
  if ( strcmp (filename, "-" ) )
    {
      if ( ! (fp = fopen(filename, "rb")) )
	{
	  ms_log (2, "Cannot open file %s: %s\n", filename, strerror(errno));
	  return -1;
	}
    }
  else
    {
      /* Use stdin as special case */
      fp = stdin;
    }
  
  while ( fgets (selectline, sizeof(selectline)-1, fp) )
    {
      selnet = 0;
      selsta = 0;
      selloc = 0;
      selchan = 0;
      selqual = 0;
      selstart = 0;
      selend = 0;
      
      linecount++;
      
      /* Guarantee termination */
      selectline[sizeof(selectline)-1] = '\0';
      
      /* End string at first newline character if any */
      if ( (cp = strchr(selectline, '\n')) )
        *cp = '\0';
      
      /* Skip empty lines */
      if ( ! strlen (selectline) )
        continue;
      
      /* Skip comment lines */
      if ( *selectline == '#' )
        continue;
      
      /* Parse: identify components of selection and terminate */
      cp = selectline;
      next = 1;
      while ( *cp )
	{
	  if ( *cp == ' ' || *cp == '\t' ) { *cp = '\0'; next = 1; }
	  else if ( *cp == '#' ) { *cp = '\0'; break; }
	  else if ( next && ! selnet ) { selnet = cp; next = 0; }
	  else if ( next && ! selsta ) { selsta = cp; next = 0; }
	  else if ( next && ! selloc ) { selloc = cp; next = 0; }
	  else if ( next && ! selchan ) { selchan = cp; next = 0; }
	  else if ( next && ! selqual ) { selqual = cp; next = 0; }
	  else if ( next && ! selstart ) { selstart = cp; next = 0; }
	  else if ( next && ! selend ) { selend = cp; next = 0; }
	  else if ( next ) { *cp = '\0'; break; }
	  cp++;
	}
      
      /* Skip line if network, station, location and channel are not defined */
      if ( ! selnet || ! selsta || ! selloc || ! selchan )
	{
	  ms_log (2, "[%s] Skipping data selection line number %d\n", filename, linecount);
	  continue;
	}
      
      if ( selstart )
	{
	  starttime = ms_seedtimestr2hptime (selstart);
	  if ( starttime == HPTERROR )
	    {
	      ms_log (2, "Cannot convert data selection start time (line %d): %s\n", linecount, selstart);
	      return -1;
	    }
	}
      else
	{
	  starttime = HPTERROR;
	}
      
      if ( selend )
	{
	  endtime = ms_seedtimestr2hptime (selend);
	  if ( endtime == HPTERROR )
	    {
	      ms_log (2, "Cannot convert data selection end time (line %d): %s\n",  linecount, selend);
	      return -1;
	    }
	}
      else
	{
	  endtime = HPTERROR;
	}
      
      /* Add selection to list */
      if ( ms_addselect_comp (ppselections, selnet, selsta, selloc, selchan, selqual, starttime, endtime) )
	{
	  ms_log (2, "[%s] Error adding selection on line %d\n", filename, linecount);
	  return -1;
	}
      
      selectcount++;
    }
  
  if ( fp != stdin )
    fclose (fp);
  
  return selectcount;
} /* End of ms_readselectionsfile() */


/***************************************************************************
 * ms_freeselections:
 *
 * Free all memory associated with a Selections struct.
 ***************************************************************************/
void
ms_freeselections ( Selections *selections )
{
  Selections *select;
  Selections *selectnext;
  SelectTime *selecttime;
  SelectTime *selecttimenext;
  
  if ( selections )
    {
      select = selections;
      
      while ( select )
	{
	  selectnext = select->next;
	  
	  selecttime = select->timewindows;
	  
	  while ( selecttime )
	    {
	      selecttimenext = selecttime->next;
	      
	      free (selecttime);
	      
	      selecttime = selecttimenext;
	    }
	  
	  free (select);
	  
	  select = selectnext;
	}
    }

} /* End of ms_freeselections() */


/***************************************************************************
 * ms_printselections:
 *
 * Print the selections list using the ms_log() facility.
 ***************************************************************************/
void
ms_printselections ( Selections *selections )
{
  Selections *select;
  SelectTime *selecttime;
  char starttime[50];
  char endtime[50];
  
  if ( ! selections )
    return;
  
  select = selections;
  while ( select )
    {
      ms_log (0, "Selection: %s\n", select->srcname);
      
      selecttime = select->timewindows;
      while ( selecttime )
	{
	  if ( selecttime->starttime != HPTERROR )
	    ms_hptime2seedtimestr (selecttime->starttime, starttime, 1);
	  else
	    strncpy (starttime, "No start time", sizeof(starttime)-1);
	  
	  if ( selecttime->endtime != HPTERROR )
	    ms_hptime2seedtimestr (selecttime->endtime, endtime, 1);
	  else
	    strncpy (endtime, "No end time", sizeof(endtime)-1);
	  
	  ms_log (0, "  %30s  %30s\n", starttime, endtime);
	  
	  selecttime = selecttime->next;
	}
      
      select = select->next;
    }
} /* End of ms_printselections() */


/***********************************************************************
 * robust glob pattern matcher
 * ozan s. yigit/dec 1994
 * public domain
 *
 * glob patterns:
 *	*	matches zero or more characters
 *	?	matches any single character
 *	[set]	matches any character in the set
 *	[^set]	matches any character NOT in the set
 *		where a set is a group of characters or ranges. a range
 *		is written as two characters seperated with a hyphen: a-z denotes
 *		all characters between a to z inclusive.
 *	[-set]	set matches a literal hypen and any character in the set
 *	[]set]	matches a literal close bracket and any character in the set
 *
 *	char	matches itself except where char is '*' or '?' or '['
 *	\char	matches char, including any pattern character
 *
 * examples:
 *	a*c		ac abc abbc ...
 *	a?c		acc abc aXc ...
 *	a[a-z]c		aac abc acc ...
 *	a[-a-z]c	a-c aac abc ...
 *
 * Revision 1.4  2004/12/26  12:38:00  ct
 * Changed function name (amatch -> globmatch), variables and
 * formatting for clarity.  Also add matching header globmatch.h.
 *
 * Revision 1.3  1995/09/14  23:24:23  oz
 * removed boring test/main code.
 *
 * Revision 1.2  94/12/11  10:38:15  oz
 * charset code fixed. it is now robust and interprets all
 * variations of charset [i think] correctly, including [z-a] etc.
 * 
 * Revision 1.1  94/12/08  12:45:23  oz
 * Initial revision
 ***********************************************************************/

#define GLOBMATCH_TRUE    1
#define GLOBMATCH_FALSE   0
#define GLOBMATCH_NEGATE '^'       /* std char set negation char */

/***********************************************************************
 * ms_globmatch:
 *
 * Check if a string matches a globbing pattern.
 *
 * Return 0 if string does not match pattern and non-zero otherwise.
 **********************************************************************/
static int
ms_globmatch (char *string, char *pattern)
{
  int negate;
  int match;
  int c;
  
  while ( *pattern )
    {
      if ( !*string && *pattern != '*' )
	return GLOBMATCH_FALSE;
      
      switch ( c = *pattern++ )
	{
	  
	case '*':
	  while ( *pattern == '*' )
	    pattern++;
	  
	  if ( !*pattern )
	    return GLOBMATCH_TRUE;
	  
	  if ( *pattern != '?' && *pattern != '[' && *pattern != '\\' )
	    while ( *string && *pattern != *string )
	      string++;
	  
	  while ( *string )
	    {
	      if ( ms_globmatch(string, pattern) )
		return GLOBMATCH_TRUE;
	      string++;
	    }
	  return GLOBMATCH_FALSE;
	  
	case '?':
	  if ( *string )
	    break;
	  return GLOBMATCH_FALSE;
	  
	  /* set specification is inclusive, that is [a-z] is a, z and
	   * everything in between. this means [z-a] may be interpreted
	   * as a set that contains z, a and nothing in between.
	   */
	case '[':
	  if ( *pattern != GLOBMATCH_NEGATE )
	    negate = GLOBMATCH_FALSE;
	  else
	    {
	      negate = GLOBMATCH_TRUE;
	      pattern++;
	    }
	  
	  match = GLOBMATCH_FALSE;
	  
	  while ( !match && (c = *pattern++) )
	    {
	      if ( !*pattern )
		return GLOBMATCH_FALSE;
	      
	      if ( *pattern == '-' ) 	/* c-c */
		{
		  if ( !*++pattern )
		    return GLOBMATCH_FALSE;
		  if ( *pattern != ']' )
		    {
		      if ( *string == c || *string == *pattern ||
			   ( *string > c && *string < *pattern ) )
			match = GLOBMATCH_TRUE;
		    }
		  else
		    {		/* c-] */
		      if ( *string >= c )
			match = GLOBMATCH_TRUE;
		      break;
		    }
		}
	      else			/* cc or c] */
		{
		  if ( c == *string )
		    match = GLOBMATCH_TRUE;
		  if ( *pattern != ']' )
		    {
		      if ( *pattern == *string )
			match = GLOBMATCH_TRUE;
		    }
		  else
		    break;
		}
	    } 
	  
	  if ( negate == match )
	    return GLOBMATCH_FALSE;
	  
	  /*
	   * if there is a match, skip past the charset and continue on
	   */
	  while ( *pattern && *pattern != ']' )
	    pattern++;
	  if ( !*pattern++ )	/* oops! */
	    return GLOBMATCH_FALSE;
	  break;
	  
	case '\\':
	  if ( *pattern )
	    c = *pattern++;
	default:
	  if ( c != *string )
	    return GLOBMATCH_FALSE;
	  break;
	}
      
      string++;
    }
  
  return !*string;
}  /* End of ms_globmatch() */
/***************************************************************************
 * tracelist.c:
 *
 * Routines to handle TraceList and related structures.
 *
 * Written by Chad Trabant, IRIS Data Management Center
 *
 * modified: 2012.105
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "libmseed.h"


MSTraceSeg *mstl_msr2seg (MSRecord *msr, hptime_t endtime);
MSTraceSeg *mstl_addmsrtoseg (MSTraceSeg *seg, MSRecord *msr, hptime_t endtime, flag whence);
MSTraceSeg *mstl_addsegtoseg (MSTraceSeg *seg1, MSTraceSeg *seg2);


/***************************************************************************
 * mstl_init:
 *
 * Initialize and return a MSTraceList struct, allocating memory if
 * needed.  If the supplied MSTraceList is not NULL any associated
 * memory it will be freed including data at prvtptr pointers.
 *
 * Returns a pointer to a MSTraceList struct on success or NULL on error.
 ***************************************************************************/
MSTraceList *
mstl_init ( MSTraceList *mstl )
{
  if ( mstl )
    {
      mstl_free (&mstl, 1);
    }
  
  mstl = (MSTraceList *) malloc (sizeof(MSTraceList));
  
  if ( mstl == NULL )
    {
      ms_log (2, "mstl_init(): Cannot allocate memory\n");
      return NULL;
    }
  
  memset (mstl, 0, sizeof (MSTraceList));
  
  return mstl;
} /* End of mstl_init() */


/***************************************************************************
 * mstl_free:
 *
 * Free all memory associated with a MSTraceList struct and set the
 * pointer to 0.
 *
 * If the freeprvtptr flag is true any private pointer data will also
 * be freed when present.
 ***************************************************************************/
void
mstl_free ( MSTraceList **ppmstl, flag freeprvtptr )
{
  MSTraceID *id = 0;
  MSTraceID *nextid = 0;
  MSTraceSeg *seg = 0;
  MSTraceSeg *nextseg = 0;  
  
  if ( ! ppmstl )
    return;
  
  if ( *ppmstl )
    {
      /* Free any associated traces */
      id = (*ppmstl)->traces;
      while ( id )
	{
	  nextid = id->next;
	  
	  /* Free any associated trace segments */
	  seg = id->first;
	  while ( seg )
	    {
	      nextseg = seg->next;
	      
	      /* Free private pointer data if present and requested*/
	      if ( freeprvtptr && seg->prvtptr )
		free (seg->prvtptr);
	      
	      /* Free data array if allocated */
	      if ( seg->datasamples )
		free (seg->datasamples);
	      
	      free (seg);
	      seg = nextseg;
	    }
	  
	  /* Free private pointer data if present and requested*/
	  if ( freeprvtptr && id->prvtptr )
	    free (id->prvtptr);
	  
	  free (id);
	  id = nextid;
	}
      
      free (*ppmstl);
      
      *ppmstl = NULL;
    }
  
  return;
} /* End of mstl_free() */


/***************************************************************************
 * mstl_addmsr:
 *
 * Add data coverage from an MSRecord to a MSTraceList by searching the
 * list for the appropriate MSTraceID and MSTraceSeg and either adding
 * data to it or creating a new MStraceID and/or MSTraceSeg if needed.
 *
 * If the dataquality flag is true the data quality bytes must also
 * match otherwise they are ignored.
 *
 * If the autoheal flag is true extra processing is invoked to conjoin
 * trace segments that fit together after the MSRecord coverage is
 * added.  For segments that are removed, any memory at the prvtptr
 * will be freed.
 *
 * An MSTraceList is always maintained with the MSTraceIDs in
 * descending alphanumeric order.  MSTraceIDs are always maintained
 * with MSTraceSegs in data time time order.
 *
 * Return a pointer to the MSTraceSeg updated or 0 on error.
 ***************************************************************************/
MSTraceSeg *
mstl_addmsr ( MSTraceList *mstl, MSRecord *msr, flag dataquality,
	      flag autoheal, double timetol, double sampratetol )
{
  MSTraceID *id = 0;
  MSTraceID *searchid = 0;
  MSTraceID *ltid = 0;
  
  MSTraceSeg *seg = 0;
  MSTraceSeg *searchseg = 0;
  MSTraceSeg *segbefore = 0;
  MSTraceSeg *segafter = 0;
  MSTraceSeg *followseg = 0;
  
  hptime_t endtime;
  hptime_t pregap;
  hptime_t postgap;
  hptime_t lastgap;
  hptime_t firstgap;
  hptime_t hpdelta;
  hptime_t hptimetol = 0;
  hptime_t nhptimetol = 0;
  
  char srcname[45];
  char *s1, *s2;
  flag whence;
  flag lastratecheck;
  flag firstratecheck;
  int mag;
  int cmp;
  int ltmag;
  int ltcmp;
  
  if ( ! mstl || ! msr )
    return 0;
  
  /* Calculate end time for MSRecord */
  if ( (endtime = msr_endtime (msr)) == HPTERROR )
    {
      ms_log (2, "mstl_addmsr(): Error calculating record end time\n");
      return 0;
    }
  
  /* Generate source name string */
  if ( ! msr_srcname (msr, srcname, dataquality) )
    {
      ms_log (2, "mstl_addmsr(): Error generating srcname for MSRecord\n");
      return 0;
    }
  
  /* Search for matching trace ID starting with last accessed ID and
     then looping through the trace ID list. */
  if ( mstl->last )
    {
      s1 = mstl->last->srcname;
      s2 = srcname;
      while ( *s1 == *s2++ )
	{
	  if ( *s1++ == '\0' )
	    break;
	}
      cmp = (*s1 - *--s2);
      
      if ( ! cmp )
	{
	  id = mstl->last;
	}
      else
	{
	  /* Loop through trace ID list searching for a match, simultaneously
	   * track the source name which is closest but less than the MSRecord
	   * to allow for later insertion with sort order. */
	  searchid = mstl->traces;
	  ltcmp = 0;
	  ltmag = 0;
	  while ( searchid )
	    {
	      /* Compare source names */
	      s1 = searchid->srcname;
	      s2 = srcname;
	      mag = 0;
	      while ( *s1 == *s2++ )
		{
		  mag++;
		  if ( *s1++ == '\0' )
		    break;
		}
	      cmp = (*s1 - *--s2);
	      
	      /* If source names did not match track closest "less than" value 
	       * and continue searching. */
	      if ( cmp != 0 )
		{
		  if ( cmp < 0 )
		    {
		      if ( (ltcmp == 0 || cmp >= ltcmp) && mag >= ltmag )
			{
			  ltcmp = cmp;
			  ltmag = mag;
			  ltid = searchid;
			}
		      else if ( mag > ltmag )
			{
			  ltcmp = cmp;
			  ltmag = mag;
			  ltid = searchid;
			}
		    }
		  
		  searchid = searchid->next;
		  continue;
		}
	      
	      /* If we made it this far we found a match */
	      id = searchid;
	      break;
	    }
	}
    } /* Done searching for match in trace ID list */
  
  /* If no matching ID was found create new MSTraceID and MSTraceSeg entries */
  if ( ! id )
    {
      if ( ! (id = (MSTraceID *) calloc (1, sizeof(MSTraceID))) )
	{
	  ms_log (2, "mstl_addmsr(): Error allocating memory\n");
	  return 0;
	}
      
      /* Populate MSTraceID */
      strcpy (id->network, msr->network);
      strcpy (id->station, msr->station);
      strcpy (id->location, msr->location);
      strcpy (id->channel, msr->channel);
      id->dataquality = msr->dataquality;
      strcpy (id->srcname, srcname);
      
      id->earliest = msr->starttime;
      id->latest = endtime;
      id->numsegments = 1;
      
      if ( ! (seg = mstl_msr2seg (msr, endtime)) )
	{
	  return 0;
	}
      id->first = id->last = seg;
      
      /* Add new MSTraceID to MSTraceList */
      if ( ! mstl->traces || ! ltid )
	{
	  id->next = mstl->traces;
	  mstl->traces = id;
	}
      else
	{
	  id->next = ltid->next;
	  ltid->next = id;
	}
      
      mstl->numtraces++;
    }
  /* Add data coverage to the matching MSTraceID */
  else
    {
      /* Calculate high-precision sample period */
      hpdelta = (hptime_t) (( msr->samprate ) ? (HPTMODULUS / msr->samprate) : 0.0);
      
      /* Calculate high-precision time tolerance */
      if ( timetol == -1.0 )
	hptimetol = (hptime_t) (0.5 * hpdelta);   /* Default time tolerance is 1/2 sample period */
      else if ( timetol >= 0.0 )
	hptimetol = (hptime_t) (timetol * HPTMODULUS);
      
      nhptimetol = ( hptimetol ) ? -hptimetol : 0;
      
      /* last/firstgap are negative when the record overlaps the trace
       * segment and positive when there is a time gap. */
      
      /* Gap relative to the last segment */
      lastgap = msr->starttime - id->last->endtime - hpdelta;
      
      /* Gap relative to the first segment */
      firstgap = id->first->starttime - endtime - hpdelta;
      
      /* Sample rate tolerance checks for first and last segments */
      if ( sampratetol == -1.0 )
	{
	  lastratecheck = MS_ISRATETOLERABLE (msr->samprate, id->last->samprate);
	  firstratecheck = MS_ISRATETOLERABLE (msr->samprate, id->first->samprate);
	}
      else
	{
	  lastratecheck = (ms_dabs (msr->samprate - id->last->samprate) > sampratetol) ? 0 : 1;
	  firstratecheck = (ms_dabs (msr->samprate - id->first->samprate) > sampratetol) ? 0 : 1;
	}
      
      /* Search first for the simple scenarios in order of likelihood:
       * - Record fits at end of last segment
       * - Record fits after all coverage
       * - Record fits before all coverage
       * - Record fits at beginning of first segment
       *
       * If none of those scenarios are true search the complete segment list.
       */
      
      /* Record coverage fits at end of last segment */
      if ( lastgap <= hptimetol && lastgap >= nhptimetol && lastratecheck )
	{
	  if ( ! mstl_addmsrtoseg (id->last, msr, endtime, 1) )
	    return 0;
	  
	  seg = id->last;
	  
	  if ( endtime > id->latest )
	    id->latest = endtime;
	}
      /* Record coverage is after all other coverage */
      else if ( (msr->starttime - hpdelta - hptimetol) > id->latest )
	{
	  if ( ! (seg = mstl_msr2seg (msr, endtime)) )
	    return 0;
	  
	  /* Add to end of list */
	  id->last->next = seg;
	  seg->prev = id->last;
	  id->last = seg;
	  id->numsegments++;
	  
	  if ( endtime > id->latest )
	    id->latest = endtime;
	}
      /* Record coverage is before all other coverage */
      else if ( (endtime + hpdelta + hptimetol) < id->earliest )
	{
	  if ( ! (seg = mstl_msr2seg (msr, endtime)) )
	    return 0;
	  
	  /* Add to beginning of list */
	  id->first->prev = seg;
	  seg->next = id->first;
	  id->first = seg;
	  id->numsegments++;
	  
	  if ( msr->starttime < id->earliest )
	    id->earliest = msr->starttime;
	}
      /* Record coverage fits at beginning of first segment */
      else if ( firstgap <= hptimetol && firstgap >= nhptimetol && firstratecheck )
	{
	  if ( ! mstl_addmsrtoseg (id->first, msr, endtime, 2) )
	    return 0;
	  
	  seg = id->first;
	  
	  if ( msr->starttime < id->earliest )
	    id->earliest = msr->starttime;
	}
      /* Search complete segment list for matches */
      else
	{
	  searchseg = id->first;
	  segbefore = 0;     /* Find segment that record fits before */
	  segafter = 0;      /* Find segment that record fits after */
	  followseg = 0;     /* Track segment that record follows in time order */
	  while ( searchseg )
	    {
	      if ( msr->starttime > searchseg->starttime )
		followseg = searchseg;
	      
	      whence = 0;
	      
	      postgap = msr->starttime - searchseg->endtime - hpdelta;
	      if ( ! segbefore && postgap <= hptimetol && postgap >= nhptimetol )
		whence = 1;
	      
	      pregap = searchseg->starttime - endtime - hpdelta;
	      if ( ! segafter && pregap <= hptimetol && pregap >= nhptimetol )
		whence = 2;
	      
	      if ( ! whence )
		{
		  searchseg = searchseg->next;
		  continue;
		}
	      
	      if ( sampratetol == -1.0 )
		{
		  if ( ! MS_ISRATETOLERABLE (msr->samprate, searchseg->samprate) )
		    {
		      searchseg = searchseg->next;
		      continue;
		    }
		}
	      else
		{
		  if ( ms_dabs (msr->samprate - searchseg->samprate) > sampratetol )
		    {
		      searchseg = searchseg->next;
		      continue;
		    }
		}
	      
	      if ( whence == 1 )
		segbefore = searchseg;
	      else
		segafter = searchseg;
	      
	      /* Done searching if not autohealing */
	      if ( ! autoheal )
		break;
	      
	      /* Done searching if both before and after segments are found */
	      if ( segbefore && segafter )
		break;
	      
	      searchseg = searchseg->next;
	    } /* Done looping through segments */
	  
	  /* Add MSRecord coverage to end of segment before */
	  if ( segbefore )
	    {
	      if ( ! mstl_addmsrtoseg (segbefore, msr, endtime, 1) )
		{
		  return 0;
		}
	      
	      /* Merge two segments that now fit if autohealing */
	      if ( autoheal && segafter && segbefore != segafter )
		{
		  /* Add segafter coverage to segbefore */
		  if ( ! mstl_addsegtoseg (segbefore, segafter) )
		    {
		      return 0;
		    }
		  
		  /* Shift last segment pointer if it's going to be removed */
		  if ( segafter == id->last )
		    id->last = id->last->prev;
		  
		  /* Remove segafter from list */
		  if ( segafter->prev )
		    segafter->prev->next = segafter->next;
		  if ( segafter->next )
		    segafter->next->prev = segafter->prev;
		  
		  /* Free data samples, private data and segment structure */
		  if (segafter->datasamples)
		    free (segafter->datasamples);
		  
		  if (segafter->prvtptr)
		    free (segafter->prvtptr);
		  
		  free (segafter);
		}
	      
	      seg = segbefore;
	    }
	  /* Add MSRecord coverage to beginning of segment after */
	  else if ( segafter )
	    {
	      if ( ! mstl_addmsrtoseg (segafter, msr, endtime, 2) )
		{
		  return 0;
		}
	      
	      seg = segafter;
	    }
	  /* Add MSRecord coverage to new segment */
	  else
	    {
	      /* Create new segment */
	      if ( ! (seg = mstl_msr2seg (msr, endtime)) )
		{
		  return 0;
		}
	      
	      /* Add new segment as first in list */
	      if ( ! followseg )
		{
		  seg->next = id->first;
		  if ( id->first )
		    id->first->prev = seg;
		  
		  id->first = seg;
		}
	      /* Add new segment after the followseg segment */
	      else
		{
		  seg->next = followseg->next;
		  seg->prev = followseg;
		  if ( followseg->next )
		    followseg->next->prev = seg;
		  followseg->next = seg;
		  
		  if ( followseg == id->last )
		    id->last = seg;
		}
	      
	      id->numsegments++;
	    }
	  
	  /* Track earliest and latest times */
	  if ( msr->starttime < id->earliest )
	    id->earliest = msr->starttime;
	  
	  if ( endtime > id->latest )
	    id->latest = endtime;
	} /* End of searching segment list */
    } /* End of adding coverage to matching ID */
  
  /* Sort modified segment into place, logic above should limit these to few shifts if any */
  while ( seg->next && ( seg->starttime > seg->next->starttime ||
			 (seg->starttime == seg->next->starttime && seg->endtime < seg->next->endtime) ) )
    {
      /* Move segment down list, swap seg and seg->next */
      segafter = seg->next;
      
      if ( seg->prev )
	seg->prev->next = segafter;
      
      if ( segafter->next )
	segafter->next->prev = seg;
      
      segafter->prev = seg->prev;
      seg->prev = segafter;
      seg->next = segafter->next;
      segafter->next = seg;
      
      /* Reset first and last segment pointers if replaced */
      if ( id->first == seg )
	id->first = segafter;
      
      if ( id->last == segafter )
	id->last = seg;
    }
  while ( seg->prev && ( seg->starttime < seg->prev->starttime ||
			 (seg->starttime == seg->prev->starttime && seg->endtime > seg->prev->endtime) ) )
    {
      /* Move segment up list, swap seg and seg->prev */
      segbefore = seg->prev;
      
      if ( seg->next )
	seg->next->prev = segbefore;
      
      if ( segbefore->prev )
	segbefore->prev->next = seg;
      
      segbefore->next = seg->next;
      seg->next = segbefore;
      seg->prev = segbefore->prev;
      segbefore->prev = seg;
      
      /* Reset first and last segment pointers if replaced */
      if ( id->first == segbefore )
	id->first = seg;
      
      if ( id->last == seg )
	id->last = segbefore;
    }
  
  /* Set MSTraceID as last accessed */
  mstl->last = id;
  
  return seg;
}  /* End of mstl_addmsr() */


/***************************************************************************
 * mstl_msr2seg:
 *
 * Create an MSTraceSeg structure from an MSRecord structure.
 *
 * Return a pointer to a MSTraceSeg otherwise 0 on error.
 ***************************************************************************/
MSTraceSeg *
mstl_msr2seg (MSRecord *msr, hptime_t endtime)
{
  MSTraceSeg *seg = 0;
  int samplesize;
  
  if ( ! (seg = (MSTraceSeg *) calloc (1, sizeof(MSTraceSeg))) )
    {
      ms_log (2, "mstl_addmsr(): Error allocating memory\n");
      return 0;
    }
  
  /* Populate MSTraceSeg */
  seg->starttime = msr->starttime;
  seg->endtime = endtime;
  seg->samprate = msr->samprate;
  seg->samplecnt = msr->samplecnt;
  seg->sampletype = msr->sampletype;
  seg->numsamples = msr->numsamples;
  
  /* Allocate space for and copy datasamples */
  if ( msr->datasamples && msr->numsamples )
    {
      samplesize = ms_samplesize (msr->sampletype);
      
      if ( ! (seg->datasamples = malloc ((size_t) (samplesize * msr->numsamples))) )
	{
	  ms_log (2, "mstl_msr2seg(): Error allocating memory\n");
	  return 0;
	}
      
      /* Copy data samples from MSRecord to MSTraceSeg */
      memcpy (seg->datasamples, msr->datasamples, (size_t) (samplesize * msr->numsamples));
    }
  
  return seg;
} /* End of mstl_msr2seg() */


/***************************************************************************
 * mstl_addmsrtoseg:
 *
 * Add data coverage from a MSRecord structure to a MSTraceSeg structure.
 *
 * Data coverage is added to the beginning or end of MSTraceSeg
 * according to the whence flag:
 * 1 : add coverage to the end
 * 2 : add coverage to the beginninig
 *
 * Return a pointer to a MSTraceSeg otherwise 0 on error.
 ***************************************************************************/
MSTraceSeg *
mstl_addmsrtoseg (MSTraceSeg *seg, MSRecord *msr, hptime_t endtime, flag whence)
{
  int samplesize = 0;
  void *newdatasamples;
  
  if ( ! seg || ! msr )
    return 0;

  /* Allocate more memory for data samples if included */
  if ( msr->datasamples && msr->numsamples > 0 )
    {
      if ( msr->sampletype != seg->sampletype )
	{
	  ms_log (2, "mstl_addmsrtoseg(): MSRecord sample type (%c) does not match segment sample type (%c)\n",
		  msr->sampletype, seg->sampletype);
	  return 0;
	}
      
      if ( ! (samplesize = ms_samplesize (msr->sampletype)) )
	{
	  ms_log (2, "mstl_addmsrtoseg(): Unknown sample size for sample type: %c\n", msr->sampletype);
	  return 0;
	}
      
      if ( ! (newdatasamples = realloc (seg->datasamples, (size_t)((seg->numsamples + msr->numsamples) * samplesize))) )
	{
	  ms_log (2, "mstl_addmsrtoseg(): Error allocating memory\n");
	  return 0;
	}
      
      seg->datasamples = newdatasamples;
    }
  
  /* Add coverage to end of segment */
  if ( whence == 1 )
    {
      seg->endtime = endtime;
      seg->samplecnt += msr->samplecnt;
      
      if ( msr->datasamples && msr->numsamples > 0 )
	{
	  memcpy ((char *)seg->datasamples + (seg->numsamples * samplesize),
                  msr->datasamples,
                  (size_t) (msr->numsamples * samplesize));
	  
	  seg->numsamples += msr->numsamples;
	}
    }
  /* Add coverage to beginning of segment */
  else if ( whence == 2 )
    {
      seg->starttime = msr->starttime;
      seg->samplecnt += msr->samplecnt;
      
      if ( msr->datasamples && msr->numsamples > 0 )
	{
	  memmove ((char *)seg->datasamples + (msr->numsamples * samplesize),
		   seg->datasamples,
		   (size_t) (seg->numsamples * samplesize));
	  
	  memcpy (seg->datasamples,
                  msr->datasamples,
                  (size_t) (msr->numsamples * samplesize));
	  
	  seg->numsamples += msr->numsamples;
	}
    }
  else
    {
      ms_log (2, "mstl_addmsrtoseg(): unrecognized whence value: %d\n", whence);
      return 0;
    }
  
  return seg;
} /* End of mstl_addmsrtoseg() */


/***************************************************************************
 * mstl_addsegtoseg:
 *
 * Add data coverage from seg2 to seg1.
 *
 * Return a pointer to a seg1 otherwise 0 on error.
 ***************************************************************************/
MSTraceSeg *
mstl_addsegtoseg (MSTraceSeg *seg1, MSTraceSeg *seg2)
{
  int samplesize = 0;
  void *newdatasamples;
  
  if ( ! seg1 || ! seg2 )
    return 0;
  
  /* Allocate more memory for data samples if included */
  if ( seg2->datasamples && seg2->numsamples > 0 )
    {
      if ( seg2->sampletype != seg1->sampletype )
	{
	  ms_log (2, "mstl_addsegtoseg(): MSTraceSeg sample types do not match (%c and %c)\n",
		  seg1->sampletype, seg2->sampletype);
	  return 0;
	}
      
      if ( ! (samplesize = ms_samplesize (seg1->sampletype)) )
	{
	  ms_log (2, "mstl_addsegtoseg(): Unknown sample size for sample type: %c\n", seg1->sampletype);
	  return 0;
	}
      
      if ( ! (newdatasamples = realloc (seg1->datasamples, (size_t) ((seg1->numsamples + seg2->numsamples) * samplesize))) )
	{
	  ms_log (2, "mstl_addsegtoseg(): Error allocating memory\n");
	  return 0;
	}
      
      seg1->datasamples = newdatasamples;
    }
  
  /* Add seg2 coverage to end of seg1 */
  seg1->endtime = seg2->endtime;
  seg1->samplecnt += seg2->samplecnt;
  
  if ( seg2->datasamples && seg2->numsamples > 0 )
    {
      memcpy ((char *)seg1->datasamples + (seg1->numsamples * samplesize),
	      seg2->datasamples,
	      (size_t) (seg2->numsamples * samplesize));
      
      seg1->numsamples += seg2->numsamples;
    }
  
  return seg1;
} /* End of mstl_addsegtoseg() */


/***************************************************************************
 * mstl_printtracelist:
 *
 * Print trace list summary information for the specified MSTraceList.
 *
 * By default only print the srcname, starttime and endtime for each
 * trace.  If details is greater than 0 include the sample rate,
 * number of samples and a total trace count.  If gaps is greater than
 * 0 and the previous trace matches (srcname & samprate) include the
 * gap between the endtime of the last trace and the starttime of the
 * current trace.
 *
 * The timeformat flag can either be:
 * 0 : SEED time format (year, day-of-year, hour, min, sec)
 * 1 : ISO time format (year, month, day, hour, min, sec)
 * 2 : Epoch time, seconds since the epoch
 ***************************************************************************/
void
mstl_printtracelist ( MSTraceList *mstl, flag timeformat,
		      flag details, flag gaps )
{
  MSTraceID *id = 0;
  MSTraceSeg *seg = 0;
  char stime[30];
  char etime[30];
  char gapstr[20];
  flag nogap;
  double gap;
  double delta;
  int tracecnt = 0;
  int segcnt = 0;
  
  if ( ! mstl )
    {
      return;
    }
  
  /* Print out the appropriate header */
  if ( details > 0 && gaps > 0 )
    ms_log (0, "   Source                Start sample             End sample        Gap  Hz  Samples\n");
  else if ( details <= 0 && gaps > 0 )
    ms_log (0, "   Source                Start sample             End sample        Gap\n");
  else if ( details > 0 && gaps <= 0 )
    ms_log (0, "   Source                Start sample             End sample        Hz  Samples\n");
  else
    ms_log (0, "   Source                Start sample             End sample\n");
  
  /* Loop through trace list */
  id = mstl->traces;  
  while ( id )
    {
      /* Loop through segment list */
      seg = id->first;
      while ( seg )
	{
	  /* Create formatted time strings */
	  if ( timeformat == 2 )
	    {
	      snprintf (stime, sizeof(stime), "%.6f", (double) MS_HPTIME2EPOCH(seg->starttime) );
	      snprintf (etime, sizeof(etime), "%.6f", (double) MS_HPTIME2EPOCH(seg->endtime) );
	    }
	  else if ( timeformat == 1 )
	    {
	      if ( ms_hptime2isotimestr (seg->starttime, stime, 1) == NULL )
		ms_log (2, "Cannot convert trace start time for %s\n", id->srcname);
	      
	      if ( ms_hptime2isotimestr (seg->endtime, etime, 1) == NULL )
		ms_log (2, "Cannot convert trace end time for %s\n", id->srcname);
	    }
	  else
	    {
	      if ( ms_hptime2seedtimestr (seg->starttime, stime, 1) == NULL )
		ms_log (2, "Cannot convert trace start time for %s\n", id->srcname);
	      
	      if ( ms_hptime2seedtimestr (seg->endtime, etime, 1) == NULL )
		ms_log (2, "Cannot convert trace end time for %s\n", id->srcname);
	    }
	  
	  /* Print segment info at varying levels */
	  if ( gaps > 0 )
	    {
	      gap = 0.0;
	      nogap = 0;
	      
	      if ( seg->prev )
		gap = (double) (seg->starttime - seg->prev->endtime) / HPTMODULUS;
	      else
		nogap = 1;
	      
	      /* Check that any overlap is not larger than the trace coverage */
	      if ( gap < 0.0 )
		{
		  delta = ( seg->samprate ) ? (1.0 / seg->samprate) : 0.0;
		  
		  if ( (gap * -1.0) > (((double)(seg->endtime - seg->starttime)/HPTMODULUS) + delta) )
		    gap = -(((double)(seg->endtime - seg->starttime)/HPTMODULUS) + delta);
		}
	      
	      /* Fix up gap display */
	      if ( nogap )
		snprintf (gapstr, sizeof(gapstr), " == ");
	      else if ( gap >= 86400.0 || gap <= -86400.0 )
		snprintf (gapstr, sizeof(gapstr), "%-3.1fd", (gap / 86400));
	      else if ( gap >= 3600.0 || gap <= -3600.0 )
		snprintf (gapstr, sizeof(gapstr), "%-3.1fh", (gap / 3600));
	      else if ( gap == 0.0 )
		snprintf (gapstr, sizeof(gapstr), "-0  ");
	      else
		snprintf (gapstr, sizeof(gapstr), "%-4.4g", gap);
	      
	      if ( details <= 0 )
		ms_log (0, "%-17s %-24s %-24s %-4s\n",
			id->srcname, stime, etime, gapstr);
	      else
		ms_log (0, "%-17s %-24s %-24s %-s %-3.3g %-lld\n",
			id->srcname, stime, etime, gapstr, seg->samprate, (long long int)seg->samplecnt);
	    }
	  else if ( details > 0 && gaps <= 0 )
	    ms_log (0, "%-17s %-24s %-24s %-3.3g %-lld\n",
		    id->srcname, stime, etime, seg->samprate, (long long int)seg->samplecnt);
	  else
	    ms_log (0, "%-17s %-24s %-24s\n", id->srcname, stime, etime);
	  
	  segcnt++;
	  seg = seg->next;
	}
      
      tracecnt++;
      id = id->next;
    }
  
  if ( tracecnt != mstl->numtraces )
    ms_log (2, "mstl_printtracelist(): number of traces in trace list is inconsistent\n");
  
  if ( details > 0 )
    ms_log (0, "Total: %d trace(s) with %d segment(s)\n", tracecnt, segcnt);
  
  return;
}  /* End of mstl_printtracelist() */


/***************************************************************************
 * mstl_printsynclist:
 *
 * Print SYNC trace list summary information for the specified MSTraceList.
 *
 * The SYNC header line will be created using the supplied dccid, if
 * the pointer is NULL the string "DCC" will be used instead.
 *
 * If the subsecond flag is true the segment start and end times will
 * include subsecond precision, otherwise they will be truncated to
 * integer seconds.
 *
 ***************************************************************************/
void
mstl_printsynclist ( MSTraceList *mstl, char *dccid, flag subsecond )
{
  MSTraceID *id = 0;
  MSTraceSeg *seg = 0;
  char starttime[30];
  char endtime[30];
  char yearday[10];
  time_t now;
  struct tm *nt;
  
  if ( ! mstl )
    {
      return;
    }
  
  /* Generate current time stamp */
  now = time (NULL);
  nt = localtime ( &now ); nt->tm_year += 1900; nt->tm_yday += 1;
  snprintf ( yearday, sizeof(yearday), "%04d,%03d", nt->tm_year, nt->tm_yday);
  
  /* Print SYNC header line */
  ms_log (0, "%s|%s\n", (dccid)?dccid:"DCC", yearday);
  
  /* Loop through trace list */
  id = mstl->traces;  
  while ( id )
    {
      /* Loop through segment list */
      seg = id->first;
      while ( seg )
	{
	  ms_hptime2seedtimestr (seg->starttime, starttime, subsecond);
	  ms_hptime2seedtimestr (seg->endtime, endtime, subsecond);
	  
	  /* Print SYNC line */
	  ms_log (0, "%s|%s|%s|%s|%s|%s||%.10g|%lld|||||||%s\n",
		  id->network, id->station, id->location, id->channel,
		  starttime, endtime, seg->samprate, (long long int)seg->samplecnt,
		  yearday);
	  
	  seg = seg->next;
	}
      
      id = id->next;
    }
  
  return;
}  /* End of mstl_printsynclist() */


/***************************************************************************
 * mstl_printgaplist:
 *
 * Print gap/overlap list summary information for the specified
 * MSTraceList.  Overlaps are printed as negative gaps.
 *
 * If mingap and maxgap are not NULL their values will be enforced and
 * only gaps/overlaps matching their implied criteria will be printed.
 *
 * The timeformat flag can either be:
 * 0 : SEED time format (year, day-of-year, hour, min, sec)
 * 1 : ISO time format (year, month, day, hour, min, sec)
 * 2 : Epoch time, seconds since the epoch
 ***************************************************************************/
void
mstl_printgaplist (MSTraceList *mstl, flag timeformat,
		   double *mingap, double *maxgap)
{
  MSTraceID *id = 0;
  MSTraceSeg *seg = 0;

  char time1[30], time2[30];
  char gapstr[30];
  double gap;
  double delta;
  double nsamples;
  flag printflag;
  int gapcnt = 0;
  
  if ( ! mstl )
    return;
  
  if ( ! mstl->traces )
    return;
  
  ms_log (0, "   Source                Last Sample              Next Sample       Gap  Samples\n");
  
  id = mstl->traces;
  while ( id )
    {
      seg = id->first;
      while ( seg->next )
	{
	  /* Skip segments with 0 sample rate, usually from SOH records */
	  if ( seg->samprate == 0.0 )
	    {
	      seg = seg->next;
	      continue;
	    }
	  
	  gap = (double) (seg->next->starttime - seg->endtime) / HPTMODULUS;
	  
	  /* Check that any overlap is not larger than the trace coverage */
	  if ( gap < 0.0 )
	    {
	      delta = ( seg->next->samprate ) ? (1.0 / seg->next->samprate) : 0.0;
	      
	      if ( (gap * -1.0) > (((double)(seg->next->endtime - seg->next->starttime)/HPTMODULUS) + delta) )
		gap = -(((double)(seg->next->endtime - seg->next->starttime)/HPTMODULUS) + delta);
	    }
	  
	  printflag = 1;
	      
	  /* Check gap/overlap criteria */
	  if ( mingap )
	    if ( gap < *mingap )
	      printflag = 0;
	  
	  if ( maxgap )
	    if ( gap > *maxgap )
	      printflag = 0;
	  
	  if ( printflag )
	    {
	      nsamples = ms_dabs(gap) * seg->samprate;
		  
	      if ( gap > 0.0 )
		nsamples -= 1.0;
	      else
		nsamples += 1.0;
	      
	      /* Fix up gap display */
	      if ( gap >= 86400.0 || gap <= -86400.0 )
		snprintf (gapstr, sizeof(gapstr), "%-3.1fd", (gap / 86400));
	      else if ( gap >= 3600.0 || gap <= -3600.0 )
		snprintf (gapstr, sizeof(gapstr), "%-3.1fh", (gap / 3600));
	      else if ( gap == 0.0 )
		snprintf (gapstr, sizeof(gapstr), "-0  ");
	      else
		snprintf (gapstr, sizeof(gapstr), "%-4.4g", gap);
	      
	      /* Create formatted time strings */
	      if ( timeformat == 2 )
		{
		  snprintf (time1, sizeof(time1), "%.6f", (double) MS_HPTIME2EPOCH(seg->endtime) );
		  snprintf (time2, sizeof(time2), "%.6f", (double) MS_HPTIME2EPOCH(seg->next->starttime) );
		}
	      else if ( timeformat == 1 )
		{
		  if ( ms_hptime2isotimestr (seg->endtime, time1, 1) == NULL )
		    ms_log (2, "Cannot convert trace end time for %s\n", id->srcname);
		  
		  if ( ms_hptime2isotimestr (seg->next->starttime, time2, 1) == NULL )
		    ms_log (2, "Cannot convert next trace start time for %s\n", id->srcname);
		}
	      else
		{
		  if ( ms_hptime2seedtimestr (seg->endtime, time1, 1) == NULL )
		    ms_log (2, "Cannot convert trace end time for %s\n", id->srcname);
		  
		  if ( ms_hptime2seedtimestr (seg->next->starttime, time2, 1) == NULL )
		    ms_log (2, "Cannot convert next trace start time for %s\n", id->srcname);
		}
	      
	      ms_log (0, "%-17s %-24s %-24s %-4s %-.8g\n",
		      id->srcname, time1, time2, gapstr, nsamples);
	      
	      gapcnt++;
	    }
	      
	  seg = seg->next;
	}
      
      id = id->next;
    }
  
  ms_log (0, "Total: %d gap(s)\n", gapcnt);
  
  return;
}  /* End of mstl_printgaplist() */
/***************************************************************************
 * traceutils.c:
 *
 * Generic routines to handle Traces.
 *
 * Written by Chad Trabant, IRIS Data Management Center
 *
 * modified: 2012.105
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "libmseed.h"

static int mst_groupsort_cmp ( MSTrace *mst1, MSTrace *mst2, flag quality );


/***************************************************************************
 * mst_init:
 *
 * Initialize and return a MSTrace struct, allocating memory if needed.
 * If the specified MSTrace includes data samples they will be freed.
 *
 * Returns a pointer to a MSTrace struct on success or NULL on error.
 ***************************************************************************/
MSTrace *
mst_init ( MSTrace *mst )
{
  /* Free datasamples, prvtptr and stream state if present */
  if ( mst )
    {
      if ( mst->datasamples )
	free (mst->datasamples);

      if ( mst->prvtptr )
	free (mst->prvtptr);
      
      if ( mst->ststate )
        free (mst->ststate);
    }
  else
    {
      mst = (MSTrace *) malloc (sizeof(MSTrace));
    }
  
  if ( mst == NULL )
    {
      ms_log (2, "mst_init(): Cannot allocate memory\n");
      return NULL;
    }
  
  memset (mst, 0, sizeof (MSTrace));
 
  return mst;
} /* End of mst_init() */


/***************************************************************************
 * mst_free:
 *
 * Free all memory associated with a MSTrace struct and set the pointer
 * to 0.
 ***************************************************************************/
void
mst_free ( MSTrace **ppmst )
{
  if ( ppmst && *ppmst )
    {
      /* Free datasamples if present */
      if ( (*ppmst)->datasamples )
        free ((*ppmst)->datasamples);

      /* Free private memory if present */
      if ( (*ppmst)->prvtptr )
        free ((*ppmst)->prvtptr);
      
      /* Free stream processing state if present */
      if ( (*ppmst)->ststate )
        free ((*ppmst)->ststate);

      free (*ppmst);
      
      *ppmst = 0;
    }
} /* End of mst_free() */


/***************************************************************************
 * mst_initgroup:
 *
 * Initialize and return a MSTraceGroup struct, allocating memory if
 * needed.  If the supplied MSTraceGroup is not NULL any associated
 * memory it will be freed.
 *
 * Returns a pointer to a MSTraceGroup struct on success or NULL on error.
 ***************************************************************************/
MSTraceGroup *
mst_initgroup ( MSTraceGroup *mstg )
{
  MSTrace *mst = 0;
  MSTrace *next = 0;
  
  if ( mstg )
    {
      mst = mstg->traces;
      
      while ( mst )
	{
	  next = mst->next;
	  mst_free (&mst);
	  mst = next;
	}
    }
  else
    {
      mstg = (MSTraceGroup *) malloc (sizeof(MSTraceGroup));
    }
  
  if ( mstg == NULL )
    {
      ms_log (2, "mst_initgroup(): Cannot allocate memory\n");
      return NULL;
    }
  
  memset (mstg, 0, sizeof (MSTraceGroup));
  
  return mstg;
} /* End of mst_initgroup() */


/***************************************************************************
 * mst_freegroup:
 *
 * Free all memory associated with a MSTraceGroup struct and set the
 * pointer to 0.
 ***************************************************************************/
void
mst_freegroup ( MSTraceGroup **ppmstg )
{
  MSTrace *mst = 0;
  MSTrace *next = 0;
  
  if ( *ppmstg )
    {
      mst = (*ppmstg)->traces;
      
      while ( mst )
	{
	  next = mst->next;
	  mst_free (&mst);
	  mst = next;
	}
      
      free (*ppmstg);
      
      *ppmstg = 0;
    }
} /* End of mst_freegroup() */


/***************************************************************************
 * mst_findmatch:
 *
 * Traverse the MSTrace chain starting at 'startmst' until a MSTrace
 * is found that matches the given name identifiers.  If the dataquality
 * byte is not 0 it must also match.
 *
 * Return a pointer a matching MSTrace otherwise 0 if no match found.
 ***************************************************************************/
MSTrace *
mst_findmatch ( MSTrace *startmst, char dataquality,
		char *network, char *station, char *location, char *channel )
{
  int idx;
  
  if ( ! startmst )
    return 0;
  
  while ( startmst )
    {
      if ( dataquality && dataquality != startmst->dataquality )
	{
	  startmst = startmst->next;
	  continue;
	}

      /* Compare network */
      idx = 0;
      while ( network[idx] == startmst->network[idx] )
	{
	  if ( network[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( network[idx] != '\0' || startmst->network[idx] != '\0' )
	{
	  startmst = startmst->next;
	  continue;
	}
      /* Compare station */
      idx = 0;
      while ( station[idx] == startmst->station[idx] )
	{
	  if ( station[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( station[idx] != '\0' || startmst->station[idx] != '\0' )
	{
	  startmst = startmst->next;
	  continue;
	}
      /* Compare location */
      idx = 0;
      while ( location[idx] == startmst->location[idx] )
	{
	  if ( location[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( location[idx] != '\0' || startmst->location[idx] != '\0' )
	{
	  startmst = startmst->next;
	  continue;
	}
      /* Compare channel */
      idx = 0;
      while ( channel[idx] == startmst->channel[idx] )
	{
	  if ( channel[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( channel[idx] != '\0' || startmst->channel[idx] != '\0' )
	{
	  startmst = startmst->next;
	  continue;
	}
      
      /* A match was found if we made it this far */
      break;
    }
  
  return startmst;
} /* End of mst_findmatch() */


/***************************************************************************
 * mst_findadjacent:
 *
 * Find a MSTrace in a MSTraceGroup matching the given name
 * identifiers, samplerate and is adjacent with a time span.  If the
 * dataquality byte is not 0 it must also match.
 *
 * The time tolerance and sample rate tolerance are used to determine
 * if traces abut.  If timetol is -1.0 the default tolerance of 1/2
 * the sample period will be used.  If samprratetol is -1.0 the
 * default tolerance check of abs(1-sr1/sr2) < 0.0001 is used (defined
 * in libmseed.h).  If timetol or sampratetol is -2.0 the respective
 * tolerance check will not be performed.
 *
 * The 'whence' flag will be set, when a matching MSTrace is found, to
 * indicate where the indicated time span is adjacent to the MSTrace
 * using the following values:
 * 1: time span fits at the end of the MSTrace
 * 2: time span fits at the beginning of the MSTrace
 *
 * Return a pointer a matching MSTrace and set the 'whence' flag
 * otherwise 0 if no match found.
 ***************************************************************************/
MSTrace *
mst_findadjacent ( MSTraceGroup *mstg, flag *whence, char dataquality,
		   char *network, char *station, char *location, char *channel,
		   double samprate, double sampratetol,
		   hptime_t starttime, hptime_t endtime, double timetol )
{
  MSTrace *mst = 0;
  hptime_t pregap;
  hptime_t postgap;
  hptime_t hpdelta;
  hptime_t hptimetol = 0;
  hptime_t nhptimetol = 0;
  int idx;
  
  if ( ! mstg )
    return 0;
  
  *whence = 0;
  
  /* Calculate high-precision sample period */
  hpdelta = (hptime_t)(( samprate ) ? (HPTMODULUS / samprate) : 0.0);
  
  /* Calculate high-precision time tolerance */
  if ( timetol == -1.0 )
    hptimetol = (hptime_t) (0.5 * hpdelta);   /* Default time tolerance is 1/2 sample period */
  else if ( timetol >= 0.0 )
    hptimetol = (hptime_t) (timetol * HPTMODULUS);
  
  nhptimetol = ( hptimetol ) ? -hptimetol : 0;
  
  mst = mstg->traces;
  
  while ( mst )
    {
      /* post/pregap are negative when the record overlaps the trace
       * segment and positive when there is a time gap. */
      postgap = starttime - mst->endtime - hpdelta;
      
      pregap = mst->starttime - endtime - hpdelta;
      
      /* If not checking the time tolerance decide if beginning or end is a better fit */
      if ( timetol == -2.0 )
	{
	  if ( ms_dabs((double)postgap) < ms_dabs((double)pregap) )
	    *whence = 1;
	  else
	    *whence = 2;
	}
      else
	{
	  if ( postgap <= hptimetol && postgap >= nhptimetol )
	    {
              /* Span fits right at the end of the trace */
	      *whence = 1;
	    }
	  else if ( pregap <= hptimetol && pregap >= nhptimetol )
	    {
              /* Span fits right at the beginning of the trace */
	      *whence = 2;
	    }
          else
            {
	      /* Span does not fit with this Trace */
	      mst = mst->next;
	      continue;
            }
	}
      
      /* Perform samprate tolerance check if requested */
      if ( sampratetol != -2.0 )
	{ 
	  /* Perform default samprate tolerance check if requested */
	  if ( sampratetol == -1.0 )
	    {
	      if ( ! MS_ISRATETOLERABLE (samprate, mst->samprate) )
		{
		  mst = mst->next;
		  continue;
		}
	    }
	  /* Otherwise check against the specified sample rate tolerance */
	  else if ( ms_dabs(samprate - mst->samprate) > sampratetol )
	    {
	      mst = mst->next;
	      continue;
	    }
	}

       /* Compare data qualities */
       if ( dataquality && dataquality != mst->dataquality )
	{
	  mst = mst->next;
	  continue;
	}

      /* Compare network */
      idx = 0;
      while ( network[idx] == mst->network[idx] )
	{
	  if ( network[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( network[idx] != '\0' || mst->network[idx] != '\0' )
	{
	  mst = mst->next;
	  continue;
	}
      /* Compare station */
      idx = 0;
      while ( station[idx] == mst->station[idx] )
	{
	  if ( station[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( station[idx] != '\0' || mst->station[idx] != '\0' )
	{
	  mst = mst->next;
	  continue;
	}
      /* Compare location */
      idx = 0;
      while ( location[idx] == mst->location[idx] )
	{
	  if ( location[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( location[idx] != '\0' || mst->location[idx] != '\0' )
	{
	  mst = mst->next;
	  continue;
	}
      /* Compare channel */
      idx = 0;
      while ( channel[idx] == mst->channel[idx] )
	{
	  if ( channel[idx] == '\0' )
	    break;
	  idx++;
	}
      if ( channel[idx] != '\0' || mst->channel[idx] != '\0' )
	{
	  mst = mst->next;
	  continue;
	}
      
      /* A match was found if we made it this far */
      break;
    }
 
  return mst;
} /* End of mst_findadjacent() */


/***************************************************************************
 * mst_addmsr:
 *
 * Add MSRecord time coverage to a MSTrace.  The start or end time will
 * be updated and samples will be copied if they exist.  No checking
 * is done to verify that the record matches the trace in any way.
 *
 * If whence is 1 the coverage will be added at the end of the trace,
 * whereas if whence is 2 the coverage will be added at the beginning
 * of the trace.
 *
 * Return 0 on success and -1 on error.
 ***************************************************************************/
int
mst_addmsr ( MSTrace *mst, MSRecord *msr, flag whence )
{
  int samplesize = 0;
  
  if ( ! mst || ! msr )
    return -1;
  
  /* Reallocate data sample buffer if samples are present */
  if ( msr->datasamples && msr->numsamples >= 0 )
    {
      /* Check that the entire record was decompressed */
      if ( msr->samplecnt != msr->numsamples )
	{
	  ms_log (2, "mst_addmsr(): Sample counts do not match, record not fully decompressed?\n");
	  ms_log (2, "  The sample buffer will likely contain a discontinuity.\n");
	}

      if ( (samplesize = ms_samplesize(msr->sampletype)) == 0 )
	{
	  ms_log (2, "mst_addmsr(): Unrecognized sample type: '%c'\n",
		  msr->sampletype);
	  return -1;
	}
    
      if ( msr->sampletype != mst->sampletype )
	{
	  ms_log (2, "mst_addmsr(): Mismatched sample type, '%c' and '%c'\n",
		  msr->sampletype, mst->sampletype);
	  return -1;
	}
      
      mst->datasamples = realloc (mst->datasamples,
				  (size_t) (mst->numsamples * samplesize + msr->numsamples * samplesize) );
      
      if ( mst->datasamples == NULL )
	{
	  ms_log (2, "mst_addmsr(): Cannot allocate memory\n");
	  return -1;
	}
    }
  
  /* Add samples at end of trace */
  if ( whence == 1 )
    {
      if ( msr->datasamples && msr->numsamples >= 0 )
	{
	  memcpy ((char *)mst->datasamples + (mst->numsamples * samplesize),
		  msr->datasamples,
		  (size_t) (msr->numsamples * samplesize));
	  
	  mst->numsamples += msr->numsamples;
	}
      
      mst->endtime = msr_endtime (msr);
      
      if ( mst->endtime == HPTERROR )
	{
	  ms_log (2, "mst_addmsr(): Error calculating record end time\n");
	  return -1;
	}
    }
  
  /* Add samples at the beginning of trace */
  else if ( whence == 2 )
    {
      if ( msr->datasamples && msr->numsamples >= 0 )
	{
	  /* Move any samples to end of buffer */
	  if ( mst->numsamples > 0 )
	    {
	      memmove ((char *)mst->datasamples + (msr->numsamples * samplesize),
		       mst->datasamples,
		       (size_t) (mst->numsamples * samplesize));
	    }

	  memcpy (mst->datasamples,
		  msr->datasamples,
		  (size_t) (msr->numsamples * samplesize));
	  
	  mst->numsamples += msr->numsamples;
	}
      
      mst->starttime = msr->starttime;
    }
  
  /* If two different data qualities reset the MSTrace.dataquality to 0 */
  if ( mst->dataquality && msr->dataquality && mst->dataquality != msr->dataquality )
    mst->dataquality = 0;
  
  /* Update MSTrace sample count */
  mst->samplecnt += msr->samplecnt;
  
  return 0;
} /* End of mst_addmsr() */


/***************************************************************************
 * mst_addspan:
 *
 * Add a time span to a MSTrace.  The start or end time will be updated
 * and samples will be copied if they are provided.  No checking is done to
 * verify that the record matches the trace in any way.
 *
 * If whence is 1 the coverage will be added at the end of the trace,
 * whereas if whence is 2 the coverage will be added at the beginning
 * of the trace.
 *
 * Return 0 on success and -1 on error.
 ***************************************************************************/
int
mst_addspan ( MSTrace *mst, hptime_t starttime, hptime_t endtime,
	      void *datasamples, int64_t numsamples, char sampletype,
	      flag whence )
{
  int samplesize = 0;
  
  if ( ! mst )
    return -1;
  
  if ( datasamples && numsamples > 0 )
    {
      if ( (samplesize = ms_samplesize(sampletype)) == 0 )
	{
	  ms_log (2, "mst_addspan(): Unrecognized sample type: '%c'\n",
		  sampletype);
	  return -1;
	}
      
      if ( sampletype != mst->sampletype )
	{
	  ms_log (2, "mst_addspan(): Mismatched sample type, '%c' and '%c'\n",
		  sampletype, mst->sampletype);
	  return -1;
	}
      
      mst->datasamples = realloc (mst->datasamples,
				  (size_t) (mst->numsamples * samplesize + numsamples * samplesize));
      
      if ( mst->datasamples == NULL )
	{
	  ms_log (2, "mst_addspan(): Cannot allocate memory\n");
	  return -1;
	}
    }
  
  /* Add samples at end of trace */
  if ( whence == 1 )
    {
      if ( datasamples && numsamples > 0 )
	{
	  memcpy ((char *)mst->datasamples + (mst->numsamples * samplesize),
		  datasamples,
		  (size_t) (numsamples * samplesize));
	  
	  mst->numsamples += numsamples;
	}
      
      mst->endtime = endtime;      
    }
  
  /* Add samples at the beginning of trace */
  else if ( whence == 2 )
    {
      if ( datasamples && numsamples > 0 )
	{
	  /* Move any samples to end of buffer */
	  if ( mst->numsamples > 0 )
	    {
	      memmove ((char *)mst->datasamples + (numsamples * samplesize),
		       mst->datasamples,
		       (size_t) (mst->numsamples * samplesize));
	    }
	  
	  memcpy (mst->datasamples,
		  datasamples,
		  (size_t) (numsamples * samplesize));
	  
	  mst->numsamples += numsamples;
	}
      
      mst->starttime = starttime;
    }
  
  /* Update MSTrace sample count */
  if ( numsamples > 0 )
    mst->samplecnt += numsamples;
  
  return 0;
} /* End of mst_addspan() */


/***************************************************************************
 * mst_addmsrtogroup:
 *
 * Add data samples from a MSRecord to a MSTrace in a MSTraceGroup by
 * searching the group for the approriate MSTrace and either adding data
 * to it or creating a new MSTrace if no match found.
 *
 * Matching traces are found using the mst_findadjacent() routine.  If
 * the dataquality flag is true the data quality bytes must also match
 * otherwise they are ignored.
 *
 * Return a pointer to the MSTrace updated or 0 on error.
 ***************************************************************************/
MSTrace *
mst_addmsrtogroup ( MSTraceGroup *mstg, MSRecord *msr, flag dataquality,
		    double timetol, double sampratetol )
{
  MSTrace *mst = 0;
  hptime_t endtime;
  flag whence;
  char dq;

  if ( ! mstg || ! msr )
    return 0;
  
  dq = ( dataquality ) ? msr->dataquality : 0;
  
  endtime = msr_endtime (msr);
  
  if ( endtime == HPTERROR )
    {
      ms_log (2, "mst_addmsrtogroup(): Error calculating record end time\n");
      return 0;
    }
  
  /* Find matching, time adjacent MSTrace */
  mst = mst_findadjacent (mstg, &whence, dq,
			  msr->network, msr->station, msr->location, msr->channel,
			  msr->samprate, sampratetol,
			  msr->starttime, endtime, timetol);
  
  /* If a match was found update it otherwise create a new MSTrace and
     add to end of MSTrace chain */
  if ( mst )
    {
      /* Records with no time coverage do not contribute to a trace */
      if ( msr->samplecnt <= 0 || msr->samprate <= 0.0 )
	return mst;
      
      if ( mst_addmsr (mst, msr, whence) )
	{
	  return 0;
	}
    }
  else
    {
      mst = mst_init (NULL);
      
      mst->dataquality = dq;
      
      strncpy (mst->network, msr->network, sizeof(mst->network));
      strncpy (mst->station, msr->station, sizeof(mst->station));
      strncpy (mst->location, msr->location, sizeof(mst->location));
      strncpy (mst->channel, msr->channel, sizeof(mst->channel));
      
      mst->starttime = msr->starttime;
      mst->samprate = msr->samprate;
      mst->sampletype = msr->sampletype;
      
      if ( mst_addmsr (mst, msr, 1) )
	{
	  mst_free (&mst);
	  return 0;
	}
      
      /* Link new MSTrace into the end of the chain */
      if ( ! mstg->traces )
	{
	  mstg->traces = mst;
	}
      else
	{
	  MSTrace *lasttrace = mstg->traces;
	  
	  while ( lasttrace->next )
	    lasttrace = lasttrace->next;
	  
	  lasttrace->next = mst;
	}
      
      mstg->numtraces++;
    }
  
  return mst;
}  /* End of mst_addmsrtogroup() */


/***************************************************************************
 * mst_addtracetogroup:
 *
 * Add a MSTrace to a MSTraceGroup at the end of the MSTrace chain.
 *
 * Return a pointer to the MSTrace added or 0 on error.
 ***************************************************************************/
MSTrace *
mst_addtracetogroup ( MSTraceGroup *mstg, MSTrace *mst )
{
  MSTrace *lasttrace;

  if ( ! mstg || ! mst )
    return 0;
  
  if ( ! mstg->traces )
    {
      mstg->traces = mst;
    }
  else
    {
      lasttrace = mstg->traces;
      
      while ( lasttrace->next )
	lasttrace = lasttrace->next;
      
      lasttrace->next = mst;
    }
  
  mst->next = 0;
  
  mstg->numtraces++;
  
  return mst;
} /* End of mst_addtracetogroup() */


/***************************************************************************
 * mst_groupheal:
 *
 * Check if traces in MSTraceGroup can be healed, if contiguous segments
 * belong together they will be merged.  This routine is only useful
 * if the trace group was assembled from segments out of time order
 * (e.g. a file of Mini-SEED records not in time order) but forming
 * contiguous time coverage.  The MSTraceGroup will be sorted using
 * mst_groupsort() before healing.
 *
 * The time tolerance and sample rate tolerance are used to determine
 * if the traces are indeed the same.  If timetol is -1.0 the default
 * tolerance of 1/2 the sample period will be used.  If samprratetol
 * is -1.0 the default tolerance check of abs(1-sr1/sr2) < 0.0001 is
 * used (defined in libmseed.h).
 *
 * Return number of trace mergings on success otherwise -1 on error.
 ***************************************************************************/
int
mst_groupheal ( MSTraceGroup *mstg, double timetol, double sampratetol )
{
  int mergings = 0;
  MSTrace *curtrace = 0;
  MSTrace *nexttrace = 0;
  MSTrace *searchtrace = 0;
  MSTrace *prevtrace = 0;
  int8_t merged = 0;
  double postgap, pregap, delta;
  
  if ( ! mstg )
    return -1;
  
  /* Sort MSTraceGroup before any healing */
  if ( mst_groupsort (mstg, 1) )
    return -1;
  
  curtrace = mstg->traces;
  
  while ( curtrace )
    {
      nexttrace = mstg->traces;
      prevtrace = mstg->traces;
      
      while ( nexttrace )
	{
	  searchtrace = nexttrace;
	  nexttrace = searchtrace->next;
	  
	  /* Do not process the same MSTrace we are trying to match */
	  if ( searchtrace == curtrace )
	    {
	      prevtrace = searchtrace;
	      continue;
	    }
	  
	  /* Check if this trace matches the curtrace */
	  if ( strcmp (searchtrace->network, curtrace->network) ||
	       strcmp (searchtrace->station, curtrace->station) ||
	       strcmp (searchtrace->location, curtrace->location) ||
	       strcmp (searchtrace->channel, curtrace->channel) )
	    {
	      prevtrace = searchtrace;
	      continue;
	    }
      	  
	  /* Perform default samprate tolerance check if requested */
	  if ( sampratetol == -1.0 )
	    {
	      if ( ! MS_ISRATETOLERABLE (searchtrace->samprate, curtrace->samprate) )
		{
		  prevtrace = searchtrace;
		  continue;
		}
	    }
	  /* Otherwise check against the specified sample rates tolerance */
	  else if ( ms_dabs(searchtrace->samprate - curtrace->samprate) > sampratetol )
	    {
	      prevtrace = searchtrace;
	      continue;
	    }
	  
	  merged = 0;
	  
	  /* post/pregap are negative when searchtrace overlaps curtrace
	   * segment and positive when there is a time gap.
	   */
	  delta = ( curtrace->samprate ) ? (1.0 / curtrace->samprate) : 0.0;
	  
	  postgap = ((double)(searchtrace->starttime - curtrace->endtime)/HPTMODULUS) - delta;
	  
	  pregap = ((double)(curtrace->starttime - searchtrace->endtime)/HPTMODULUS) - delta;
	  
	  /* Calculate default time tolerance (1/2 sample period) if needed */
	  if ( timetol == -1.0 )
	    timetol = 0.5 * delta;
	  
	  /* Fits right at the end of curtrace */
	  if ( ms_dabs(postgap) <= timetol )
	    {
	      /* Merge searchtrace with curtrace */
	      mst_addspan (curtrace, searchtrace->starttime, searchtrace->endtime,
			   searchtrace->datasamples, searchtrace->numsamples,
			   searchtrace->sampletype, 1);
	      
	      /* If no data is present, make sure sample count is updated */
	      if ( searchtrace->numsamples <= 0 )
		curtrace->samplecnt += searchtrace->samplecnt;
	      
	      /* If qualities do not match reset the indicator */
	      if (curtrace->dataquality != searchtrace->dataquality)
		curtrace->dataquality = 0;

	      merged = 1;
	    }
	  
	  /* Fits right at the beginning of curtrace */
	  else if ( ms_dabs(pregap) <= timetol )
	    {
	      /* Merge searchtrace with curtrace */
	      mst_addspan (curtrace, searchtrace->starttime, searchtrace->endtime,
			   searchtrace->datasamples, searchtrace->numsamples,
			   searchtrace->sampletype, 2);
	      
	      /* If no data is present, make sure sample count is updated */
	      if ( searchtrace->numsamples <= 0 )
		curtrace->samplecnt += searchtrace->samplecnt;
	      
	      /* If qualities do not match reset the indicator */
	      if (curtrace->dataquality != searchtrace->dataquality)
		curtrace->dataquality = 0;
	      
	      merged = 1;
	    }
	 
	  /* If searchtrace was merged with curtrace remove it from the chain */
	  if ( merged )
	    {
	      /* Re-link trace chain and free searchtrace */
	      if ( searchtrace == mstg->traces )
		mstg->traces = nexttrace;
	      else
		prevtrace->next = nexttrace;
	      
	      mst_free (&searchtrace);
	      
	      mstg->numtraces--;
	      mergings++;
	    }
	  else
	    {
	      prevtrace = searchtrace;
	    }
	}
      
      curtrace = curtrace->next;
    }

  return mergings;
}  /* End of mst_groupheal() */


/***************************************************************************
 * mst_groupsort:
 *
 * Sort a MSTraceGroup using a mergesort algorithm.  MSTrace entries
 * are compared using the mst_groupsort_cmp() function.
 *
 * The mergesort implementation was inspired by the listsort function
 * published and copyright 2001 by Simon Tatham.
 *
 * Return 0 on success and -1 on error.
 ***************************************************************************/
int
mst_groupsort ( MSTraceGroup *mstg, flag quality )
{
  MSTrace *p, *q, *e, *top, *tail;
  int nmerges;
  int insize, psize, qsize, i;
  
  if ( ! mstg )
    return -1;
  
  if ( ! mstg->traces )
    return 0;
  
  top = mstg->traces;
  insize = 1;
  
  for (;;)
    {
      p = top;
      top = NULL;
      tail = NULL;
      
      nmerges = 0;  /* count number of merges we do in this pass */
      
      while ( p )
        {
          nmerges++;  /* there exists a merge to be done */
	  
          /* step `insize' places along from p */
          q = p;
          psize = 0;
          for (i = 0; i < insize; i++)
            {
              psize++;
              q = q->next;
              if ( ! q )
                break;
            }
          
          /* if q hasn't fallen off end, we have two lists to merge */
          qsize = insize;
	  
          /* now we have two lists; merge them */
          while ( psize > 0 || (qsize > 0 && q) )
            {
              /* decide whether next element of merge comes from p or q */
              if ( psize == 0 )
                {  /* p is empty; e must come from q. */
                  e = q; q = q->next; qsize--;
                }
              else if ( qsize == 0 || ! q )
                {  /* q is empty; e must come from p. */
                  e = p; p = p->next; psize--;
                }
              else if ( mst_groupsort_cmp (p, q, quality) <= 0 )
                {  /* First element of p is lower (or same), e must come from p. */
                  e = p; p = p->next; psize--;
                }
              else
                {  /* First element of q is lower; e must come from q. */
                  e = q; q = q->next; qsize--;
                }
	      
              /* add the next element to the merged list */
              if ( tail )
                tail->next = e;
              else
                top = e;
	      
              tail = e;
            }
	  
          /* now p has stepped `insize' places along, and q has too */
          p = q;
        }
      
      tail->next = NULL;
      
      /* If we have done only one merge, we're finished. */
      if ( nmerges <= 1 )   /* allow for nmerges==0, the empty list case */
        {
          mstg->traces = top;
          
	  return 0;
        }
      
      /* Otherwise repeat, merging lists twice the size */
      insize *= 2;
    }
} /* End of mst_groupsort() */


/***************************************************************************
 * mst_groupsort_cmp:
 *
 * Compare two MSTrace entities for the purposes of sorting a
 * MSTraceGroup.  Criteria for MSTrace comparison are (in order of
 * testing): source name, start time, descending endtime (longest
 * trace first) and sample rate.
 *
 * Return 1 if mst1 is "greater" than mst2, otherwise return 0.
 ***************************************************************************/
static int
mst_groupsort_cmp ( MSTrace *mst1, MSTrace *mst2, flag quality )
{
  char src1[50], src2[50];
  int strcmpval;
  
  if ( ! mst1 || ! mst2 )
    return -1;
  
  mst_srcname (mst1, src1, quality);
  mst_srcname (mst2, src2, quality);
  
  strcmpval = strcmp (src1, src2);
  
  /* If the source names do not match make sure the "greater" string is 2nd,
   * otherwise, if source names do match, make sure the later start time is 2nd
   * otherwise, if start times match, make sure the earlier end time is 2nd
   * otherwise, if end times match, make sure the highest sample rate is 2nd
   */
  if ( strcmpval > 0 )
    {
      return 1;
    }
  else if ( strcmpval == 0 )
    {
      if ( mst1->starttime > mst2->starttime )
	{
	  return 1;
	}
      else if ( mst1->starttime == mst2->starttime )
	{
	  if ( mst1->endtime < mst2->endtime )
	    {
	      return 1;
	    }
	  else if ( mst1->endtime == mst2->endtime )
	    {
	      if ( ! MS_ISRATETOLERABLE (mst1->samprate, mst2->samprate) &&
		   mst1->samprate > mst2->samprate )
		{
		  return 1;
		}
	    }
	}
    }
  
  return 0;
} /* End of mst_groupsort_cmp() */


/***************************************************************************
 * mst_srcname:
 *
 * Generate a source name string for a specified MSTrace in the
 * format: 'NET_STA_LOC_CHAN[_QUAL]'.  The quality is added to the
 * srcname if the quality flag argument is 1 and mst->dataquality is
 * not zero.  The passed srcname must have enough room for the
 * resulting string.
 *
 * Returns a pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
mst_srcname (MSTrace *mst, char *srcname, flag quality)
{
  char *src = srcname;
  char *cp = srcname;
  
  if ( ! mst || ! srcname )
    return NULL;
  
  /* Build the source name string */
  cp = mst->network;
  while ( *cp ) { *src++ = *cp++; }
  *src++ = '_';
  cp = mst->station;
  while ( *cp ) { *src++ = *cp++; }  
  *src++ = '_';
  cp = mst->location;
  while ( *cp ) { *src++ = *cp++; }  
  *src++ = '_';
  cp = mst->channel;
  while ( *cp ) { *src++ = *cp++; }  
  
  if ( quality && mst->dataquality )
    {
      *src++ = '_';
      *src++ = mst->dataquality;
    }
  
  *src = '\0';
  
  return srcname;
} /* End of mst_srcname() */


/***************************************************************************
 * mst_printtracelist:
 *
 * Print trace list summary information for the specified MSTraceGroup.
 *
 * By default only print the srcname, starttime and endtime for each
 * trace.  If details is greater than 0 include the sample rate,
 * number of samples and a total trace count.  If gaps is greater than
 * 0 and the previous trace matches (srcname & samprate) include the
 * gap between the endtime of the last trace and the starttime of the
 * current trace.
 *
 * The timeformat flag can either be:
 * 0 : SEED time format (year, day-of-year, hour, min, sec)
 * 1 : ISO time format (year, month, day, hour, min, sec)
 * 2 : Epoch time, seconds since the epoch
 ***************************************************************************/
void
mst_printtracelist ( MSTraceGroup *mstg, flag timeformat,
		     flag details, flag gaps )
{
  MSTrace *mst = 0;
  char srcname[50];
  char prevsrcname[50];
  char stime[30];
  char etime[30];
  char gapstr[20];
  flag nogap;
  double gap;
  double delta;
  double prevsamprate;
  hptime_t prevendtime;
  int tracecnt = 0;
  
  if ( ! mstg )
    {
      return;
    }
  
  mst = mstg->traces;
  
  /* Print out the appropriate header */
  if ( details > 0 && gaps > 0 )
    ms_log (0, "   Source                Start sample             End sample        Gap  Hz  Samples\n");
  else if ( details <= 0 && gaps > 0 )
    ms_log (0, "   Source                Start sample             End sample        Gap\n");
  else if ( details > 0 && gaps <= 0 )
    ms_log (0, "   Source                Start sample             End sample        Hz  Samples\n");
  else
    ms_log (0, "   Source                Start sample             End sample\n");
  
  prevsrcname[0] = '\0';
  prevsamprate = -1.0;
  prevendtime = 0;
  
  while ( mst )
    {
      mst_srcname (mst, srcname, 1);
      
      /* Create formatted time strings */
      if ( timeformat == 2 )
	{
	  snprintf (stime, sizeof(stime), "%.6f", (double) MS_HPTIME2EPOCH(mst->starttime) );
	  snprintf (etime, sizeof(etime), "%.6f", (double) MS_HPTIME2EPOCH(mst->endtime) );
	}
      else if ( timeformat == 1 )
	{
	  if ( ms_hptime2isotimestr (mst->starttime, stime, 1) == NULL )
	    ms_log (2, "Cannot convert trace start time for %s\n", srcname);
	  
	  if ( ms_hptime2isotimestr (mst->endtime, etime, 1) == NULL )
	    ms_log (2, "Cannot convert trace end time for %s\n", srcname);
	}
      else
	{
	  if ( ms_hptime2seedtimestr (mst->starttime, stime, 1) == NULL )
	    ms_log (2, "Cannot convert trace start time for %s\n", srcname);
	  
	  if ( ms_hptime2seedtimestr (mst->endtime, etime, 1) == NULL )
	    ms_log (2, "Cannot convert trace end time for %s\n", srcname);
	}
      
      /* Print trace info at varying levels */
      if ( gaps > 0 )
	{
	  gap = 0.0;
	  nogap = 0;
	  
	  if ( ! strcmp (prevsrcname, srcname) && prevsamprate != -1.0 &&
	       MS_ISRATETOLERABLE (prevsamprate, mst->samprate) )
	    gap = (double) (mst->starttime - prevendtime) / HPTMODULUS;
	  else
	    nogap = 1;
	  
	  /* Check that any overlap is not larger than the trace coverage */
	  if ( gap < 0.0 )
	    {
	      delta = ( mst->samprate ) ? (1.0 / mst->samprate) : 0.0;
	      
	      if ( (gap * -1.0) > (((double)(mst->endtime - mst->starttime)/HPTMODULUS) + delta) )
		gap = -(((double)(mst->endtime - mst->starttime)/HPTMODULUS) + delta);
	    }
	  
	  /* Fix up gap display */
	  if ( nogap )
	    snprintf (gapstr, sizeof(gapstr), " == ");
	  else if ( gap >= 86400.0 || gap <= -86400.0 )
	    snprintf (gapstr, sizeof(gapstr), "%-3.1fd", (gap / 86400));
	  else if ( gap >= 3600.0 || gap <= -3600.0 )
	    snprintf (gapstr, sizeof(gapstr), "%-3.1fh", (gap / 3600));
	  else if ( gap == 0.0 )
	    snprintf (gapstr, sizeof(gapstr), "-0  ");
	  else
	    snprintf (gapstr, sizeof(gapstr), "%-4.4g", gap);
	  
	  if ( details <= 0 )
	    ms_log (0, "%-17s %-24s %-24s %-4s\n",
		    srcname, stime, etime, gapstr);
	  else
	    ms_log (0, "%-17s %-24s %-24s %-s %-3.3g %-lld\n",
		    srcname, stime, etime, gapstr, mst->samprate, (long long int)mst->samplecnt);
	}
      else if ( details > 0 && gaps <= 0 )
	ms_log (0, "%-17s %-24s %-24s %-3.3g %-lld\n",
		srcname, stime, etime, mst->samprate, (long long int)mst->samplecnt);
      else
	ms_log (0, "%-17s %-24s %-24s\n", srcname, stime, etime);
      
      if ( gaps > 0 )
	{
	  strcpy (prevsrcname, srcname);
	  prevsamprate = mst->samprate;
	  prevendtime = mst->endtime;
	}
      
      tracecnt++;
      mst = mst->next;
    }

  if ( tracecnt != mstg->numtraces )
    ms_log (2, "mst_printtracelist(): number of traces in trace group is inconsistent\n");
  
  if ( details > 0 )
    ms_log (0, "Total: %d trace segment(s)\n", tracecnt);
  
}  /* End of mst_printtracelist() */


/***************************************************************************
 * mst_printsynclist:
 *
 * Print SYNC trace list summary information for the specified MSTraceGroup.
 *
 * The SYNC header line will be created using the supplied dccid, if
 * the pointer is NULL the string "DCC" will be used instead.
 *
 * If the subsecond flag is true the segment start and end times will
 * include subsecond precision, otherwise they will be truncated to
 * integer seconds.
 *
 ***************************************************************************/
void
mst_printsynclist ( MSTraceGroup *mstg, char *dccid, flag subsecond )
{
  MSTrace *mst = 0;
  char stime[30];
  char etime[30];
  char yearday[10];
  time_t now;
  struct tm *nt;
  
  if ( ! mstg )
    {
      return;
    }
  
  /* Generate current time stamp */
  now = time (NULL);
  nt = localtime ( &now ); nt->tm_year += 1900; nt->tm_yday += 1;
  snprintf ( yearday, sizeof(yearday), "%04d,%03d", nt->tm_year, nt->tm_yday);
  
  /* Print SYNC header line */
  ms_log (0, "%s|%s\n", (dccid)?dccid:"DCC", yearday);
  
  /* Loope through trace list */
  mst = mstg->traces;
  while ( mst )
    {
      ms_hptime2seedtimestr (mst->starttime, stime, subsecond);
      ms_hptime2seedtimestr (mst->endtime, etime, subsecond);
      
      /* Print SYNC line */
      ms_log (0, "%s|%s|%s|%s|%s|%s||%.10g|%lld|||||||%s\n",
	      mst->network, mst->station, mst->location, mst->channel,
	      stime, etime, mst->samprate, (long long int)mst->samplecnt,
	      yearday);
      
      mst = mst->next;
    }
}  /* End of mst_printsynclist() */


/***************************************************************************
 * mst_printgaplist:
 *
 * Print gap/overlap list summary information for the specified
 * MSTraceGroup.  Overlaps are printed as negative gaps.  The trace
 * summary information in the MSTraceGroup is logically inverted so gaps
 * for like channels are identified.
 *
 * If mingap and maxgap are not NULL their values will be enforced and
 * only gaps/overlaps matching their implied criteria will be printed.
 *
 * The timeformat flag can either be:
 * 0 : SEED time format (year, day-of-year, hour, min, sec)
 * 1 : ISO time format (year, month, day, hour, min, sec)
 * 2 : Epoch time, seconds since the epoch
 ***************************************************************************/
void
mst_printgaplist (MSTraceGroup *mstg, flag timeformat,
		  double *mingap, double *maxgap)
{
  MSTrace *mst;
  char src1[50], src2[50];
  char time1[30], time2[30];
  char gapstr[30];
  double gap;
  double delta;
  double nsamples;
  flag printflag;
  int gapcnt = 0;
  
  if ( ! mstg )
    return;
  
  if ( ! mstg->traces )
    return;
  
  mst = mstg->traces;
  
  ms_log (0, "   Source                Last Sample              Next Sample       Gap  Samples\n");
  
  while ( mst->next )
    {
      mst_srcname (mst, src1, 1);
      mst_srcname (mst->next, src2, 1);
      
      if ( ! strcmp (src1, src2) )
	{
	  /* Skip MSTraces with 0 sample rate, usually from SOH records */
	  if ( mst->samprate == 0.0 )
	    {
	      mst = mst->next;
	      continue;
	    }
	  
	  /* Check that sample rates match using default tolerance */
	  if ( ! MS_ISRATETOLERABLE (mst->samprate, mst->next->samprate) )
	    {
	      ms_log (2, "%s Sample rate changed! %.10g -> %.10g\n",
		      src1, mst->samprate, mst->next->samprate );
	    }
	  
	  gap = (double) (mst->next->starttime - mst->endtime) / HPTMODULUS;
	  
	  /* Check that any overlap is not larger than the trace coverage */
	  if ( gap < 0.0 )
	    {
	      delta = ( mst->next->samprate ) ? (1.0 / mst->next->samprate) : 0.0;
	      
	      if ( (gap * -1.0) > (((double)(mst->next->endtime - mst->next->starttime)/HPTMODULUS) + delta) )
		gap = -(((double)(mst->next->endtime - mst->next->starttime)/HPTMODULUS) + delta);
	    }
	  
	  printflag = 1;
	  
	  /* Check gap/overlap criteria */
	  if ( mingap )
	    if ( gap < *mingap )
	      printflag = 0;
	  
	  if ( maxgap )
	    if ( gap > *maxgap )
	      printflag = 0;
	  
	  if ( printflag )
	    {
	      nsamples = ms_dabs(gap) * mst->samprate;
	      
	      if ( gap > 0.0 )
		nsamples -= 1.0;
	      else
		nsamples += 1.0;
	      
	      /* Fix up gap display */
	      if ( gap >= 86400.0 || gap <= -86400.0 )
		snprintf (gapstr, sizeof(gapstr), "%-3.1fd", (gap / 86400));
	      else if ( gap >= 3600.0 || gap <= -3600.0 )
		snprintf (gapstr, sizeof(gapstr), "%-3.1fh", (gap / 3600));
	      else if ( gap == 0.0 )
		snprintf (gapstr, sizeof(gapstr), "-0  ");
	      else
		snprintf (gapstr, sizeof(gapstr), "%-4.4g", gap);
	      
	      /* Create formatted time strings */
	      if ( timeformat == 2 )
		{
		  snprintf (time1, sizeof(time1), "%.6f", (double) MS_HPTIME2EPOCH(mst->endtime) );
		  snprintf (time2, sizeof(time2), "%.6f", (double) MS_HPTIME2EPOCH(mst->next->starttime) );
		}
	      else if ( timeformat == 1 )
		{
		  if ( ms_hptime2isotimestr (mst->endtime, time1, 1) == NULL )
		    ms_log (2, "Cannot convert trace end time for %s\n", src1);
		  
		  if ( ms_hptime2isotimestr (mst->next->starttime, time2, 1) == NULL )
		    ms_log (2, "Cannot convert next trace start time for %s\n", src1);
		}
	      else
		{
		  if ( ms_hptime2seedtimestr (mst->endtime, time1, 1) == NULL )
		    ms_log (2, "Cannot convert trace end time for %s\n", src1);
		  
		  if ( ms_hptime2seedtimestr (mst->next->starttime, time2, 1) == NULL )
		    ms_log (2, "Cannot convert next trace start time for %s\n", src1);
		}
	      
	      ms_log (0, "%-17s %-24s %-24s %-4s %-.8g\n",
		      src1, time1, time2, gapstr, nsamples);
	      
	      gapcnt++;
	    }
	}
      
      mst = mst->next;
    }
  
  ms_log (0, "Total: %d gap(s)\n", gapcnt);
  
}  /* End of mst_printgaplist() */


/***************************************************************************
 * mst_pack:
 *
 * Pack MSTrace data into Mini-SEED records using the specified record
 * length, encoding format and byte order.  The datasamples array and
 * numsamples field will be adjusted (reduced) based on how many
 * samples were packed.
 *
 * As each record is filled and finished they are passed to
 * record_handler which expects 1) a char * to the record, 2) the
 * length of the record and 3) a pointer supplied by the original
 * caller containing optional private data (handlerdata).  It is the
 * responsibility of record_handler to process the record, the memory
 * will be re-used or freed when record_handler returns.
 *
 * If the flush flag is > 0 all of the data will be packed into data
 * records even though the last one will probably not be filled.
 *
 * If the mstemplate argument is not NULL it will be used as the
 * template for the packed Mini-SEED records.  Otherwise a new
 * MSRecord will be initialized and populated from values in the
 * MSTrace.  The reclen, encoding and byteorder arguments take
 * precedence over those in the template.  The start time, sample
 * rate, datasamples, numsamples and sampletype values from the
 * template will be preserved.
 *
 * Returns the number of records created on success and -1 on error.
 ***************************************************************************/
int
mst_pack ( MSTrace *mst, void (*record_handler) (char *, int, void *),
	   void *handlerdata, int reclen, flag encoding, flag byteorder,
	   int64_t *packedsamples, flag flush, flag verbose,
	   MSRecord *mstemplate )
{
  MSRecord *msr;
  char srcname[50];
  int trpackedrecords;
  int64_t trpackedsamples;
  int samplesize;
  int64_t bufsize;
  
  hptime_t preservestarttime = 0;
  double preservesamprate = 0.0;
  void *preservedatasamples = 0;
  int64_t preservenumsamples = 0;
  char preservesampletype = 0;
  StreamState *preserveststate = 0;
  
  if ( packedsamples )
    *packedsamples = 0;
  
  /* Allocate stream processing state space if needed */
  if ( ! mst->ststate )
    {
      mst->ststate = (StreamState *) malloc (sizeof(StreamState));
      if ( ! mst->ststate )
        {
          ms_log (2, "mst_pack(): Could not allocate memory for StreamState\n");
          return -1;
        }
      memset (mst->ststate, 0, sizeof(StreamState));
    }
  
  if ( mstemplate )
    {
      msr = mstemplate;
      
      preservestarttime = msr->starttime;
      preservesamprate = msr->samprate;
      preservedatasamples = msr->datasamples;
      preservenumsamples = msr->numsamples;
      preservesampletype = msr->sampletype;
      preserveststate = msr->ststate;
    }
  else
    {
      msr = msr_init (NULL);
      
      if ( msr == NULL )
	{
	  ms_log (2, "mst_pack(): Error initializing msr\n");
	  return -1;
	}
      
      msr->dataquality = 'D';
      strcpy (msr->network, mst->network);
      strcpy (msr->station, mst->station);
      strcpy (msr->location, mst->location);
      strcpy (msr->channel, mst->channel);
    }
  
  /* Setup MSRecord template for packing */
  msr->reclen = reclen;
  msr->encoding = encoding;
  msr->byteorder = byteorder;
  
  msr->starttime = mst->starttime;
  msr->samprate = mst->samprate;
  msr->datasamples = mst->datasamples;
  msr->numsamples = mst->numsamples;
  msr->sampletype = mst->sampletype;
  msr->ststate = mst->ststate;
  
  /* Sample count sanity check */
  if ( mst->samplecnt != mst->numsamples )
    {
      ms_log (2, "mst_pack(): Sample counts do not match, abort\n");
      return -1;
    }
  
  /* Pack data */
  trpackedrecords = msr_pack (msr, record_handler, handlerdata, &trpackedsamples, flush, verbose);
  
  if ( verbose > 1 )
    {
      ms_log (1, "Packed %d records for %s trace\n", trpackedrecords, mst_srcname (mst, srcname, 1));
    }
  
  /* Adjust MSTrace start time, data array and sample count */
  if ( trpackedsamples > 0 )
    {
      /* The new start time was calculated my msr_pack */
      mst->starttime = msr->starttime;
      
      samplesize = ms_samplesize (mst->sampletype);
      bufsize = (mst->numsamples - trpackedsamples) * samplesize;
      
      if ( bufsize )
	{
	  memmove (mst->datasamples,
		   (char *) mst->datasamples + (trpackedsamples * samplesize),
		   (size_t) bufsize);
	  
	  mst->datasamples = realloc (mst->datasamples, (size_t) bufsize);
	  
	  if ( mst->datasamples == NULL )
	    {
	      ms_log (2, "mst_pack(): Cannot (re)allocate datasamples buffer\n");
	      return -1;
	    }
	}
      else
	{
	  if ( mst->datasamples )
	    free (mst->datasamples);
	  mst->datasamples = 0;
	}
      
      mst->samplecnt -= trpackedsamples;
      mst->numsamples -= trpackedsamples;
    }
    
  /* Reinstate preserved values if a template was used */
  if ( mstemplate )
    {
      msr->starttime = preservestarttime;
      msr->samprate = preservesamprate;
      msr->datasamples = preservedatasamples;
      msr->numsamples = preservenumsamples;
      msr->sampletype = preservesampletype;
      msr->ststate = preserveststate;
    }
  else
    {
      msr->datasamples = 0;
      msr->ststate = 0;
      msr_free (&msr);
    }
  
  if ( packedsamples )
    *packedsamples = trpackedsamples;
  
  return trpackedrecords;
}  /* End of mst_pack() */


/***************************************************************************
 * mst_packgroup:
 *
 * Pack MSTraceGroup data into Mini-SEED records by calling mst_pack()
 * for each MSTrace in the group.
 *
 * Returns the number of records created on success and -1 on error.
 ***************************************************************************/
int
mst_packgroup ( MSTraceGroup *mstg, void (*record_handler) (char *, int, void *),
		void *handlerdata, int reclen, flag encoding, flag byteorder,
		int64_t *packedsamples, flag flush, flag verbose,
		MSRecord *mstemplate )
{
  MSTrace *mst;
  int trpackedrecords = 0;
  int64_t trpackedsamples = 0;
  char srcname[50];

  if ( ! mstg )
    {
      return -1;
    }
  
  if ( packedsamples )
    *packedsamples = 0;
  
  mst = mstg->traces;
  
  while ( mst )
    {
      if ( mst->numsamples <= 0 )
	{
	  if ( verbose > 1 )
	    {
	      mst_srcname (mst, srcname, 1);
	      ms_log (1, "No data samples for %s, skipping\n", srcname);
	    }
	}
      else
	{
	  trpackedrecords += mst_pack (mst, record_handler, handlerdata, reclen,
				     encoding, byteorder, &trpackedsamples, flush,
				     verbose, mstemplate);
	  
	  if ( trpackedrecords == -1 )
	    break;
	  
	  if ( packedsamples )
	    *packedsamples += trpackedsamples;
	}
      
      mst = mst->next;
    }
  
  return trpackedrecords;
}  /* End of mst_packgroup() */
/***************************************************************************
 * unpack.c:
 *
 * Generic routines to unpack Mini-SEED records.
 *
 * Appropriate values from the record header will be byte-swapped to
 * the host order.  The purpose of this code is to provide a portable
 * way of accessing common SEED data record header information.  All
 * data structures in SEED 2.4 data records are supported.  The data
 * samples are optionally decompressed/unpacked.
 *
 * Written by Chad Trabant,
 *   ORFEUS/EC-Project MEREDIAN
 *   IRIS Data Management Center
 *
 * modified: 2012.114
 ***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "libmseed.h"
#include "unpackdata.h"

/* Function(s) internal to this file */
static int msr_unpack_data (MSRecord * msr, int swapflag, int verbose);
static int check_environment (int verbose);

/* Header and data byte order flags controlled by environment variables */
/* -2 = not checked, -1 = checked but not set, or 0 = LE and 1 = BE */
flag unpackheaderbyteorder = -2;
flag unpackdatabyteorder   = -2;

/* Data encoding format/fallback controlled by environment variable */
/* -2 = not checked, -1 = checked but not set, or = encoding */
int unpackencodingformat   = -2;
int unpackencodingfallback = -2;

/* A pointer to the srcname of the record being unpacked */
char *UNPACK_SRCNAME = NULL;


/***************************************************************************
 * msr_unpack:
 *
 * Unpack a SEED data record header/blockettes and populate a MSRecord
 * struct. All approriate fields are byteswapped, if needed, and
 * pointers to structured data are setup in addition to setting the
 * common header fields.
 *
 * If 'dataflag' is true the data samples are unpacked/decompressed
 * and the MSRecord->datasamples pointer is set appropriately.  The
 * data samples will be either 32-bit integers, 32-bit floats or
 * 64-bit floats (doubles) with the same byte order as the host
 * machine.  The MSRecord->numsamples will be set to the actual number
 * of samples unpacked/decompressed and MSRecord->sampletype will
 * indicated the sample type.
 *
 * All appropriate values will be byte-swapped to the host order,
 * including the data samples.
 *
 * All header values, blockette values and data samples will be
 * overwritten by subsequent calls to this function.
 *
 * If the msr struct is NULL it will be allocated.
 * 
 * Returns MS_NOERROR and populates the MSRecord struct at *ppmsr on
 * success, otherwise returns a libmseed error code (listed in
 * libmseed.h).
 ***************************************************************************/
int
msr_unpack ( char *record, int reclen, MSRecord **ppmsr,
	     flag dataflag, flag verbose )
{
  flag headerswapflag = 0;
  flag dataswapflag = 0;
  int retval;
  
  MSRecord *msr = NULL;
  char sequence_number[7];
  char srcname[50];
  
  /* For blockette parsing */
  BlktLink *blkt_link = 0;
  uint16_t blkt_type;
  uint16_t next_blkt;
  uint32_t blkt_offset;
  uint32_t blkt_length;
  int blkt_count = 0;
  
  if ( ! ppmsr )
    {
      ms_log (2, "msr_unpack(): ppmsr argument cannot be NULL\n");
      return MS_GENERROR;
    }
  
  /* Verify that record includes a valid header */
  if ( ! MS_ISVALIDHEADER(record) )
    {
      ms_recsrcname (record, srcname, 1);
      ms_log (2, "msr_unpack(%s) Record header & quality indicator unrecognized: '%c'\n", srcname);
      ms_log (2, "msr_unpack(%s) This is not a valid Mini-SEED record\n", srcname);
      
      return MS_NOTSEED;
    }
  
  /* Verify that passed record length is within supported range */
  if ( reclen < MINRECLEN || reclen > MAXRECLEN )
    {
      ms_recsrcname (record, srcname, 1);
      ms_log (2, "msr_unpack(%s): Record length is out of range: %d\n", srcname, reclen);
      return MS_OUTOFRANGE;
    }
  
  /* Initialize the MSRecord */  
  if ( ! (*ppmsr = msr_init (*ppmsr)) )
    return MS_GENERROR;
  
  /* Shortcut pointer, historical and help readability */
  msr = *ppmsr;
  
  /* Set raw record pointer and record length */
  msr->record = record;
  msr->reclen = reclen;
  
  /* Check environment variables if necessary */
  if ( unpackheaderbyteorder == -2 ||
       unpackdatabyteorder == -2 ||
       unpackencodingformat == -2 ||
       unpackencodingfallback == -2 )
    if ( check_environment(verbose) )
      return MS_GENERROR;
  
  /* Allocate and copy fixed section of data header */
  msr->fsdh = realloc (msr->fsdh, sizeof (struct fsdh_s));
  
  if ( msr->fsdh == NULL )
    {
      ms_log (2, "msr_unpack(): Cannot allocate memory\n");
      return MS_GENERROR;
    }
  
  memcpy (msr->fsdh, record, sizeof (struct fsdh_s));
  
  /* Check to see if byte swapping is needed by testing the year */
  if ( (msr->fsdh->start_time.year < 1920) ||
       (msr->fsdh->start_time.year > 2050) )
    headerswapflag = dataswapflag = 1;
  
  /* Check if byte order is forced */
  if ( unpackheaderbyteorder >= 0 )
    {
      headerswapflag = ( ms_bigendianhost() != unpackheaderbyteorder ) ? 1 : 0;
    }
  
  if ( unpackdatabyteorder >= 0 )
    {
      dataswapflag = ( ms_bigendianhost() != unpackdatabyteorder ) ? 1 : 0;
    }
  
  /* Swap byte order? */
  if ( headerswapflag )
    {
      MS_SWAPBTIME (&msr->fsdh->start_time);
      ms_gswap2a (&msr->fsdh->numsamples);
      ms_gswap2a (&msr->fsdh->samprate_fact);
      ms_gswap2a (&msr->fsdh->samprate_mult);
      ms_gswap4a (&msr->fsdh->time_correct);
      ms_gswap2a (&msr->fsdh->data_offset);
      ms_gswap2a (&msr->fsdh->blockette_offset);
    }
  
  /* Populate some of the common header fields */
  strncpy (sequence_number, msr->fsdh->sequence_number, 6);
  msr->sequence_number = (int32_t) strtol (sequence_number, NULL, 10);
  msr->dataquality = msr->fsdh->dataquality;
  ms_strncpcleantail (msr->network, msr->fsdh->network, 2);
  ms_strncpcleantail (msr->station, msr->fsdh->station, 5);
  ms_strncpcleantail (msr->location, msr->fsdh->location, 2);
  ms_strncpcleantail (msr->channel, msr->fsdh->channel, 3);
  msr->samplecnt = msr->fsdh->numsamples;
  
  /* Generate source name for MSRecord */
  if ( msr_srcname (msr, srcname, 1) == NULL )
    {
      ms_log (2, "msr_unpack_data(): Cannot generate srcname\n");
      return MS_GENERROR;
    }
  
  /* Set shared srcname pointer to source name */
  UNPACK_SRCNAME = &srcname[0];
  
  /* Report byte swapping status */
  if ( verbose > 2 )
    {
      if ( headerswapflag )
	ms_log (1, "%s: Byte swapping needed for unpacking of header\n",
		UNPACK_SRCNAME);
      else
	ms_log (1, "%s: Byte swapping NOT needed for unpacking of header\n",
		UNPACK_SRCNAME);
    }
  
  /* Traverse the blockettes */
  blkt_offset = msr->fsdh->blockette_offset;
  
  while ((blkt_offset != 0) &&
	 ((int)blkt_offset < reclen) &&
	 (blkt_offset < MAXRECLEN))
    {
      /* Every blockette has a similar 4 byte header: type and next */
      memcpy (&blkt_type, record + blkt_offset, 2);
      blkt_offset += 2;
      memcpy (&next_blkt, record + blkt_offset, 2);
      blkt_offset += 2;
      
      if ( headerswapflag )
	{
	  ms_gswap2 (&blkt_type);
	  ms_gswap2 (&next_blkt);
	}
      
      /* Get blockette length */
      blkt_length = ms_blktlen (blkt_type,
				record + blkt_offset - 4,
				headerswapflag);
      
      if ( blkt_length == 0 )
	{
	  ms_log (2, "msr_unpack(%s): Unknown blockette length for type %d\n",
		  UNPACK_SRCNAME, blkt_type);
	  break;
	}
      
      /* Make sure blockette is contained within the msrecord buffer */
      if ( (int)(blkt_offset - 4 + blkt_length) > reclen )
	{
	  ms_log (2, "msr_unpack(%s): Blockette %d extends beyond record size, truncated?\n",
		  UNPACK_SRCNAME, blkt_type);
	  break;
	}
      
      if ( blkt_type == 100 )
	{			/* Found a Blockette 100 */
	  struct blkt_100_s *blkt_100;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_100_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_100 = (struct blkt_100_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      ms_gswap4 (&blkt_100->samprate);
	    }
	  
	  msr->samprate = msr->Blkt100->samprate;
	}

      else if ( blkt_type == 200 )
	{			/* Found a Blockette 200 */
	  struct blkt_200_s *blkt_200;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_200_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_200 = (struct blkt_200_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      ms_gswap4 (&blkt_200->amplitude);
	      ms_gswap4 (&blkt_200->period);
	      ms_gswap4 (&blkt_200->background_estimate);
	      MS_SWAPBTIME (&blkt_200->time);
	    }
	}

      else if ( blkt_type == 201 )
	{			/* Found a Blockette 201 */
	  struct blkt_201_s *blkt_201;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_201_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_201 = (struct blkt_201_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      ms_gswap4 (&blkt_201->amplitude);
	      ms_gswap4 (&blkt_201->period);
	      ms_gswap4 (&blkt_201->background_estimate);
	      MS_SWAPBTIME (&blkt_201->time);
	    }
	}
      
      else if ( blkt_type == 300 )
	{			/* Found a Blockette 300 */
	  struct blkt_300_s *blkt_300;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_300_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_300 = (struct blkt_300_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      MS_SWAPBTIME (&blkt_300->time);
	      ms_gswap4 (&blkt_300->step_duration);
	      ms_gswap4 (&blkt_300->interval_duration);
	      ms_gswap4 (&blkt_300->amplitude);
	      ms_gswap4 (&blkt_300->reference_amplitude);
	    }
	}
      
      else if ( blkt_type == 310 )
	{			/* Found a Blockette 310 */
	  struct blkt_310_s *blkt_310;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_310_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_310 = (struct blkt_310_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      MS_SWAPBTIME (&blkt_310->time);
	      ms_gswap4 (&blkt_310->duration);
	      ms_gswap4 (&blkt_310->period);
	      ms_gswap4 (&blkt_310->amplitude);
	      ms_gswap4 (&blkt_310->reference_amplitude);
	    }
	}
      
      else if ( blkt_type == 320 )
	{			/* Found a Blockette 320 */
	  struct blkt_320_s *blkt_320;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_320_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_320 = (struct blkt_320_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      MS_SWAPBTIME (&blkt_320->time);
	      ms_gswap4 (&blkt_320->duration);
	      ms_gswap4 (&blkt_320->ptp_amplitude);
	      ms_gswap4 (&blkt_320->reference_amplitude);
	    }
	}

      else if ( blkt_type == 390 )
	{			/* Found a Blockette 390 */
	  struct blkt_390_s *blkt_390;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_390_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_390 = (struct blkt_390_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      MS_SWAPBTIME (&blkt_390->time);
	      ms_gswap4 (&blkt_390->duration);
	      ms_gswap4 (&blkt_390->amplitude);
	    }
	}

      else if ( blkt_type == 395 )
	{			/* Found a Blockette 395 */
	  struct blkt_395_s *blkt_395;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_395_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_395 = (struct blkt_395_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      MS_SWAPBTIME (&blkt_395->time);
	    }
	}
      
      else if ( blkt_type == 400 )
	{			/* Found a Blockette 400 */
	  struct blkt_400_s *blkt_400;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_400_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_400 = (struct blkt_400_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      ms_gswap4 (&blkt_400->azimuth);
	      ms_gswap4 (&blkt_400->slowness);
	      ms_gswap2 (&blkt_400->configuration);
	    }
	}
      
      else if ( blkt_type == 405 )
	{			/* Found a Blockette 405 */
	  struct blkt_405_s *blkt_405;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_405_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_405 = (struct blkt_405_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      ms_gswap2 (&blkt_405->delay_values);
	    }

	  if ( verbose > 0 )
	    {
	      ms_log (1, "msr_unpack(%s): WARNING Blockette 405 cannot be fully supported\n",
		      UNPACK_SRCNAME);
	    }
	}
      
      else if ( blkt_type == 500 )
	{			/* Found a Blockette 500 */
	  struct blkt_500_s *blkt_500;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_500_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_500 = (struct blkt_500_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      ms_gswap4 (&blkt_500->vco_correction);
	      MS_SWAPBTIME (&blkt_500->time);
	      ms_gswap4 (&blkt_500->exception_count);
	    }
	}
      
      else if ( blkt_type == 1000 )
	{			/* Found a Blockette 1000 */
	  struct blkt_1000_s *blkt_1000;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_1000_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;

	  blkt_1000 = (struct blkt_1000_s *) blkt_link->blktdata;
	  
	  /* Calculate record length in bytes as 2^(blkt_1000->reclen) */
	  msr->reclen = (uint32_t) 1 << blkt_1000->reclen;
	  
	  /* Compare against the specified length */
	  if ( msr->reclen != reclen && verbose )
	    {
	      ms_log (2, "msr_unpack(%s): Record length in Blockette 1000 (%d) != specified length (%d)\n",
		      UNPACK_SRCNAME, msr->reclen, reclen);
	    }
	  
	  msr->encoding = blkt_1000->encoding;
	  msr->byteorder = blkt_1000->byteorder;
	}
      
      else if ( blkt_type == 1001 )
	{			/* Found a Blockette 1001 */
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					sizeof (struct blkt_1001_s),
					blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	}
      
      else if ( blkt_type == 2000 )
	{			/* Found a Blockette 2000 */
	  struct blkt_2000_s *blkt_2000;
	  uint16_t b2klen;
	  
	  /* Read the blockette length from blockette */
	  memcpy (&b2klen, record + blkt_offset, 2);
	  if ( headerswapflag ) ms_gswap2 (&b2klen);
	  
	  /* Minus four bytes for the blockette type and next fields */
	  b2klen -= 4;
	  
	  blkt_link = msr_addblockette (msr, record + blkt_offset,
					b2klen, blkt_type, 0);
	  if ( ! blkt_link )
	    break;
	  
	  blkt_link->blktoffset = blkt_offset - 4;
	  blkt_link->next_blkt = next_blkt;
	  
	  blkt_2000 = (struct blkt_2000_s *) blkt_link->blktdata;
	  
	  if ( headerswapflag )
	    {
	      ms_gswap2 (&blkt_2000->length);
	      ms_gswap2 (&blkt_2000->data_offset);
	      ms_gswap4 (&blkt_2000->recnum);
	    }
	}
      
      else
	{                      /* Unknown blockette type */
	  if ( blkt_length >= 4 )
	    {
	      blkt_link = msr_addblockette (msr, record + blkt_offset,
					    blkt_length - 4,
					    blkt_type, 0);
	      
	      if ( ! blkt_link )
		break;
	      
	      blkt_link->blktoffset = blkt_offset - 4;
	      blkt_link->next_blkt = next_blkt;
	    }
	}
      
      /* Check that the next blockette offset is beyond the current blockette */
      if ( next_blkt && next_blkt < (blkt_offset + blkt_length - 4) )
	{
	  ms_log (2, "msr_unpack(%s): Offset to next blockette (%d) is within current blockette ending at byte %d\n",
		  UNPACK_SRCNAME, next_blkt, (blkt_offset + blkt_length - 4));
	  
	  blkt_offset = 0;
	}
      /* Check that the offset is within record length */
      else if ( next_blkt && next_blkt > reclen )
	{
	  ms_log (2, "msr_unpack(%s): Offset to next blockette (%d) from type %d is beyond record length\n",
		  UNPACK_SRCNAME, next_blkt, blkt_type);
	  
	  blkt_offset = 0;
	}
      else
	{
	  blkt_offset = next_blkt;
	}
      
      blkt_count++;
    }  /* End of while looping through blockettes */
  
  /* Check for a Blockette 1000 */
  if ( msr->Blkt1000 == 0 )
    {
      if ( verbose > 1 )
	{
	  ms_log (1, "%s: Warning: No Blockette 1000 found\n", UNPACK_SRCNAME);
	}
    }
  
  /* Check that the data offset is after the blockette chain */
  if ( blkt_link && msr->fsdh->numsamples && msr->fsdh->data_offset < (blkt_link->blktoffset + blkt_link->blktdatalen + 4) )
    {
      ms_log (1, "%s: Warning: Data offset in fixed header (%d) is within the blockette chain ending at %d\n",
	      UNPACK_SRCNAME, msr->fsdh->data_offset, (blkt_link->blktoffset + blkt_link->blktdatalen + 4));
    }
  
  /* Check that the blockette count matches the number parsed */
  if ( msr->fsdh->numblockettes != blkt_count )
    {
      ms_log (1, "%s: Warning: Number of blockettes in fixed header (%d) does not match the number parsed (%d)\n",
	      UNPACK_SRCNAME, msr->fsdh->numblockettes, blkt_count);
    }
  
  /* Populate remaining common header fields */
  msr->starttime = msr_starttime (msr);
  msr->samprate = msr_samprate (msr);
  
  /* Set MSRecord->byteorder if data byte order is forced */
  if ( unpackdatabyteorder >= 0 )
    {
      msr->byteorder = unpackdatabyteorder;
    }
  
  /* Check if encoding format is forced */
  if ( unpackencodingformat >= 0 )
    {
      msr->encoding = unpackencodingformat;
    }
  
  /* Use encoding format fallback if defined and no encoding is set,
   * also make sure the byteorder is set by default to big endian */
  if ( unpackencodingfallback >= 0 && msr->encoding == -1 )
    {
      msr->encoding = unpackencodingfallback;
      
      if ( msr->byteorder == -1 )
	{
	  msr->byteorder = 1;
	}
    }
  
  /* Unpack the data samples if requested */
  if ( dataflag && msr->samplecnt > 0 )
    {
      flag dswapflag = headerswapflag;
      flag bigendianhost = ms_bigendianhost();
      
      /* Determine byte order of the data and set the dswapflag as
	 needed; if no Blkt1000 or UNPACK_DATA_BYTEORDER environment
	 variable setting assume the order is the same as the header */
      if ( msr->Blkt1000 != 0 && unpackdatabyteorder < 0 )
	{
	  dswapflag = 0;
	  
	  /* If BE host and LE data need swapping */
	  if ( bigendianhost && msr->byteorder == 0 )
	    dswapflag = 1;
	  /* If LE host and BE data (or bad byte order value) need swapping */
	  else if ( !bigendianhost && msr->byteorder > 0 )
	    dswapflag = 1;
	}
      else if ( unpackdatabyteorder >= 0 )
	{
	  dswapflag = dataswapflag;
	}
      
      if ( verbose > 2 && dswapflag )
	ms_log (1, "%s: Byte swapping needed for unpacking of data samples\n",
		UNPACK_SRCNAME);
      else if ( verbose > 2 )
	ms_log (1, "%s: Byte swapping NOT needed for unpacking of data samples \n",
		UNPACK_SRCNAME);
      
      retval = msr_unpack_data (msr, dswapflag, verbose);
      
      if ( retval < 0 )
	return retval;
      else
	msr->numsamples = retval;
    }
  else
    {
      if ( msr->datasamples )
	free (msr->datasamples);
      
      msr->datasamples = 0;
      msr->numsamples = 0;
    }
  
  /* Unset shared pointer to source name */
  UNPACK_SRCNAME = NULL;
  
  return MS_NOERROR;
} /* End of msr_unpack() */


/************************************************************************
 *  msr_unpack_data:
 *
 *  Unpack Mini-SEED data samples for a given MSRecord.  The packed
 *  data is accessed in the record indicated by MSRecord->record and
 *  the unpacked samples are placed in MSRecord->datasamples.  The
 *  resulting data samples are either 32-bit integers, 32-bit floats
 *  or 64-bit floats in host byte order.
 *
 *  Return number of samples unpacked or negative libmseed error code.
 ************************************************************************/
static int
msr_unpack_data ( MSRecord *msr, int swapflag, int verbose )
{
  int     datasize;             /* byte size of data samples in record 	*/
  int     nsamples;		/* number of samples unpacked		*/
  int     unpacksize;		/* byte size of unpacked samples	*/
  int     samplesize = 0;       /* size of the data samples in bytes    */
  const char *dbuf;
  int32_t    *diffbuff;
  int32_t     x0, xn;
  
  /* Sanity record length */
  if ( msr->reclen == -1 )
    {
      ms_log (2, "msr_unpack_data(%s): Record size unknown\n",
	      UNPACK_SRCNAME);
      return MS_NOTSEED;
    }
    
  switch (msr->encoding)
    {
    case DE_ASCII:
      samplesize = 1; break;
    case DE_INT16:
    case DE_INT32:
    case DE_FLOAT32:
    case DE_STEIM1:
    case DE_STEIM2:
    case DE_GEOSCOPE24:
    case DE_GEOSCOPE163:
    case DE_GEOSCOPE164:
    case DE_CDSN:
    case DE_SRO:
    case DE_DWWSSN:
      samplesize = 4; break;
    case DE_FLOAT64:
      samplesize = 8; break;
    default:
      samplesize = 0; break;
    }
  
  /* Calculate buffer size needed for unpacked samples */
  unpacksize = (int) msr->samplecnt * samplesize;
  
  /* (Re)Allocate space for the unpacked data */
  if ( unpacksize > 0 )
    {
      msr->datasamples = realloc (msr->datasamples, unpacksize);
      
      if ( msr->datasamples == NULL )
	{
	  ms_log (2, "msr_unpack_data(%s): Cannot (re)allocate memory\n",
		  UNPACK_SRCNAME);
	  return MS_GENERROR;
	}
    }
  else
    {
      if ( msr->datasamples )
	free (msr->datasamples);
      msr->datasamples = 0;
      msr->numsamples = 0;
    }
  
  datasize = msr->reclen - msr->fsdh->data_offset;
  dbuf = msr->record + msr->fsdh->data_offset;
  
  if ( verbose > 2 )
    ms_log (1, "%s: Unpacking %lld samples\n",
	    UNPACK_SRCNAME, (long long int)msr->samplecnt);
  
  /* Decide if this is a encoding that we can decode */
  switch (msr->encoding)
    {
      
    case DE_ASCII:
      if ( verbose > 1 )
	ms_log (1, "%s: Found ASCII data\n", UNPACK_SRCNAME);
      
      nsamples = (int)msr->samplecnt;
      memcpy (msr->datasamples, dbuf, nsamples);
      msr->sampletype = 'a';      
      break;
      
    case DE_INT16:
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking INT-16 data samples\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_int_16 ((int16_t *)dbuf, (int)msr->samplecnt,
				    (int)msr->samplecnt, msr->datasamples,
				    swapflag);
      msr->sampletype = 'i';
      break;
      
    case DE_INT32:
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking INT-32 data samples\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_int_32 ((int32_t *)dbuf, (int)msr->samplecnt,
				    (int)msr->samplecnt, msr->datasamples,
				    swapflag);
      msr->sampletype = 'i';
      break;
      
    case DE_FLOAT32:
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking FLOAT-32 data samples\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_float_32 ((float *)dbuf, (int)msr->samplecnt,
				      (int)msr->samplecnt, msr->datasamples,
				      swapflag);
      msr->sampletype = 'f';
      break;
      
    case DE_FLOAT64:
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking FLOAT-64 data samples\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_float_64 ((double *)dbuf, (int)msr->samplecnt,
				      (int)msr->samplecnt, msr->datasamples,
				      swapflag);
      msr->sampletype = 'd';
      break;
      
    case DE_STEIM1:
      diffbuff = (int32_t *) malloc(unpacksize);
      if ( diffbuff == NULL )
	{
	  ms_log (2, "msr_unpack_data(%s): Cannot allocate diff buffer\n",
		  UNPACK_SRCNAME);
	  return MS_GENERROR;
	}
      
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking Steim-1 data frames\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_steim1 ((FRAME *)dbuf, datasize, (int)msr->samplecnt,
				    (int)msr->samplecnt, msr->datasamples, diffbuff, 
				    &x0, &xn, swapflag, verbose);
      msr->sampletype = 'i';
      free (diffbuff);
      break;
      
    case DE_STEIM2:
      diffbuff = (int32_t *) malloc(unpacksize);
      if ( diffbuff == NULL )
	{
	  ms_log (2, "msr_unpack_data(%s): Cannot allocate diff buffer\n",
		  UNPACK_SRCNAME);
	  return MS_GENERROR;
	}
      
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking Steim-2 data frames\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_steim2 ((FRAME *)dbuf, datasize, (int)msr->samplecnt,
				    (int)msr->samplecnt, msr->datasamples, diffbuff,
				    &x0, &xn, swapflag, verbose);
      msr->sampletype = 'i';
      free (diffbuff);
      break;
      
    case DE_GEOSCOPE24:
    case DE_GEOSCOPE163:
    case DE_GEOSCOPE164:
      if ( verbose > 1 )
	{
	  if ( msr->encoding == DE_GEOSCOPE24 )
	    ms_log (1, "%s: Unpacking GEOSCOPE 24bit integer data samples\n",
		    UNPACK_SRCNAME);
	  if ( msr->encoding == DE_GEOSCOPE163 )
	    ms_log (1, "%s: Unpacking GEOSCOPE 16bit gain ranged/3bit exponent data samples\n",
		    UNPACK_SRCNAME);
	  if ( msr->encoding == DE_GEOSCOPE164 )
	    ms_log (1, "%s: Unpacking GEOSCOPE 16bit gain ranged/4bit exponent data samples\n",
		    UNPACK_SRCNAME);
	}
      
      nsamples = msr_unpack_geoscope (dbuf, (int)msr->samplecnt, (int)msr->samplecnt,
				      msr->datasamples, msr->encoding, swapflag);
      msr->sampletype = 'f';
      break;
      
    case DE_CDSN:
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking CDSN encoded data samples\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_cdsn ((int16_t *)dbuf, (int)msr->samplecnt, (int)msr->samplecnt,
				  msr->datasamples, swapflag);
      msr->sampletype = 'i';
      break;
      
    case DE_SRO:
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking SRO encoded data samples\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_sro ((int16_t *)dbuf, (int)msr->samplecnt, (int)msr->samplecnt,
				 msr->datasamples, swapflag);
      msr->sampletype = 'i';
      break;
      
    case DE_DWWSSN:
      if ( verbose > 1 )
	ms_log (1, "%s: Unpacking DWWSSN encoded data samples\n", UNPACK_SRCNAME);
      
      nsamples = msr_unpack_dwwssn ((int16_t *)dbuf, (int)msr->samplecnt, (int)msr->samplecnt,
				    msr->datasamples, swapflag);
      msr->sampletype = 'i';
      break;
      
    default:
      ms_log (2, "%s: Unsupported encoding format %d (%s)\n",
	      UNPACK_SRCNAME, msr->encoding, (char *) ms_encodingstr(msr->encoding));
      
      return MS_UNKNOWNFORMAT;
    }
  
  return nsamples;
} /* End of msr_unpack_data() */


/************************************************************************
 *  check_environment:
 *
 *  Check environment variables and set global variables appropriately.
 *  
 *  Return 0 on success and -1 on error.
 ************************************************************************/
static int
check_environment (int verbose)
{
  char *envvariable;

  /* Read possible environmental variables that force byteorder */
  if ( unpackheaderbyteorder == -2 )
    {
      if ( (envvariable = getenv("UNPACK_HEADER_BYTEORDER")) )
	{
	  if ( *envvariable != '0' && *envvariable != '1' )
	    {
	      ms_log (2, "Environment variable UNPACK_HEADER_BYTEORDER must be set to '0' or '1'\n");
	      return -1;
	    }
	  else if ( *envvariable == '0' )
	    {
	      unpackheaderbyteorder = 0;
	      if ( verbose > 2 )
		ms_log (1, "UNPACK_HEADER_BYTEORDER=0, unpacking little-endian header\n");
	    }
	  else
	    {
	      unpackheaderbyteorder = 1;
	      if ( verbose > 2 )
		ms_log (1, "UNPACK_HEADER_BYTEORDER=1, unpacking big-endian header\n");
	    }
	}
      else
	{
	  unpackheaderbyteorder = -1;
	}
    }

  if ( unpackdatabyteorder == -2 )
    {
      if ( (envvariable = getenv("UNPACK_DATA_BYTEORDER")) )
	{
	  if ( *envvariable != '0' && *envvariable != '1' )
	    {
	      ms_log (2, "Environment variable UNPACK_DATA_BYTEORDER must be set to '0' or '1'\n");
	      return -1;
	    }
	  else if ( *envvariable == '0' )
	    {
	      unpackdatabyteorder = 0;
	      if ( verbose > 2 )
		ms_log (1, "UNPACK_DATA_BYTEORDER=0, unpacking little-endian data samples\n");
	    }
	  else
	    {
	      unpackdatabyteorder = 1;
	      if ( verbose > 2 )
		ms_log (1, "UNPACK_DATA_BYTEORDER=1, unpacking big-endian data samples\n");
	    }
	}
      else
	{
	  unpackdatabyteorder = -1;
	}
    }
  
  /* Read possible environmental variable that forces encoding format */
  if ( unpackencodingformat == -2 )
    {
      if ( (envvariable = getenv("UNPACK_DATA_FORMAT")) )
	{
	  unpackencodingformat = (int) strtol (envvariable, NULL, 10);
	  
	  if ( unpackencodingformat < 0 || unpackencodingformat > 33 )
	    {
	      ms_log (2, "Environment variable UNPACK_DATA_FORMAT set to invalid value: '%d'\n", unpackencodingformat);
	      return -1;
	    }
	  else if ( verbose > 2 )
	    ms_log (1, "UNPACK_DATA_FORMAT, unpacking data in encoding format %d\n", unpackencodingformat);
	}
      else
	{
	  unpackencodingformat = -1;
	}
    }
  
  /* Read possible environmental variable to be used as a fallback encoding format */
  if ( unpackencodingfallback == -2 )
    {
      if ( (envvariable = getenv("UNPACK_DATA_FORMAT_FALLBACK")) )
	{
	  unpackencodingfallback = (int) strtol (envvariable, NULL, 10);
	  
	  if ( unpackencodingfallback < 0 || unpackencodingfallback > 33 )
	    {
	      ms_log (2, "Environment variable UNPACK_DATA_FORMAT_FALLBACK set to invalid value: '%d'\n",
		      unpackencodingfallback);
	      return -1;
	    }
	  else if ( verbose > 2 )
	    ms_log (1, "UNPACK_DATA_FORMAT_FALLBACK, fallback data unpacking encoding format %d\n",
		    unpackencodingfallback);
	}
      else
	{
	  unpackencodingfallback = 10;  /* Default fallback is Steim-1 encoding */
	}
    }
  
  return 0;
} /* End of check_environment() */
/************************************************************************
 *  Routines for unpacking INT_16, INT_32, FLOAT_32, FLOAT_64,
 *  STEIM1, STEIM2, GEOSCOPE (24bit and gain ranged), CDSN, SRO
 *  and DWWSSN encoded data records.
 *
 *  Some routines originated and were borrowed from qlib2 by:
 *
 *	Douglas Neuhauser
 *	Seismographic Station
 *	University of California, Berkeley
 *	doug@seismo.berkeley.edu
 *									
 *  Modified by Chad Trabant,
 *  (previously) ORFEUS/EC-Project MEREDIAN
 *  (currently) IRIS Data Management Center
 *
 *  modified: 2010.012
 ************************************************************************/

/*
 * Copyright (c) 1996 The Regents of the University of California.
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for educational, research and non-profit purposes,
 * without fee, and without a written agreement is hereby granted,
 * provided that the above copyright notice, this paragraph and the
 * following three paragraphs appear in all copies.
 * 
 * Permission to incorporate this software into commercial products may
 * be obtained from the Office of Technology Licensing, 2150 Shattuck
 * Avenue, Suite 510, Berkeley, CA  94704.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES,
 * INCLUDING LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND
 * ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE
 * PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
 * CALIFORNIA HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT,
 * UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include "libmseed.h"
#include "unpackdata.h"

#define MAX12 0x7ff         /* maximum 12 bit positive # */
#define MAX14 0x1fff        /* maximum 14 bit positive # */
#define MAX16 0x7fff        /* maximum 16 bit positive # */
#define MAX24 0x7fffff      /* maximum 24 bit positive # */

/* For Steim encodings */
#define Y0  pf->w[0].fw
#define YN  pf->w[1].fw



/************************************************************************
 *  msr_unpack_int_16:							*
 *                                                                      *
 *  Unpack int_16 miniSEED data and place in supplied buffer.           *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_int_16 
 (int16_t      *ibuf,		/* ptr to input data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  int32_t      *databuff,	/* ptr to unpacked data array.		*/
  int		swapflag)       /* if data should be swapped.	        */
{
  int		nd = 0;		/* # of data points in packet.		*/
  int16_t	stmp;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++) {
    stmp = ibuf[nd];
    if ( swapflag ) ms_gswap2a (&stmp);
    databuff[nd] = stmp;
  }
  
  return nd;
}  /* End of msr_unpack_int_16() */


/************************************************************************
 *  msr_unpack_int_32:							*
 *                                                                      *
 *  Unpack int_32 miniSEED data and place in supplied buffer.           *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_int_32
 (int32_t      *ibuf,		/* ptr to input data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  int32_t      *databuff,	/* ptr to unpacked data array.		*/
  int		swapflag)	/* if data should be swapped.	        */
{
  int		nd = 0;		/* # of data points in packet.		*/
  int32_t    	itmp;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++) {
    itmp = ibuf[nd];
    if ( swapflag) ms_gswap4a (&itmp);
    databuff[nd] = itmp;
  }
  
  return nd;
}  /* End of msr_unpack_int_32() */


/************************************************************************
 *  msr_unpack_float_32:	       				 	*
 *                                                                      *
 *  Unpack float_32 miniSEED data and place in supplied buffer.	        *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_float_32
 (float	       *fbuf,		/* ptr to input data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  float	       *databuff,	/* ptr to unpacked data array.		*/
  int		swapflag)	/* if data should be swapped.	        */
{
  int		nd = 0;		/* # of data points in packet.		*/
  float    	ftmp;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++) {
    ftmp = fbuf[nd];
    if ( swapflag ) ms_gswap4a (&ftmp);
    databuff[nd] = ftmp;
  }
  
  return nd;
}  /* End of msr_unpack_float_32() */


/************************************************************************
 *  msr_unpack_float_64:	       					*
 *                                                                      *
 *  Unpack float_64 miniSEED data and place in supplied buffer.	        *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_float_64
 (double       *fbuf,		/* ptr to input data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  double       *databuff,	/* ptr to unpacked data array.		*/
  int		swapflag)	/* if data should be swapped.	        */
{
  int		nd = 0;		/* # of data points in packet.		*/
  double  	dtmp;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++) {
    dtmp = fbuf[nd];
    if ( swapflag ) ms_gswap8a (&dtmp);
    databuff[nd] = dtmp;
  }
  
  return nd;
}  /* End of msr_unpack_float_64() */


/************************************************************************
 *  msr_unpack_steim1:							*
 *                                                                      *
 *  Unpack STEIM1 data frames and place in supplied buffer.		*
 *  See the SEED format manual for Steim-1 encoding details.            *
 *                                                                      *
 *  Return: # of samples returned or negative error code.               *
 ************************************************************************/
int msr_unpack_steim1
 (FRAME	       *pf,		/* ptr to Steim1 data frames.		*/
  int		nbytes,		/* number of bytes in all data frames.	*/
  int		num_samples,	/* number of data samples in all frames.*/
  int		req_samples,	/* number of data desired by caller.	*/
  int32_t      *databuff,	/* ptr to unpacked data array.		*/
  int32_t      *diffbuff,	/* ptr to unpacked diff array.		*/
  int32_t      *px0,		/* return Y0, first sample in frame.	*/
  int32_t      *pxn,		/* return YN, last sample in frame.	*/
  int		swapflag,	/* if data should be swapped.	        */
  int           verbose)
{
  int32_t      *diff = diffbuff;
  int32_t      *data = databuff;
  int32_t      *prev;
  int	        num_data_frames = nbytes / sizeof(FRAME);
  int		nd = 0;		/* # of data points in packet.		*/
  int		fn;		/* current frame number.		*/
  int		wn;		/* current work number in the frame.	*/
  int		compflag;      	/* current compression flag.		*/
  int		nr, i;
  int32_t	last_data;
  int32_t	itmp;
  int16_t	stmp;
  uint32_t	ctrl;
  
  if (num_samples < 0) return 0;
  if (num_samples == 0) return 0;
  if (req_samples < 0) return 0;
  
  /* Extract forward and reverse integration constants in first frame */
  *px0 = Y0;
  *pxn = YN;
  
  if ( swapflag )
    {
      ms_gswap4a (px0);
      ms_gswap4a (pxn);
    }
  
  if ( verbose > 2 )
    ms_log (1, "%s: forward/reverse integration constants:\nY0: %d  YN: %d\n",
	    UNPACK_SRCNAME, *px0, *pxn);
  
  /* Decode compressed data in each frame */
  for (fn = 0; fn < num_data_frames; fn++)
    {
      
      ctrl = pf->ctrl;
      if ( swapflag ) ms_gswap4a (&ctrl);

      for (wn = 0; wn < VALS_PER_FRAME; wn++)
	{
	  if (nd >= num_samples) break;
	  
	  compflag = (ctrl >> ((VALS_PER_FRAME-wn-1)*2)) & 0x3;
	  
	  switch (compflag)
	    {
	      
	    case STEIM1_SPECIAL_MASK:
	      /* Headers info -- skip it */
	      break;
	      
	    case STEIM1_BYTE_MASK:
	      /* Next 4 bytes are 4 1-byte differences */
	      for (i=0; i < 4 && nd < num_samples; i++, nd++)
		*diff++ = pf->w[wn].byte[i];
	      break;
	      
	    case STEIM1_HALFWORD_MASK:
	      /* Next 4 bytes are 2 2-byte differences */
	      for (i=0; i < 2 && nd < num_samples; i++, nd++)
		{
		  if ( swapflag )
		    {
		      stmp = pf->w[wn].hw[i];
		      ms_gswap2a (&stmp);
		      *diff++ = stmp;
		    }
		  else *diff++ = pf->w[wn].hw[i];
		}
	      break;
	      
	    case STEIM1_FULLWORD_MASK:
	      /* Next 4 bytes are 1 4-byte difference */
	      if ( swapflag )
		{
		  itmp = pf->w[wn].fw;
		  ms_gswap4a (&itmp);
		  *diff++ = itmp;
		}
	      else *diff++ = pf->w[wn].fw;
	      nd++;
	      break;
	      
	    default:
	      /* Should NEVER get here */
	      ms_log (2, "msr_unpack_steim1(%s): invalid compression flag = %d\n",
		      UNPACK_SRCNAME, compflag);
	      return MS_STBADCOMPFLAG;
	    }
	}
      ++pf;
    }
  
  /* Test if the number of samples implied by the data frames is the
   * same number indicated in the header.
   */
  if ( nd != num_samples )
    {
      ms_log (1, "Warning: msr_unpack_steim1(%s): number of samples indicated in header (%d) does not equal data (%d)\n",
	      UNPACK_SRCNAME, num_samples, nd);
    }
  
  /*	For now, assume sample count in header to be correct.		*/
  /*	One way of "trimming" data from a block is simply to reduce	*/
  /*	the sample count.  It is not clear from the documentation	*/
  /*	whether this is a valid or not, but it appears to be done	*/
  /*	by other program, so we should not complain about its effect.	*/
  
  nr = req_samples;
  
  /* Compute first value based on last_value from previous buffer.	*/
  /* The two should correspond in all cases EXCEPT for the first	*/
  /* record for each component (because we don't have a valid xn from	*/
  /* a previous record).  Although the Steim compression algorithm	*/
  /* defines x(-1) as 0 for the first record, this only works for the	*/
  /* first record created since coldstart of the datalogger, NOT the	*/
  /* first record of an arbitrary starting record.	                */
  
  /* In all cases, assume x0 is correct, since we don't have x(-1).	*/
  data = databuff;
  diff = diffbuff;
  last_data = *px0;
  if (nr > 0)
    *data = *px0;
  
  /* Compute all but first values based on previous value               */
  prev = data - 1;
  while (--nr > 0 && --nd > 0)
    last_data = *++data = *++diff + *++prev;
  
  /* If a short count was requested compute the last sample in order    */
  /* to perform the integrity check comparison                          */
  while (--nd > 0)
    last_data = *++diff + last_data;
  
  /* Verify that the last value is identical to xn = rev. int. constant */
  if (last_data != *pxn)
    {
      ms_log (1, "%s: Warning: Data integrity check for Steim-1 failed, last_data=%d, xn=%d\n",
	      UNPACK_SRCNAME, last_data, *pxn);
    }
  
  return ((req_samples < num_samples) ? req_samples : num_samples);
}  /* End of msr_unpack_steim1() */


/************************************************************************
 *  msr_unpack_steim2:							*
 *                                                                      *
 *  Unpack STEIM2 data frames and place in supplied buffer.		*
 *  See the SEED format manual for Steim-2 encoding details.            *
 *                                                                      *
 *  Return: # of samples returned or negative error code.               *
 ************************************************************************/
int msr_unpack_steim2 
 (FRAME	       *pf,		/* ptr to Steim2 data frames.		*/
  int		nbytes,		/* number of bytes in all data frames.	*/
  int		num_samples,	/* number of data samples in all frames.*/
  int		req_samples,	/* number of data desired by caller.	*/
  int32_t      *databuff,	/* ptr to unpacked data array.		*/
  int32_t      *diffbuff,	/* ptr to unpacked diff array.		*/
  int32_t      *px0,		/* return Y0, first sample in frame.	*/
  int32_t      *pxn,		/* return YN, last sample in frame.	*/
  int		swapflag,	/* if data should be swapped.	        */
  int           verbose)
{
  int32_t      *diff = diffbuff;
  int32_t      *data = databuff;
  int32_t      *prev;
  int		num_data_frames = nbytes / sizeof(FRAME);
  int		nd = 0;		/* # of data points in packet.		*/
  int		fn;		/* current frame number.		*/
  int		wn;		/* current work number in the frame.	*/
  int		compflag;     	/* current compression flag.		*/
  int		nr, i;
  int		n, bits, m1, m2;
  int32_t	last_data;
  int32_t    	val;
  int8_t	dnib;
  uint32_t	ctrl;
  
  if (num_samples < 0) return 0;
  if (num_samples == 0) return 0;
  if (req_samples < 0) return 0;
  
  /* Extract forward and reverse integration constants in first frame.*/
  *px0 = Y0;
  *pxn = YN;
  
  if ( swapflag )
    {
      ms_gswap4a (px0);
      ms_gswap4a (pxn);
    }
  
  if ( verbose > 2 )
    ms_log (1, "%s: forward/reverse integration constants:  Y0: %d  YN: %d\n",
	    UNPACK_SRCNAME, *px0, *pxn);
  
  /* Decode compressed data in each frame */
  for (fn = 0; fn < num_data_frames; fn++)
    {
      
      ctrl = pf->ctrl;
      if ( swapflag ) ms_gswap4a (&ctrl);
      
      for (wn = 0; wn < VALS_PER_FRAME; wn++)
	{
	  if (nd >= num_samples) break;
	  
	  compflag = (ctrl >> ((VALS_PER_FRAME-wn-1)*2)) & 0x3;
	  
	  switch (compflag)
	    {
	    case STEIM2_SPECIAL_MASK:
	      /* Headers info -- skip it */
	      break;
	      
	    case STEIM2_BYTE_MASK:
	      /* Next 4 bytes are 4 1-byte differences */
	      for (i=0; i < 4 && nd < num_samples; i++, nd++)
		*diff++ = pf->w[wn].byte[i];
	      break;
	      
	    case STEIM2_123_MASK:
	      val = pf->w[wn].fw;
	      if ( swapflag ) ms_gswap4a (&val);
	      dnib =  val >> 30 & 0x3;
	      switch (dnib)
		{
		case 1:	/* 1 30-bit difference */
		  bits = 30; n = 1; m1 = 0x3fffffff; m2 = 0x20000000; break;
		case 2:	/* 2 15-bit differences */
		  bits = 15; n = 2; m1 = 0x00007fff; m2 = 0x00004000; break;
		case 3:	/* 3 10-bit differences */
		  bits = 10; n = 3; m1 = 0x000003ff; m2 = 0x00000200; break;
		default:	/*  should NEVER get here  */
		  ms_log (2, "msr_unpack_steim2(%s): invalid compflag, dnib, fn, wn = %d, %d, %d, %d\n", 
			  UNPACK_SRCNAME, compflag, dnib, fn, wn);
		  return MS_STBADCOMPFLAG;
		}
	      /*  Uncompress the differences */
	      for (i=(n-1)*bits; i >= 0 && nd < num_samples; i-=bits, nd++)
		{
		  *diff = (val >> i) & m1;
		  *diff = (*diff & m2) ? *diff | ~m1 : *diff;
		  diff++;
		}
	      break;
	      
	    case STEIM2_567_MASK:
	      val = pf->w[wn].fw;
	      if ( swapflag ) ms_gswap4a (&val);
	      dnib =  val >> 30 & 0x3;
	      switch (dnib)
		{
		case 0:	/*  5 6-bit differences  */
		  bits = 6; n = 5; m1 = 0x0000003f; m2 = 0x00000020; break;
		case 1:	/*  6 5-bit differences  */
		  bits = 5; n = 6; m1 = 0x0000001f; m2 = 0x00000010; break;
		case 2:	/*  7 4-bit differences  */
		  bits = 4; n = 7; m1 = 0x0000000f; m2 = 0x00000008; break;
		default:
		  ms_log (2, "msr_unpack_steim2(%s): invalid compflag, dnib, fn, wn = %d, %d, %d, %d\n", 
			  UNPACK_SRCNAME, compflag, dnib, fn, wn);
		  return MS_STBADCOMPFLAG;
		}
	      /* Uncompress the differences */
	      for (i=(n-1)*bits; i >= 0 && nd < num_samples; i-=bits, nd++)
		{
		  *diff = (val >> i) & m1;
		  *diff = (*diff & m2) ? *diff | ~m1 : *diff;
		  diff++;
		}
	      break;
	      
	    default:
	      /* Should NEVER get here */
	      ms_log (2, "msr_unpack_steim2(%s): invalid compflag, fn, wn = %d, %d, %d - nsamp: %d\n",
		      UNPACK_SRCNAME, compflag, fn, wn, nd);
	      return MS_STBADCOMPFLAG;
	    }
	}
      ++pf;
    }
    
  /* Test if the number of samples implied by the data frames is the
   * same number indicated in the header.
   */
  if ( nd != num_samples )
    {
      ms_log (1, "Warning: msr_unpack_steim2(%s): number of samples indicated in header (%d) does not equal data (%d)\n",
	      UNPACK_SRCNAME, num_samples, nd);
    }

  /*	For now, assume sample count in header to be correct.		*/
  /*	One way of "trimming" data from a block is simply to reduce	*/
  /*	the sample count.  It is not clear from the documentation	*/
  /*	whether this is a valid or not, but it appears to be done	*/
  /*	by other program, so we should not complain about its effect.	*/
  
  nr = req_samples;
  
  /* Compute first value based on last_value from previous buffer.	*/
  /* The two should correspond in all cases EXCEPT for the first	*/
  /* record for each component (because we don't have a valid xn from	*/
  /* a previous record).  Although the Steim compression algorithm	*/
  /* defines x(-1) as 0 for the first record, this only works for the	*/
  /* first record created since coldstart of the datalogger, NOT the	*/
  /* first record of an arbitrary starting record.	                */
  
  /* In all cases, assume x0 is correct, since we don't have x(-1).	*/
  data = databuff;
  diff = diffbuff;
  last_data = *px0;
  if (nr > 0)
    *data = *px0;

  /* Compute all but first values based on previous value               */
  prev = data - 1;
  while (--nr > 0 && --nd > 0)
    last_data = *++data = *++diff + *++prev;
  
  /* If a short count was requested compute the last sample in order    */
  /* to perform the integrity check comparison                          */
  while (--nd > 0)
    last_data = *++diff + last_data;
  
  /* Verify that the last value is identical to xn = rev. int. constant */
  if (last_data != *pxn)
    {
      ms_log (1, "%s: Warning: Data integrity check for Steim-2 failed, last_data=%d, xn=%d\n",
	      UNPACK_SRCNAME, last_data, *pxn);
    }
  
  return ((req_samples < num_samples) ? req_samples : num_samples);
}  /* End of msr_unpack_steim2() */


/* Defines for GEOSCOPE encoding */
#define GEOSCOPE_MANTISSA_MASK 0x0fff   /* mask for mantissa */
#define GEOSCOPE_GAIN3_MASK 0x7000      /* mask for gainrange factor */
#define GEOSCOPE_GAIN4_MASK 0xf000      /* mask for gainrange factor */
#define GEOSCOPE_SHIFT 12               /* # bits in mantissa */

/************************************************************************
 *  msr_unpack_geoscope:                                                *
 *                                                                      *
 *  Unpack GEOSCOPE gain ranged data (demultiplexed only) encoded       *
 *  miniSEED data and place in supplied buffer.                         *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_geoscope
 (const char   *edata,		/* ptr to encoded data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  float	       *databuff,	/* ptr to unpacked data array.		*/
  int           encoding,       /* specific GEOSCOPE encoding type      */
  int		swapflag)	/* if data should be swapped.	        */
{
  int nd = 0;		/* # of data points in packet.		*/
  int mantissa;		/* mantissa from SEED data */
  int gainrange;	/* gain range factor */
  int exponent;		/* total exponent */
  int k;
  uint64_t exp2val;
  int16_t sint;
  double dsample = 0.0;
  
  union {
    uint8_t b[4];
    uint32_t i;
  } sample32;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;

  /* Make sure we recognize this as a GEOSCOPE encoding format */
  if ( encoding != DE_GEOSCOPE24 &&
       encoding != DE_GEOSCOPE163 &&
       encoding != DE_GEOSCOPE164 )
    {
      ms_log (2, "msr_unpack_geoscope(%s): unrecognized GEOSCOPE encoding: %d\n",
	      UNPACK_SRCNAME, encoding);
      return -1;
    }
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++)
    {
      switch (encoding)
	{
	case DE_GEOSCOPE24:
	  sample32.i = 0;
	  if ( swapflag )
	    for (k=0; k < 3; k++)
	      sample32.b[2-k] = edata[k];
	  else
	    for (k=0; k < 3; k++)
	      sample32.b[1+k] = edata[k];
	  
	  mantissa = sample32.i;

	  /* Take 2's complement for mantissa for overflow */
	  if (mantissa > MAX24) 
	    mantissa -= 2 * (MAX24 + 1);
	  
	  /* Store */
	  dsample = (double) mantissa;
	  
	  break;
	case DE_GEOSCOPE163:
	  memcpy (&sint, edata, sizeof(int16_t));
	  if ( swapflag ) ms_gswap2a(&sint);
	  
	  /* Recover mantissa and gain range factor */
	  mantissa = (sint & GEOSCOPE_MANTISSA_MASK);
	  gainrange = (sint & GEOSCOPE_GAIN3_MASK) >> GEOSCOPE_SHIFT;
	  
	  /* Exponent is just gainrange for GEOSCOPE */
	  exponent = gainrange;
	  
	  /* Calculate sample as mantissa / 2^exponent */
	  exp2val = (uint64_t) 1 << exponent;
	  dsample = ((double) (mantissa-2048)) / exp2val;
	  
	  break;
	case DE_GEOSCOPE164:
	  memcpy (&sint, edata, sizeof(int16_t));
	  if ( swapflag ) ms_gswap2a(&sint);
	  
	  /* Recover mantissa and gain range factor */
	  mantissa = (sint & GEOSCOPE_MANTISSA_MASK);
	  gainrange = (sint & GEOSCOPE_GAIN4_MASK) >> GEOSCOPE_SHIFT;
	  
	  /* Exponent is just gainrange for GEOSCOPE */
	  exponent = gainrange;
	  
	  /* Calculate sample as mantissa / 2^exponent */
	  exp2val = (uint64_t) 1 << exponent;
	  dsample = ((double) (mantissa-2048)) / exp2val;
	  
	  break;
	}
      
      /* Save sample in output array */
      databuff[nd] = (float) dsample;
      
      /* Increment edata pointer depending on size */
      switch (encoding)
	{
	case DE_GEOSCOPE24:
	  edata += 3;
	  break;
	case DE_GEOSCOPE163:
	case DE_GEOSCOPE164:
	  edata += 2;
	  break;
	}
    }
  
  return nd;
}  /* End of msr_unpack_geoscope() */


/* Defines for CDSN encoding */
#define CDSN_MANTISSA_MASK 0x3fff   /* mask for mantissa */
#define CDSN_GAINRANGE_MASK 0xc000  /* mask for gainrange factor */
#define CDSN_SHIFT 14               /* # bits in mantissa */

/************************************************************************
 *  msr_unpack_cdsn:                                                    *
 *                                                                      *
 *  Unpack CDSN gain ranged data encoded miniSEED data and place in     *
 *  supplied buffer.                                                    *
 *                                                                      *
 *  Notes from original rdseed routine:                                 *
 *  CDSN data are compressed according to the formula                   *
 *                                                                      *
 *  sample = M * (2 exp G)                                              *
 *                                                                      *
 *  where                                                               *
 *     sample = seismic data sample                                     *
 *     M      = mantissa; biased mantissa B is written to tape          *
 *     G      = exponent of multiplier (i.e. gain range factor);        *
 *                      key K is written to tape                        *
 *     exp    = exponentiation operation                                *
 *     B      = M + 8191, biased mantissa, written to tape              *
 *     K      = key to multiplier exponent, written to tape             *
 *                      K may have any of the values 0 - 3, as follows: *
 *                      0 => G = 0, multiplier = 2 exp 0 = 1            *
 *                      1 => G = 2, multiplier = 2 exp 2 = 4            *
 *                      2 => G = 4, multiplier = 2 exp 4 = 16           *
 *                      3 => G = 7, multiplier = 2 exp 7 = 128          *
 *     Data are stored on tape in two bytes as follows:                 *
 *             fedc ba98 7654 3210 = bit number, power of two           *
 *             KKBB BBBB BBBB BBBB = form of SEED data                  *
 *             where K = key to multiplier exponent and B = biased mantissa *
 *                                                                      *
 *     Masks to recover key to multiplier exponent and biased mantissa  *
 *     from tape are:                                                   *
 *             fedc ba98 7654 3210 = bit number = power of two          *
 *             0011 1111 1111 1111 = 0x3fff     = mask for biased mantissa *
 *            1100 0000 0000 0000 = 0xc000     = mask for gain range key *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_cdsn
 (int16_t      *edata,		/* ptr to encoded data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  int32_t      *databuff,	/* ptr to unpacked data array.		*/
  int		swapflag)	/* if data should be swapped.	        */
{
  int32_t nd = 0;	/* sample count */
  int32_t mantissa;	/* mantissa */
  int32_t gainrange;	/* gain range factor */
  int32_t mult = -1;    /* multiplier for gain range */
  uint16_t sint;
  int32_t sample;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++)
    {
      memcpy (&sint, &edata[nd], sizeof(int16_t));
      if ( swapflag ) ms_gswap2a(&sint);
      
      /* Recover mantissa and gain range factor */
      mantissa = (sint & CDSN_MANTISSA_MASK);
      gainrange = (sint & CDSN_GAINRANGE_MASK) >> CDSN_SHIFT;
      
      /* Determine multiplier from the gain range factor and format definition
       * because shift operator is used later, these are powers of two */
      if ( gainrange == 0 ) mult = 0;
      else if ( gainrange == 1 ) mult = 2;
      else if ( gainrange == 2 ) mult = 4;
      else if ( gainrange == 3 ) mult = 7;
      
      /* Unbias the mantissa */
      mantissa -= MAX14;
      
      /* Calculate sample from mantissa and multiplier using left shift
       * mantissa << mult is equivalent to mantissa * (2 exp (mult)) */
      sample = (mantissa << mult);
      
      /* Save sample in output array */
      databuff[nd] = sample;
    }
  
  return nd;
}  /* End of msr_unpack_cdsn() */


/* Defines for SRO encoding */
#define SRO_MANTISSA_MASK 0x0fff   /* mask for mantissa */
#define SRO_GAINRANGE_MASK 0xf000  /* mask for gainrange factor */
#define SRO_SHIFT 12               /* # bits in mantissa */

/************************************************************************
 *  msr_unpack_sro:                                                     *
 *                                                                      *
 *  Unpack SRO gain ranged data encoded miniSEED data and place in      *
 *  supplied buffer.                                                    *
 *                                                                      *
 *  Notes from original rdseed routine:                                 *
 *  SRO data are represented according to the formula                   *
 *                                                                      *
 *  sample = M * (b exp {[m * (G + agr)] + ar})                         *
 *                                                                      *
 *  where                                                               *
 *	sample = seismic data sample                                    *
 *	M      = mantissa                                               *
 *	G      = gain range factor                                      *
 *	b      = base to be exponentiated = 2 for SRO                   *
 *	m      = multiplier  = -1 for SRO                               *
 *	agr    = term to be added to gain range factor = 0 for SRO      *
 *	ar     = term to be added to [m * (gr + agr)]  = 10 for SRO     *
 *	exp    = exponentiation operation                               *
 *	Data are stored in two bytes as follows:                        *
 *		fedc ba98 7654 3210 = bit number, power of two          *
 *		GGGG MMMM MMMM MMMM = form of SEED data                 *
 *		where G = gain range factor and M = mantissa            *
 *	Masks to recover gain range and mantissa:                       *
 *		fedc ba98 7654 3210 = bit number = power of two         *
 *		0000 1111 1111 1111 = 0x0fff     = mask for mantissa    *
 *		1111 0000 0000 0000 = 0xf000     = mask for gain range  *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_sro
 (int16_t      *edata,		/* ptr to encoded data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  int32_t      *databuff,	/* ptr to unpacked data array.		*/
  int		swapflag)	/* if data should be swapped.	        */
{
  int32_t nd = 0;	/* sample count */
  int32_t mantissa;	/* mantissa */
  int32_t gainrange;	/* gain range factor */
  int32_t add2gr;       /* added to gainrage factor */
  int32_t mult;         /* multiplier for gain range */
  int32_t add2result;   /* added to multiplied gain rage */
  int32_t exponent;	/* total exponent */
  uint16_t sint;
  int32_t sample;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;
  
  add2gr = 0;
  mult = -1;
  add2result = 10;
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++)
    {
      memcpy (&sint, &edata[nd], sizeof(int16_t));
      if ( swapflag ) ms_gswap2a(&sint);
      
      /* Recover mantissa and gain range factor */
      mantissa = (sint & SRO_MANTISSA_MASK);
      gainrange = (sint & SRO_GAINRANGE_MASK) >> SRO_SHIFT;
      
      /* Take 2's complement for mantissa */
      if ( mantissa > MAX12 )
	mantissa -= 2 * (MAX12 + 1);
      
      /* Calculate exponent, SRO exponent = 0..10 */
      exponent = (mult * (gainrange + add2gr)) + add2result;
      
      if ( exponent < 0 || exponent > 10 )
	{
	  ms_log (2, "msr_unpack_sro(%s): SRO gain ranging exponent out of range: %d\n",
		  UNPACK_SRCNAME, exponent);
	  return MS_GENERROR;
	}
      
      /* Calculate sample as mantissa * 2^exponent */
      sample = mantissa * ( (uint64_t) 1 << exponent );
      
      /* Save sample in output array */
      databuff[nd] = sample;
    }
  
  return nd;
}  /* End of msr_unpack_sro() */


/************************************************************************
 *  msr_unpack_dwwssn:                                                  *
 *                                                                      *
 *  Unpack DWWSSN encoded miniSEED data and place in supplied buffer.   *
 *                                                                      *
 *  Return: # of samples returned.                                      *
 ************************************************************************/
int msr_unpack_dwwssn
 (int16_t      *edata,		/* ptr to encoded data.			*/
  int		num_samples,	/* number of data samples in total.     */
  int		req_samples,	/* number of data desired by caller.	*/
  int32_t      *databuff,	/* ptr to unpacked data array.		*/
  int		swapflag)	/* if data should be swapped.	        */
{
  int32_t nd = 0;	/* sample count */
  int32_t sample;
  uint16_t sint;
  
  if (num_samples < 0) return 0;
  if (req_samples < 0) return 0;
  
  for (nd=0; nd<req_samples && nd<num_samples; nd++)
    {
      memcpy (&sint, &edata[nd], sizeof(uint16_t));
      if ( swapflag ) ms_gswap2a(&sint);
      sample = (int32_t) sint;
      
      /* Take 2's complement for sample */
      if ( sample > MAX16 )
	sample -= 2 * (MAX16 + 1);
      
      /* Save sample in output array */
      databuff[nd] = sample;
    }
  
  return nd;
}  /* End of msr_unpack_dwwssn() */
