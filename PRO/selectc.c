// selcts text string from comment lines in cat fle
// made by rune luis 2012
//
// Includes
#include <stdio.h>
#include <string.h>

// Defines
#define FILE_LEN 256
#define COMM_LEN 78

// Constants
const char*       progname = "selectc";

//////////////////
// Main Program //
//////////////////
int main ( int argc, char **argv )
{
   FILE*    infile;               // Input file stream
   FILE*    outfile;              // Output file stream
   char     event[16384];         // Event buffer
   char     line[82];             // Current line
   char     filename[FILE_LEN];   // Name of the file to process
   char     outfilen[FILE_LEN];   // Name of the output file
   char     comment[COMM_LEN];    // Comment to search
   int      status;               // Processing status
   int      evtcount = 0;         // Event counter
   char*    pos;

   // Set output file name
   sprintf ( outfilen, "%s.out", progname );

   // Check input argument
   if ( argc < 3 )
   {
      // User input filename
      printf ( "Input file: " );
      fgets ( filename, FILE_LEN, stdin );
      *(strstr(filename,"\n"))='\0';
      printf ( "Search comment: " );
      fgets ( comment, COMM_LEN, stdin );
      
      // Remove new line characters
      if ( ( pos = strstr ( filename, "\n" ) ) ) *pos = '\0';
      if ( ( pos = strstr ( comment, "\n" ) ) ) *pos = '\0';
   }
   else
   {
      // Check validity of input arguments
      if ( strlen ( argv[1] ) > FILE_LEN )
      {
         fprintf ( stderr, "%s: Filename too long\n", progname );
         return 1;
      }
      if ( strlen ( argv[2] ) > COMM_LEN )
      {
         fprintf ( stderr, "%s: Comment too long\n", progname );
         return 1;
      }
      
      // Copy to variables
      strcpy ( filename, argv[1] );
      strcpy ( comment, argv[2] );
      
      // Check output file argument
      if ( argc == 4 )
         strcpy ( outfilen, argv[3] );
   }
   
   // Open input file
   infile = fopen ( filename, "r" );
   if ( infile == NULL )
   {
      fprintf ( stderr, "%s: Unable to open input file - <%s>\n",
            progname, filename );
      return 1;
   }
   
   // Open output file
   if ( strstr ( outfilen, "stdout" ) )
      outfile = stdout;
   else
   {
      outfile = fopen ( outfilen, "w" );
      if ( outfile == NULL )
      {
         fprintf ( stderr, "%s: Unable to start output file - %s\n",
               progname, filename );
         fclose ( infile );
         return 1;
      }
   }
   
   // Initialize event buffer
   event[0] = '\0';
   status = 0; // Set status 0 to indicate that event is not to be saved
   
   // Read file line by line
   while ( fgets ( line, 82, infile ) )
   {
      // Check if line marks the end of an event
      if ( strlen ( line ) < 80 || strspn ( line, " " ) > 79 )
      {
         // Add last line to event
         strcat( event, line );
         
         if ( status == 1 )
            fprintf ( outfile, "%s", event );
         //fflush ( stdout );
         
         
         // Reset event buffer
         event[0] = '\0';
         status = 0;
      }
      else
      {
         // Add line to event
         strcat( event, line );
         
         // Check if line is comment
         if ( line[79] == '3' )
         {
            // Check if comment line contains the string that we are searching
            if ( strstr ( line, comment ) ) 
            {
               status = 1; // Save this event
               evtcount++; // Increment event counter
            }
         }
      }
   }
   
   // Close output file
   fclose ( outfile );
   
   // Close input file
   fclose ( infile );
   
   if ( !strstr ( outfilen, "stdout" ) )
      printf ( "Found %d events.\nResults stored in %s\n", evtcount, outfilen );
   
   
   // Done
   return 0;
}
