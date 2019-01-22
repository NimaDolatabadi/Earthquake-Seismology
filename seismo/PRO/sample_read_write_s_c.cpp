// Test program to read and write an s-file using C++ language.
// The program will use the seisan fortran routines to read and write, 
// and the data is placed into a C structure. This C-struction is equivalent
// with the correspondiong REA.INC common block in Fortran. In c++, the 
// definitions are found in rea_c.h in INC but the definition of the variables
// are in rea.inc. All the c++ subroutines used are found in this file.
// 
//
//
// By Oyvind natvik, modified by jens havskov, November 2017
//



#include <stdio.h>
#include "rea_c.h"

int main(int argc, char *argv[])
{
   event_node_ *SFileData;		// Holds that data for an event read with MyReadClass.GetNextEvent().
   bool EndOfFile = false;
   int ReadStatus, WriteStatus,n;

   n=1;

// Check that one argument is given. This should be full path to the S-file.

   if (argc != 2) {
      printf("Error: Please give full path to S-File as argument.\n\n");
      return 1;
   }

// Create an instance of the reader class and open the S-file.

   ReadSfile MyReadClass;
   if (!MyReadClass.OpenFile(argv[1])) {
      printf("Error: Unable to open S-File for reading.\n\n");
      return 1;
   }

// Create an instance of the writer class and open a write file.

   WriteSfile MyWriteClass;
   if (!MyWriteClass.OpenFile((char*)"sample_read_write_s_c.out")) {
      printf("Error: Unable to open file 'sample_read_write_s_c.out' for writing.\n\n");
      MyReadClass.CloseFile();
      return 1;
   }

// Read and process all events from S-file in a loop.

   while (!EndOfFile) {

// Try to read an event from S-file.
      SFileData = MyReadClass.GetNextEvent(&ReadStatus);

// Handle status from GetNextEvent.

      if (SFileData) {

// We have succcesfully read an event.

         n=n+1;

// Print out the ID line.

         printf("ID Line: %s\n", SFileData->id_line);

// Print some info

   printf("Number of headers: %d \n",SFileData->nhead);
   printf("Number of records: %d \n",SFileData->nrecord);
   printf("Number of stations: %d \n",SFileData->nstat);

// make a pointer (tm) to db_time structure which contains origin time.

   db_time *tm = &SFileData->hypocenters.first->time;      

// print origin time    

   printf("Hypocenter time: %d.%d.%d %d:%d:%f \n", tm->year, tm->month,
   tm->day, tm->hour, tm->minute, tm->second);

   printf("year and month: %d  %d \n",SFileData->hypocenters.first->time.year,SFileData->hypocenters.first->time.month);

// number of text lines

   printf("Number of text lines: %d \n",SFileData->comments.numlines);



// Write the event to the out-file.

         if (!MyWriteClass.WriteEvent(SFileData, &WriteStatus)) {
            if (WriteStatus == 1) printf("Write Error: Function 'rea_event_out' returned an error.\n");
            if (WriteStatus == 2) printf("Write Error: Unable to write to S-file.\n");
         }

         // Free up the allocated memory for the event.
         FreeNode(SFileData);
      }

// Check for end-of-file or errors reading an event.

      if (ReadStatus) {
         if (ReadStatus == 1) printf("Read Error: Function rea_event_in() failed.\n");
         if (ReadStatus == 2) printf("Read Error: Memory allocation failed, out of memory.\n");
         if (ReadStatus == 3) printf("Read Warning: Function rea_event_in() returned a non-fatal error.\n");
         if (ReadStatus == 5) EndOfFile = true;
      }
   }

// Close the files.

   MyReadClass.CloseFile();
   MyWriteClass.CloseFile();
   printf("Number of events: %d \n",n);
   printf("Output file name is sample_read_write_s_c.out \n");
   return 0;
}



/////////////////////////////////////////////////////////////////////////////
//    subroutines
/////////////////////////////////////////////////////////////////////////////
//

using namespace std;

#include <new>
#include <stdio.h>
#include <string.h>

#include "rea_c.h"


// **********************************************************************************************************
// Functions for the ReadSfile class:
// **********************************************************************************************************

// ********************************************
// Constructor function.
// ********************************************
ReadSfile::ReadSfile()
{
   FileOpen = EndOfFile = false;
}

// ********************************************
// Open the S-file.
// ********************************************
bool ReadSfile::OpenFile(char FilePath[])
{
   // Check if a file is already open.
   if (FileOpen) return false;

   // Open the S-file.
   Sfile = fopen(FilePath, "r");

   // Return if file could not be opened.
   if (!Sfile) return false;

   // File has been opened. Set file-open indicator.
   FileOpen = true;

   return true;
}

// *****************************************************************
// Read and return the next event in the S-file. On success, the
// function returns a pointer to an allocated event node object.
// Status values are returned in 'status':
// 0 - Function has returned a pointer to an event node.
// 1 - Error: Function rea_event_in() failed.
// 2 - Error: Memory allocation failed, out of memory.
// 3 - Warning; Function rea_event_in() returned a non-fatal error.
// 5 - Error: EOF has been reached, no more events available.
//
// Note:
// The allocated node must be freed with function FreeNode().
// *****************************************************************
event_node_* ReadSfile::GetNextEvent(int *status)
{
   char line[128];
   int LineLength;
   event_node_ *Node;
   sint unit, read_code, all;
   char data[MAX_SFILE_LINES][80];

   // Initialize variables.
   *status = 0;

   // Check if we are already at end-of-file.
   if (EndOfFile) { *status = 5; return 0; }

   // Create new pointers to fortran common blocks.
   comblck_hyp4* hyp4 = &hyp4_;
   comblck_rea4* rea4 = &rea4_;

   // Clear the fortran common blocks.
   ClearCommonBlocks();

   // Read next event into the 'data' array.
   while (!EndOfFile) {
      if (fgets(line, sizeof(line), Sfile)) {
         // A line has been read from the file.
         LineLength = strlen(line);
         if (IsDataLineEmpty(line, LineLength)) {
            // We have read an empty line.
            if (rea4->rea_nrecord) break;
         } else {
            // Line has data. Add line to data array.
            memset(data[rea4->rea_nrecord], 32, 80);
            memcpy(data[rea4->rea_nrecord], line, LineLength - 1);
            rea4->rea_nrecord++;
            if (LineLength == 81 && line[79] != 32 && line[79] != '4') rea4->rea_nhead++;
         }
      } else {
         EndOfFile = true;
      }
   }

   // Check if we have reached end-of-file, and no data has been read.
   if (EndOfFile && !rea4->rea_nrecord) { *status = 5; return 0; }

   // Read from 'data' array into common blocks. On linux,
   // the LC_NUMERIC locale setting must be temporarily set to
   // POSIX to avoid problems with decimal point when using
   // a locale that has a decimal point other than '.'(dot).
   #ifdef OS_LINUX
   char *locale = setlocale(LC_NUMERIC, NULL); setlocale(LC_NUMERIC, "POSIX");
   #endif
   rea_event_in_(&(unit=0), &(all=1), (char*)data, &read_code, sizeof(data));
   #ifdef OS_LINUX
   setlocale(LC_NUMERIC, locale);
   #endif

   // Handle errors and warnings from 'rea_event_in'.
   if (read_code) {
      if (read_code == 2) *status = 3;
      if (read_code == 1 || read_code == 3) { *status = 1; return 0; }
   }

   // Abort if no header lines have been read.
   if (!rea4->rea_nhead) { *status = 1; return 0; }

   // Abort if no hypocenters have been read.
   if (!rea4->rea_nhyp) { *status = 1; return 0; }

   // Make sure first hypocenter at least has a year.
   if (hyp4->hyp_year[0] == -999) { *status = 1; return 0; }

   // Create the node.
   if (!(Node = CreateNodeFromCommonBlocks())) { *status = 2; return 0; }

   // Return pointer to new node.
   return Node;
}

// ********************************************
// Close the S-file.
// ********************************************
void ReadSfile::CloseFile()
{
   // Close the file
   fclose(Sfile);

   // Reset the class.
   FileOpen = EndOfFile = false;
}




// **********************************************************************************************************
// Functions for the WriteSfile class:
// **********************************************************************************************************

// ********************************************
// Constructor function.
// ********************************************
WriteSfile::WriteSfile()
{
   FileOpen = false;
}

// ********************************************
// Open the S-file.
// ********************************************
bool WriteSfile::OpenFile(char FilePath[])
{
   // Check if a file is already open.
   if (FileOpen) return false;

   // Open the S-file.
   Sfile = fopen(FilePath, "w+");

   // Return if file could not be opened.
   if (!Sfile) return false;

   // File has been opened. Set file-open indicator.
   FileOpen = true;

   return true;
}

// *************************************************************************
// This function writes an evemnt from an event node. Function returns
// 'false' if event could not be written.
//
// One of the following status values are also returned in 'status':
// 0 - No error occurred.
// 1 - Error: Function 'rea_event_out' failed.
// 2 - Unable to write to S-file.
// *************************************************************************
bool WriteSfile::WriteEvent(event_node_ *Node, int *status)
{
   char line[81];
   int index, i, i2, ws;
   char data[MAX_SFILE_LINES][80];
   signed int unit, write_code, all;
   phase_ *PhaseData;
   hypocenter_ *HypData;

   // Initialize variables.
   *status = 0;

   // Create new pointers to fortran common blocks.
   comblck_hyp1* hyp1 = &hyp1_; comblck_hyp4* hyp4 = &hyp4_;
   comblck_hyp5* hyp5 = &hyp5_; comblck_rea1* rea1 = &rea1_;
   comblck_rea2* rea2 = &rea2_; comblck_rea3* rea3 = &rea3_;
   comblck_rea4* rea4 = &rea4_; comblck_rea5* rea5 = &rea5_;
   comblck_rea8* rea8 = &rea8_;

   // Clear the common blocks.
   ClearCommonBlocks();

   // Copy all information from event node to common blocks.
   // Copy number of stations.
   rea4->rea_nstat = Node->nstat;

   // Copy number of spectra.
   rea4->rea_nspec = Node->nspec;

   // Copy action parameter.
   fstrcpy(rea3->rea_action, Node->action, 3);

   // Copy locality.
   fstrcpy(rea4->rea_locality, Node->locality, 68);

   // Copy macro data.
   rea4->rea_nmacro = Node->macros.numlines;
   for (int i=0;i<rea4->rea_nmacro;i++) {
      memcpy(rea4->rea_macro[i], Node->macros.lines+i*80, 80);
   }

   // Copy wave filenames.
   rea4->rea_nwav = Node->wavefiles.numlines;
   for (int i=0;i<rea4->rea_nwav;i++) {
      memcpy(rea4->rea_wav[i], Node->wavefiles.lines+i*80, 80);
   }

   // Copy faults.
   rea4->rea_nfault = Node->faults.numlines;
   for (int i=0;i<rea4->rea_nfault;i++) {
      memcpy(rea4->rea_fault[i], Node->faults.lines+i*80, 80);
   }

   // Copy comment lines.
   rea4->rea_ncomment = Node->comments.numlines;
   for (int i=0;i<rea4->rea_ncomment;i++) {
      memcpy(rea4->rea_comment[i], Node->comments.lines+i*80, 80);
   }

   // Copy ID-line.
   fstrcpy(rea4->rea_id_line, Node->id_line, 80);

   // Copy phases from event node.
   rea4->rea_nphase = Node->phases.nphase;
   if (rea4->rea_nphase) {
      PhaseData = Node->phases.first;
      for (index=0;index<Node->phases.nphase;index++) {
         // Copy data for one phase.
         fstrcpy(rea5->rea_stat[index], PhaseData->agency, 5);
         fstrcpy(rea4->rea_comp[index], PhaseData->comp, 4);
         fstrcpy(rea2->rea_co[index], PhaseData->co, 2);
         fstrcpy(rea8->rea_phase[index], PhaseData->phase, 8);
         rea1->rea_onset[index] = PhaseData->onset;
         rea1->rea_weight_in[index] = PhaseData->weight_in;
         fstrcpy(rea2->rea_weight_out[index], PhaseData->weight_out, 2);
         rea1->rea_polarity[index] = PhaseData->polarity;
         rea4->rea_year[index] = PhaseData->year;
         rea4->rea_month[index] = PhaseData->month;
         rea4->rea_day[index] = PhaseData->day;
         rea4->rea_hour[index] = PhaseData->hour;
         rea4->rea_min[index] = PhaseData->min;
         rea4->rea_sec[index] = PhaseData->sec;
         rea8->rea_abs_time[index] = PhaseData->abs_time;
         rea4->rea_coda[index] = PhaseData->coda;
         rea4->rea_amp[index] = PhaseData->amp;
         rea4->rea_per[index] = PhaseData->per;
         rea4->rea_baz_obs[index] = PhaseData->baz_obs;
         rea4->rea_baz_cal[index] = PhaseData->baz_cal;
         rea4->rea_vel[index] = PhaseData->vel;
         rea4->rea_ain[index] = PhaseData->ain;
         rea4->rea_baz_res[index] = PhaseData->baz_res;
         rea4->rea_res[index] = PhaseData->res;
         rea4->rea_dist[index] = PhaseData->dist;
         rea4->rea_az[index] = PhaseData->az;
         fstrcpy(rea4->rea_auto[index], PhaseData->autoproc, 20);
         rea4->rea_moment[index] = PhaseData->spectral.moment;
         rea4->rea_sdrop[index] = PhaseData->spectral.sdrop;
         rea4->rea_omega0[index] = PhaseData->spectral.omega0;
         rea4->rea_cornerf[index] = PhaseData->spectral.cornerf;
         rea4->rea_radius[index] = PhaseData->spectral.radius;
         rea4->rea_swin[index] = PhaseData->spectral.swin;
         rea4->rea_vs[index] = PhaseData->spectral.vs;
         rea4->rea_vp[index] = PhaseData->spectral.vp;
         rea4->rea_q0[index] = PhaseData->spectral.q0;
         rea4->rea_qalpha[index] = PhaseData->spectral.qalpha;
         rea4->rea_kappa[index] = PhaseData->spectral.kappa;
         rea4->rea_density[index] = PhaseData->spectral.density;
         rea4->rea_slope[index] = PhaseData->spectral.slope;
         rea4->rea_geo_dist[index] = PhaseData->spectral.geo_dist;
         rea4->rea_mc[index] = PhaseData->magnitudes.mc;
         rea4->rea_ml[index] = PhaseData->magnitudes.ml;
         rea4->rea_mb[index] = PhaseData->magnitudes.mb;
         rea4->rea_ms[index] = PhaseData->magnitudes.ms;
         rea4->rea_mw[index] = PhaseData->magnitudes.mw;
         // Move to next phase.
         PhaseData = PhaseData->next;
      }
   }

   // Copy spectral averages.
   rea4->rea_av_moment = Node->spectral_avg.av_moment;
   rea4->rea_av_sdrop = Node->spectral_avg.av_sdrop;
   rea4->rea_av_omega0 = Node->spectral_avg.av_omega0;
   rea4->rea_av_cornerf = Node->spectral_avg.av_cornerf;
   rea4->rea_av_radius = Node->spectral_avg.av_radius;
   rea4->rea_av_swin = Node->spectral_avg.av_swin;
   rea4->rea_av_mw = Node->spectral_avg.av_mw;
   rea4->rea_av_slope = Node->spectral_avg.av_slope;

   // Copy hypocenter data.
   rea4->rea_nmag = Node->hypocenters.nmag;
   for (int i=0;i<rea4->rea_nmag;i++) {
      hyp4->hyp_mag_all[i] = Node->hypocenters.mag_all[i].mag;
      hyp1->hyp_mag_type_all[i] = Node->hypocenters.mag_all[i].type;
      fstrcpy(hyp5->hyp_mag_agency_all[i], Node->hypocenters.mag_all[i].agency, 5);
   }
   rea4->rea_nhyp = Node->hypocenters.nhyp;
   if (rea4->rea_nhyp) {
      HypData = Node->hypocenters.first;
      for (index=0;index<Node->hypocenters.nhyp;index ++) {
         hyp4->hyp_year[index] = HypData->time.year;
         hyp4->hyp_month[index] = HypData->time.month;
         hyp4->hyp_day[index] = HypData->time.day;
         hyp4->hyp_hour[index] = HypData->time.hour;
         hyp4->hyp_min[index] = HypData->time.minute;
         hyp4->hyp_sec[index] = HypData->time.second;
         hyp1->hyp_model[index] = HypData->model;
         hyp1->hyp_dist_id[index] = HypData->dist_id;
         hyp1->hyp_type[index] = HypData->type;
         hyp1->hyp_fix_org[index] = HypData->fix_org;
         hyp4->hyp_lat[index] = HypData->lat;
         hyp4->hyp_lon[index] = HypData->lon;
         hyp4->hyp_depth[index] = HypData->depth;
         hyp1->hyp_depth_flag[index] = HypData->depth_flag;
         hyp1->hyp_epi_flag[index] = HypData->epi_flag;
         fstrcpy(hyp5->hyp_agency[index], HypData->agency, 5);
         hyp4->hyp_nstat[index] = HypData->nstat;
         hyp4->hyp_rms[index] = HypData->rms;
         hyp1->hyp_high_accuracy[index] = HypData->high_accuracy;
         hyp1->hyp_error[index] = HypData->error;
         hyp4->hyp_gap[index] = HypData->gap;
         hyp4->hyp_sec_err[index] = HypData->sec_err;
         hyp4->hyp_lat_err[index] = HypData->lat_err;
         hyp4->hyp_lon_err[index] = HypData->lon_err;
         hyp4->hyp_depth_err[index] = HypData->depth_err;
         hyp4->hyp_cov[index][0] = HypData->cov[0];
         hyp4->hyp_cov[index][1] = HypData->cov[1];
         hyp4->hyp_cov[index][2] = HypData->cov[2];
         fstrcpy(hyp4->hyp_auto[index], HypData->autoproc, 20);
         // Copy magnitude data (MW).
         i2 = 0;
         for (i=0;i<HypData->magnitudes.MW.nmag;i++) {
            hyp1->hyp_mag_type[index][i2] = 'W';
            hyp4->hyp_mag[index][i2] = HypData->magnitudes.MW.mag[i];
            fstrcpy(hyp5->hyp_mag_agency[index][i2], HypData->magnitudes.MW.mag_agency[i], 5);
            i2++;
         }
         for (i=0;i<HypData->magnitudes.ML.nmag;i++) {
            hyp1->hyp_mag_type[index][i2] = 'L';
            hyp4->hyp_mag[index][i2] = HypData->magnitudes.ML.mag[i];
            fstrcpy(hyp5->hyp_mag_agency[index][i2], HypData->magnitudes.ML.mag_agency[i], 5);
            i2++;
         }
         for (i=0;i<HypData->magnitudes.MC.nmag;i++) {
            hyp1->hyp_mag_type[index][i2] = 'C';
            hyp4->hyp_mag[index][i2] = HypData->magnitudes.MC.mag[i];
            fstrcpy(hyp5->hyp_mag_agency[index][i2], HypData->magnitudes.MC.mag_agency[i], 5);
            i2++;
         }
         for (i=0;i<HypData->magnitudes.Mb.nmag;i++) {
            hyp1->hyp_mag_type[index][i2] = 'b';
            hyp4->hyp_mag[index][i2] = HypData->magnitudes.Mb.mag[i];
            fstrcpy(hyp5->hyp_mag_agency[index][i2], HypData->magnitudes.Mb.mag_agency[i], 5);
            i2++;
         }
         for (i=0;i<HypData->magnitudes.MB.nmag;i++) {
            hyp1->hyp_mag_type[index][i2] = 'B';
            hyp4->hyp_mag[index][i2] = HypData->magnitudes.MB.mag[i];
            fstrcpy(hyp5->hyp_mag_agency[index][i2], HypData->magnitudes.MB.mag_agency[i], 5);
            i2++;
         }
         for (i=0;i<HypData->magnitudes.Ms.nmag;i++) {
            hyp1->hyp_mag_type[index][i2] = 's';
            hyp4->hyp_mag[index][i2] = HypData->magnitudes.Ms.mag[i];
            fstrcpy(hyp5->hyp_mag_agency[index][i2], HypData->magnitudes.Ms.mag_agency[i], 5);
            i2++;
         }
         for (i=0;i<HypData->magnitudes.MS.nmag;i++) {
            hyp1->hyp_mag_type[index][i2] = 'S';
            hyp4->hyp_mag[index][i2] = HypData->magnitudes.MS.mag[i];
            fstrcpy(hyp5->hyp_mag_agency[index][i2], HypData->magnitudes.MS.mag_agency[i], 5);
            i2++;
         }

         // Move to next hypocenter.
         HypData = HypData->next;
      }
   }

   // Create S-file contents from the data in the common blocks.
   // Function rea_event_out will write S-file lines into 'data' array.
   // The LC_NUMERIC locale setting must be temporarily set to 'POSIX'
   // to avoid problems with decimal point when using a locale that has
   // a desimal point other than '.'(dot) is used.
   rea4_.rea_err_unit = 1;
   memset(data, 32, sizeof(data));
   #ifdef OS_LINUX
   locale = setlocale(LC_NUMERIC, NULL); setlocale(LC_NUMERIC, "POSIX");
   #endif
   rea_event_out_(&(unit=0), &(all=1), (char*)data, &write_code, sizeof(data));
   #ifdef OS_LINUX
   setlocale(LC_NUMERIC, locale);
   #endif
   if (write_code) { *status = 1; return false; }

   // Write the event to the S-file. Abort if a write error occur.
   for (index=0;index<rea4_.rea_nrecord;index++) {
      memcpy(line, data[index], 80); UnPadString(line, sizeof(line));
      fputs(line, Sfile); fputs("\n", Sfile);
   }
   //ws = fputs("\n", Sfile);
   if (ws == EOF) { *status = 2; return false; }

   return true;
}





// **********************************************************************************************************
// Support functions:
// **********************************************************************************************************

// ********************************************
// Close the S-file.
// ********************************************
void WriteSfile::CloseFile()
{
   // Close the file
   fclose(Sfile);

   // Initialize the class.
   WriteSfile();
}

// *****************************************************
// This function deallocates a node.
// *****************************************************
void FreeNode(event_node_ *Node)
{
   // Check for nullpointer to node.
   if (!Node) return;

   // Free macrolines.
   if (Node->macros.numlines) delete [] Node->macros.lines;

   // Free wave file names.
   if (Node->wavefiles.numlines) delete [] Node->wavefiles.lines;

   // Free fault lines.
   if (Node->faults.numlines) delete [] Node->faults.lines;

   // Free comment lines.
   if (Node->comments.numlines) delete [] Node->comments.lines;

   // Free all phase structures.
   struct phase_ *Phase, *NextPhase;
   if (Node->phases.nphase) {
      Phase = Node->phases.first;
      while (Phase) {
         NextPhase = Phase->next;
         delete Phase;
         Phase = NextPhase;
      }
   }

   // Free hypocenter magnitudes.
   if (Node->hypocenters.mag_all) delete[] Node->hypocenters.mag_all;

   // Free all hypocenter structures.
   struct hypocenter_ *Hyp, *NextHyp;
   if (Node->hypocenters.nhyp) {
      Hyp = Node->hypocenters.first;
      while (Hyp) {
         NextHyp = Hyp->next;
         delete Hyp;
         Hyp = NextHyp;
      }
   }

   // Free up the node itself.
   delete Node;
}

// *********************************************************************
// This function creates an event node from the current contents of the
// fortran common blocks. Memory for the node is allocated here.
// Function returns a pointer to the new node. A NULL pointer is
// returned if memory allocation failed.
// *********************************************************************
event_node_* CreateNodeFromCommonBlocks()
{
   bool OutOfMem;
   int index, i, i2;
   event_node_ *Node;
   phase_ *PhaseData, *LastPhase = 0;
   hypocenter_ *HypData, *LastHypo = 0;

   // Create new pointers to fortran common blocks.
   // This is done to simplify debugging in Qt Creator.
   comblck_hyp1* hyp1 = &hyp1_; comblck_hyp4* hyp4 = &hyp4_;
   comblck_hyp5* hyp5 = &hyp5_; comblck_rea1* rea1 = &rea1_;
   comblck_rea2* rea2 = &rea2_; comblck_rea3* rea3 = &rea3_;
   comblck_rea4* rea4 = &rea4_; comblck_rea5* rea5 = &rea5_;
   comblck_rea8* rea8 = &rea8_;

   // Allocate memory for a new db node.
   if (!(Node = new (nothrow) event_node_)) return 0;

   // Clear memory for the new db node.
   memset(Node, 0, sizeof(event_node_));

   // Copy all information from common blocks
   // to new db node (except for the filename).
   // First, copy number of header lines.
   Node->nhead = rea4->rea_nhead;

   // Copy number of lines.
   Node->nrecord = rea4->rea_nrecord;

   // Copy number of stations.
   Node->nstat = rea4->rea_nstat;

   // Copy number of spectra.
   Node->nspec = rea4->rea_nspec;

   // Copy action parameter.
   memcpy(Node->action, rea3->rea_action, 3);
   UnPadString(Node->action, sizeof(Node->action));

   // Copy locality.
   memcpy(Node->locality, rea4->rea_locality, 68);
   UnPadString(Node->locality, sizeof(Node->locality));

   // Copy macro data (type 2 lines).
   OutOfMem = false;
   Node->macros.numlines = rea4->rea_nmacro;
   if (Node->macros.numlines) {
      if ((Node->macros.lines = new (nothrow) char [rea4->rea_nmacro*80])) {
         for (int i=0;i<rea4->rea_nmacro;i++) {
            memcpy((Node->macros.lines+i*80), rea4->rea_macro[i], 80);
         }
      } else {
         OutOfMem = true;
      }
   }

   // Copy wave filenames (type 6 lines).
   Node->wavefiles.numlines = rea4->rea_nwav;
   if (Node->wavefiles.numlines && !OutOfMem) {
      if ((Node->wavefiles.lines = new (nothrow) char [rea4->rea_nwav*80])) {
         for (int i=0;i<rea4->rea_nwav;i ++) {
            memcpy((Node->wavefiles.lines+i*80), rea4->rea_wav[i], 80);
         }
      } else {
         OutOfMem = true;
      }
   }

   // Copy faults (type F lines).
   Node->faults.numlines = rea4->rea_nfault;
   if (Node->faults.numlines && !OutOfMem) {
      if ((Node->faults.lines = new (nothrow) char [rea4->rea_nfault*80])) {
         for (int i=0;i<rea4->rea_nfault;i ++) {
            memcpy((Node->faults.lines+i*80), rea4->rea_fault[i], 80);
         }
      } else {
         OutOfMem = true;
      }
   }

   // Copy comment lines (type 3 lines).
   Node->comments.numlines = rea4->rea_ncomment;
   if (Node->comments.numlines && !OutOfMem) {
      if ((Node->comments.lines = new (nothrow) char [rea4->rea_ncomment*80])) {
         for (int i=0;i<rea4->rea_ncomment;i ++) {
            memcpy((Node->comments.lines+i*80), rea4->rea_comment[i], 80);
         }
      } else {
         OutOfMem = true;
      }
   }

   // Copy ID-line.
   memcpy(Node->id_line, rea4->rea_id_line, 80);

   // Copy phase data to event node.
   LastPhase = 0;
   Node->phases.nphase = rea4->rea_nphase;
   for (index=0;index<Node->phases.nphase;index ++) {
      // Copy data for phase pointed to by 'index'.
      if (!OutOfMem && (PhaseData = new (nothrow) phase_)) {
         memset(PhaseData, 0, sizeof(phase_));
         memcpy(PhaseData->agency, rea5->rea_stat[index], 5);
         UnPadString(PhaseData->agency, 6);
         memcpy(PhaseData->comp, rea4->rea_comp[index], 4);
         UnPadString(PhaseData->comp, 5);
         memcpy(PhaseData->co, rea2->rea_co[index], 2);
         memcpy(PhaseData->phase, rea8->rea_phase[index], 8);
         UnPadString(PhaseData->phase, 9);
         PhaseData->onset = rea1->rea_onset[index];
         PhaseData->weight_in = rea1->rea_weight_in[index];
         memcpy(PhaseData->weight_out, rea2->rea_weight_out[index], 2);
         PhaseData->polarity = rea1->rea_polarity[index];
         PhaseData->year = rea4->rea_year[index];
         PhaseData->month = rea4->rea_month[index];
         PhaseData->day = rea4->rea_day[index];
         PhaseData->hour = rea4->rea_hour[index];
         PhaseData->min = rea4->rea_min[index];
         PhaseData->sec = rea4->rea_sec[index];
         PhaseData->abs_time = rea8->rea_abs_time[index];
         PhaseData->coda = rea4->rea_coda[index];
         PhaseData->amp = rea4->rea_amp[index];
         PhaseData->per = rea4->rea_per[index];
         PhaseData->baz_obs = rea4->rea_baz_obs[index];
         PhaseData->baz_cal = rea4->rea_baz_cal[index];
         PhaseData->vel = rea4->rea_vel[index];
         PhaseData->ain = rea4->rea_ain[index];
         PhaseData->baz_res = rea4->rea_baz_res[index];
         PhaseData->res = rea4->rea_res[index];
         PhaseData->dist = rea4->rea_dist[index];
         PhaseData->az = rea4->rea_az[index];
         memcpy(PhaseData->autoproc, rea4->rea_auto[index], 20);
         UnPadString(PhaseData->autoproc, 21);
         PhaseData->spectral.moment = rea4->rea_moment[index];
         PhaseData->spectral.sdrop = rea4->rea_sdrop[index];
         PhaseData->spectral.omega0 = rea4->rea_omega0[index];
         PhaseData->spectral.cornerf = rea4->rea_cornerf[index];
         PhaseData->spectral.radius = rea4->rea_radius[index];
         PhaseData->spectral.swin = rea4->rea_swin[index];
         PhaseData->spectral.vs = rea4->rea_vs[index];
         PhaseData->spectral.vp = rea4->rea_vp[index];
         PhaseData->spectral.q0 = rea4->rea_q0[index];
         PhaseData->spectral.qalpha = rea4->rea_qalpha[index];
         PhaseData->spectral.kappa = rea4->rea_kappa[index];
         PhaseData->spectral.density = rea4->rea_density[index];
         PhaseData->spectral.slope = rea4->rea_slope[index];
         PhaseData->spectral.geo_dist = rea4->rea_geo_dist[index];
         PhaseData->magnitudes.mc = rea4->rea_mc[index];
         PhaseData->magnitudes.ml = rea4->rea_ml[index];
         PhaseData->magnitudes.mb = rea4->rea_mb[index];
         PhaseData->magnitudes.ms = rea4->rea_ms[index];
         PhaseData->magnitudes.mw = rea4->rea_mw[index];
         PhaseData->next = (phase_*)0;
         // Add phase to event node.
         if (!index) {
            Node->phases.first = PhaseData;
         } else {
            LastPhase->next = PhaseData;
         }
         LastPhase = PhaseData;
      } else {
         OutOfMem = true;
      }
   }

   // Copy spectral averages to event node.
   Node->spectral_avg.av_moment = rea4->rea_av_moment;
   Node->spectral_avg.av_sdrop = rea4->rea_av_sdrop;
   Node->spectral_avg.av_omega0 = rea4->rea_av_omega0;
   Node->spectral_avg.av_cornerf = rea4->rea_av_cornerf;
   Node->spectral_avg.av_radius = rea4->rea_av_radius;
   Node->spectral_avg.av_swin = rea4->rea_av_swin;
   Node->spectral_avg.av_mw = rea4->rea_av_mw;
   Node->spectral_avg.av_slope = rea4->rea_av_slope;

   // Copy hypocenter data to event node.
   Node->hypocenters.nmag = rea4->rea_nmag;
   if (rea4->rea_nmag) {
      if (!OutOfMem && (Node->hypocenters.mag_all = new (nothrow) mag_info [rea4->rea_nmag])) {
         memset(Node->hypocenters.mag_all, 0, sizeof(mag_info)*rea4->rea_nmag);
         for (int i=0;i<Node->hypocenters.nmag;i ++) {
            Node->hypocenters.mag_all[i].mag = hyp4->hyp_mag_all[i];
            Node->hypocenters.mag_all[i].type = hyp1->hyp_mag_type_all[i];
            memcpy(Node->hypocenters.mag_all[i].agency, hyp5->hyp_mag_agency_all[i], 5);
         }
      } else {
         OutOfMem = true;
      }
   }
   Node->hypocenters.nhyp = rea4->rea_nhyp;
   for (index=0;index<Node->hypocenters.nhyp;index ++) {
      // Copy data for hypocenter pointed to by 'index'.
      if (!OutOfMem && (HypData = new (nothrow) hypocenter_)) {
         memset(HypData, 0, sizeof(hypocenter_));
         HypData->time.year = hyp4->hyp_year[index];
         HypData->time.month = hyp4->hyp_month[index];
         HypData->time.day = hyp4->hyp_day[index];
         HypData->time.hour = hyp4->hyp_hour[index];
         HypData->time.minute = hyp4->hyp_min[index];
         HypData->time.second = hyp4->hyp_sec[index];
         HypData->model = hyp1->hyp_model[index];
         HypData->dist_id = hyp1->hyp_dist_id[index];
         HypData->type = hyp1->hyp_type[index]; //event-id
         HypData->fix_org = hyp1->hyp_fix_org[index];
         HypData->lat = hyp4->hyp_lat[index];
         HypData->lon = hyp4->hyp_lon[index];
         HypData->depth = hyp4->hyp_depth[index];
         HypData->depth_flag = hyp1->hyp_depth_flag[index];
         HypData->epi_flag = hyp1->hyp_epi_flag[index];
         memcpy(HypData->agency, hyp5->hyp_agency[index], 5);
         UnPadString(HypData->agency, 6);
         HypData->nstat = hyp4->hyp_nstat[index];
         HypData->rms = hyp4->hyp_rms[index];
         HypData->high_accuracy = hyp1->hyp_high_accuracy[index];
         HypData->error = hyp1->hyp_error[index];
         HypData->gap = hyp4->hyp_gap[index];
         HypData->sec_err = hyp4->hyp_sec_err[index];
         HypData->lat_err = hyp4->hyp_lat_err[index];
         HypData->lon_err = hyp4->hyp_lon_err[index];
         HypData->depth_err = hyp4->hyp_depth_err[index];
         HypData->cov[0] = hyp4->hyp_cov[index][0];
         HypData->cov[1] = hyp4->hyp_cov[index][1];
         HypData->cov[2] = hyp4->hyp_cov[index][2];
         memcpy(HypData->autoproc, hyp4->hyp_auto[index], 20);
         UnPadString(HypData->autoproc, 21);
         // Copy magnitude data.
         for (i=0;i<6;i++) {
            if (hyp1->hyp_mag_type[index][i] == 'W') {
               i2 = HypData->magnitudes.MW.nmag++;
               HypData->magnitudes.MW.mag[i2] = hyp4->hyp_mag[index][i];
               memcpy(HypData->magnitudes.MW.mag_agency[i2], hyp5->hyp_mag_agency[index][i], 5);
            }
            if (hyp1->hyp_mag_type[index][i] == 'L') {
               i2 = HypData->magnitudes.ML.nmag++;
               HypData->magnitudes.ML.mag[i2] = hyp4->hyp_mag[index][i];
               memcpy(HypData->magnitudes.ML.mag_agency[i2], hyp5->hyp_mag_agency[index][i], 5);
            }
            if (hyp1->hyp_mag_type[index][i] == 'C') {
               i2 = HypData->magnitudes.MC.nmag++;
               HypData->magnitudes.MC.mag[i2] = hyp4->hyp_mag[index][i];
               memcpy(HypData->magnitudes.MC.mag_agency[i2], hyp5->hyp_mag_agency[index][i], 5);
            }
            if (hyp1->hyp_mag_type[index][i] == 'b') {
               i2 = HypData->magnitudes.Mb.nmag++;
               HypData->magnitudes.Mb.mag[i2] = hyp4->hyp_mag[index][i];
               memcpy(HypData->magnitudes.Mb.mag_agency[i2], hyp5->hyp_mag_agency[index][i], 5);
            }
            if (hyp1->hyp_mag_type[index][i] == 'B') {
               i2 = HypData->magnitudes.MB.nmag++;
               HypData->magnitudes.MB.mag[i2] = hyp4->hyp_mag[index][i];
               memcpy(HypData->magnitudes.MB.mag_agency[i2], hyp5->hyp_mag_agency[index][i], 5);
            }
            if (hyp1->hyp_mag_type[index][i] == 's') {
               i2 = HypData->magnitudes.Ms.nmag++;
               HypData->magnitudes.Ms.mag[i2] = hyp4->hyp_mag[index][i];
               memcpy(HypData->magnitudes.Ms.mag_agency[i2], hyp5->hyp_mag_agency[index][i], 5);
            }
            if (hyp1->hyp_mag_type[index][i] == 'S') {
               i2 = HypData->magnitudes.MS.nmag++;
               HypData->magnitudes.MS.mag[i2] = hyp4->hyp_mag[index][i];
               memcpy(HypData->magnitudes.MS.mag_agency[i2], hyp5->hyp_mag_agency[index][i], 5);
            }
         }
         HypData->next = (hypocenter_*)0;

         // Add hypocenter to event node.
         if (!index) {
            Node->hypocenters.first = HypData;
            LastHypo = HypData;
         } else {
            LastHypo->next = HypData;
            LastHypo = HypData;
         }
      } else {
         OutOfMem = true;
      }
   }

   // If we have run out of memory, free up all allocated node memory.
   if (OutOfMem) { FreeNode(Node); return 0; }

   // Return pointer to new node.
   return Node;
}

// *****************************************************************
// This function is used clear all information in the common blocks.
// *****************************************************************
void ClearCommonBlocks()
{
   // Initialize single int/float single values.
   rea4_.rea_nwav = 0;
   rea4_.rea_nmag = 0;
   rea4_.rea_nhyp = 0;
   rea4_.rea_nstat = 0;
   rea4_.rea_nspec = 0;
   rea4_.rea_nhead = 0;
   rea4_.rea_nmacro = 0;
   rea4_.rea_nfault = 0;
   rea4_.rea_nphase = 0;
   rea4_.rea_nrecord = 0;
   rea4_.rea_ncomment = 0;
   rea4_.rea_av_moment = -999.0;
   rea4_.rea_av_sdrop = -999.0;
   rea4_.rea_av_omega0 = -999.0;
   rea4_.rea_av_cornerf = -999.0;
   rea4_.rea_av_radius = -999.0;
   rea4_.rea_av_swin = -999.0;
   rea4_.rea_av_mw = -999.0;
   rea4_.rea_av_slope = -999.0;
   rea4_.rea_err_unit = 0;
   rea4_.rea_read_err = 0;
   rea4_.rea_write_err = 0;

   // Fill character variables zeros.
   memset(rea1_.rea_onset, 0, MAX_DATA/2);
   memset(rea1_.rea_polarity, 0, MAX_DATA/2);
   memset(rea1_.rea_weight_in, 0, MAX_DATA/2);
   memset(rea2_.rea_co, 0, MAX_DATA/2*2);
   memset(rea2_.rea_weight_out, 0, MAX_DATA/2*2);
   memset(rea3_.rea_action, 0, 3);
   memset(rea4_.rea_locality, 0, 68);
   memset(rea4_.rea_id_line, 0, 80);
   memset(rea4_.rea_macro, 0, 100*80);
   memset(rea4_.rea_macro, 0, 100*80);
   memset(rea4_.rea_wav, 0, 100*80);
   memset(rea4_.rea_fault, 0, 100*80);
   memset(rea4_.rea_comment, 0, 100*80);
   memset(rea4_.rea_comment, 0, 100*80);
   memset(rea4_.rea_comp, 0, MAX_DATA/2*4);
   memset(rea4_.rea_auto, 0, MAX_DATA/2*20);
   memset(rea5_.rea_stat, 0, MAX_DATA/2*5);
   memset(rea8_.rea_phase, 0, MAX_DATA/2*8);
   memset(hyp1_.hyp_mag_type_all, 0, 200);
   memset(hyp1_.hyp_model, 0, 100);
   memset(hyp1_.hyp_dist_id, 0, 100);
   memset(hyp1_.hyp_type, 0, 100);
   memset(hyp1_.hyp_fix_org, 0, 100);
   memset(hyp1_.hyp_depth_flag, 0, 100);
   memset(hyp1_.hyp_epi_flag, 0, 100);
   memset(hyp1_.hyp_mag_type, 0, 100*6);
   memset(hyp4_.hyp_auto, 0, 100*20);
   memset(hyp5_.hyp_mag_agency_all, 0, 200*5);
   memset(hyp5_.hyp_agency, 0, 100*5);
   memset(hyp5_.hyp_mag_agency, 0, 100*6*5);

   // Initialize int/float arrays.
   for (int index=0;index<MAX_DATA/2;index++) {
      rea4_.rea_year[index] = -999;
      rea4_.rea_month[index] = -999;
      rea4_.rea_day[index] = -999;
      rea4_.rea_hour[index] = -999;
      rea4_.rea_min[index] = -999;
      rea4_.rea_sec[index] = -999.0;
      rea4_.rea_coda[index] = -999.0;
      rea4_.rea_amp[index] = -999.0;
      rea4_.rea_per[index] = -999.0;
      rea4_.rea_baz_obs[index] = -999.0;
      rea4_.rea_baz_cal[index] = -999.0;
      rea4_.rea_vel[index] = -999.0;
      rea4_.rea_ain[index] = -999.0;
      rea4_.rea_baz_res[index] = -999.0;
      rea4_.rea_dist[index] = -999.0;
      rea4_.rea_az[index] = -999.0;
      rea4_.rea_res[index] = -999.0;
      rea4_.rea_moment[index] = -999.0;
      rea4_.rea_sdrop[index] = -999.0;
      rea4_.rea_omega0[index] = -999.0;
      rea4_.rea_cornerf[index] = -999.0;
      rea4_.rea_radius[index] = -999.0;
      rea4_.rea_swin[index] = -999.0;
      rea4_.rea_vs[index] = -999.0;
      rea4_.rea_vp[index] = -999.0;
      rea4_.rea_q_below_1hz[index] = -999.0;
      rea4_.rea_q0[index] = -999.0;
      rea4_.rea_qalpha[index] = -999.0;
      rea4_.rea_kappa[index] = -999.0;
      rea4_.rea_density[index] = -999.0;
      rea4_.rea_slope[index] = -999.0;
      rea4_.rea_mc[index] = -999.0;
      rea4_.rea_ml[index] = -999.0;
      rea4_.rea_mb[index] = -999.0;
      rea4_.rea_ms[index] = -999.0;
      rea4_.rea_mw[index] = -999.0;
      rea4_.rea_geo_dist[index] = -999.0;
      rea8_.rea_abs_time[index] = -999.0;
   }
   for (int index=0;index<100;index++) {
      hyp1_.hyp_high_accuracy[index] = 0;
      hyp1_.hyp_error[index] = 0;
      hyp4_.hyp_year[index] = -999;
      hyp4_.hyp_month[index] = -999;
      hyp4_.hyp_day[index] = -999;
      hyp4_.hyp_hour[index] = -999;
      hyp4_.hyp_min[index] = -999;
      hyp4_.hyp_nstat[index] = -999;
      hyp4_.hyp_sec[index] = -999.0;
      hyp4_.hyp_lat[index] = -999.0;
      hyp4_.hyp_lon[index] = -999.0;
      hyp4_.hyp_depth[index] = -999.0;
      hyp4_.hyp_rms[index] = -999.0;
      hyp4_.hyp_gap[index] = -999.0;
      hyp4_.hyp_sec_err[index] = -999.0;
      hyp4_.hyp_lat_err[index] = -999.0;
      hyp4_.hyp_lon_err[index] = -999.0;
      hyp4_.hyp_depth_err[index] = -999.0;
      hyp4_.hyp_cov[index][0] = -9.9e10;
      hyp4_.hyp_cov[index][1] = -9.9e10;
      hyp4_.hyp_cov[index][2] = -9.9e10;
      hyp4_.hyp_mag[index][0] = -999.0;
      hyp4_.hyp_mag[index][1] = -999.0;
      hyp4_.hyp_mag[index][2] = -999.0;
      hyp4_.hyp_mag[index][3] = -999.0;
      hyp4_.hyp_mag[index][4] = -999.0;
      hyp4_.hyp_mag[index][5] = -999.0;
   }
   for (int index=0;index<200;index++) {
      hyp4_.hyp_mag_all[index] = -999.0;
   }
}

// ********************************************************
// This functions converts a fortran string to a C++ string
// by adding a \0 terminator after the last used character
// in the string
// ********************************************************
void UnPadString(char *string, int strsize)
{
   // Start at end of string and find first
   // character that is not a space. Replace
   // the last space with a NULL terminator.
   bool done = false;
   int index = strsize-1;

   while (!done) {
      if (string[index] != 32 && string[index] != 0) {
         if (index == strsize-1) {
            string[index] = 0;
         } else {
            string[index+1] = 0;
         }
         done = true;
      }
      // Check if we have processed the whole string and string is empty.
      if (!done && !index) { string[index] = 0; done = true; }
      index --;
   }
}

// *********************************************************
// This function converts a char string to a fortran string
// by filling free space in a string with spaces. The string
// terminator (\0) is also overwritten with a space.
// *********************************************************
void PadString(char *string, int strsize)
{
   for (int i=strlen(string);i<=strsize-1;i++) string[i]=32;
}

// ********************************************************
// This function copies a source string into a dest string,
// and then converts the dest string to a fortran string.
// ********************************************************
void fstrcpy(char dest[], char *source, int strsize_dest)
{
   int length = strlen(source);
   memset((void*) dest, 0, (size_t) strsize_dest);
   if (length > strsize_dest) length = strsize_dest;
   if (length) strncpy(dest, source, length);
   if (length < strsize_dest) PadString(dest, strsize_dest);
}

// *****************************************************************
// This function checks if a line is empty. An empty line
// is here defined as a being a line with only spaces.
// ******************************************************************
bool IsDataLineEmpty(char Line[], int LineLength)
{
   for (int i=0;i<LineLength-1;i++) if (Line[i]!=32 && Line[i]!='\r' && Line[i]!='\n') return false;
   return true;
}
