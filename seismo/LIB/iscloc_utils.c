#include "iscloc.h"

/*#DOC  Title:																*/
/*#DOC    utils.c															*/
/*#DOC  Desc:																*/
/*#DOC   File with the following utility functions used by location:		*/
/*#DOC    print_sol															*/
/*#DOC    print_pha															*/
/*#DOC    calc_delta														*/
/*#DOC    calc_esaz															*/
/*#DOC    calc_dircos														*/
/*#DOC    dsort																*/
/*#DOC    read_time															*/
/*#DOC    write_time														*/
/*#DOC    split_time														*/
/*#DOC    join_time															*/
/*#DOC    add_to_error														*/
/*#DOC    handle_error														*/


/*#DOC  Title:																*/
/*#DOC    print_sol															*/
/*#DOC  Desc:																*/
/*#DOC   Prints one line with the current solution as a diagnostic.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    sp  - pointer to structure containing current solution.			*/
/*#DOC  Return:																*/
/*#DOC    None.																*/
/*#DOC  Uses:																*/
/*#DOC    write_time() from this file.										*/

void print_sol(struct sol_rec *sp)
{
	char timestr[24];

	fprintf(logfp,"%s ",write_time(sp->time,timestr));
	if (sp->error[2] != NULLVAL)
		fprintf(logfp,"%6.3f+%6.3f ",sp->lat,sp->error[2]);
	else
		fprintf(logfp,"%6.3f+%6s ",sp->lat,"");
	if (sp->error[3] != NULLVAL)
		fprintf(logfp,"%6.3f+%6.3f ",sp->lon,sp->error[3]);
	else
		fprintf(logfp,"%6.3f+%6s ",sp->lon,"");
	if (sp->error[4] != NULLVAL)
		fprintf(logfp,"%.0f+%4.1f ",sp->depth,sp->error[4]);
	else
		fprintf(logfp,"%.0f+%4s ",sp->depth,"");
	if (sp->sdobs != NULLVAL)
		fprintf(logfp,"sd=%3.1f\n",sp->sdobs);
	else
		fprintf(logfp,"sd=%3s\n","");

	return;
}

/*#DOC  Title:																*/
/*#DOC    print_pha															*/
/*#DOC  Desc:																*/
/*#DOC   Prints a table with all the phases for one event.					*/
/*#DOC  Input Arguments:													*/
/*#DOC    numphas - number of phases in p[].								*/
/*#DOC    p[] - array of phase structures.									*/
/*#DOC  Return:																*/
/*#DOC    None.																*/
/*#DOC  Uses:																*/
/*#DOC    write_time() from this file.										*/

void print_pha(int numphas, struct pha_rec p[])
{
	int i;
	int ndef;
	char timestr[24];
	struct pha_rec temp_pha;

	fprintf(logfp,"RDID      NET   STA     REP   ISC   TIME                  ");
	fprintf(logfp,"    DELTA FACT  WT       RESID       AMP  PER  MAG\n");

	ndef=0;
	for (i=0; i< numphas; i++){
		fprintf(logfp,"%-9d ",p[i].rdid);
		if (strcmp(p[i].net,""))
			fprintf(logfp,"%-5s ",p[i].net);
		else
			fprintf(logfp,"%5s ","");
		fprintf(logfp,"%-5s ",p[i].sta);
		if (p[i].comp)
			fprintf(logfp,"%c ",p[i].comp);
		else
			fprintf(logfp,"  ");
		fprintf(logfp,"%-5s ",p[i].rep_phase);
		fprintf(logfp,"%-5s ",p[i].phase);
		fprintf(logfp,"%23s ",write_time(p[i].time,timestr));
		fprintf(logfp,"%7.3f ",p[i].delta);
		if (p[i].purged) 
			fprintf(logfp,"PURGE ");
		else if (p[i].duplicate == 1) 
			fprintf(logfp,"DUPLI ");
		else
			fprintf(logfp,"%0.3f ",p[i].weight_factor);
		fprintf(logfp,"%0.4f ",p[i].weight);
		if (p[i].resid != NULLVAL)
			fprintf(logfp,"%+8.4f ",p[i].resid);
		else
			fprintf(logfp,"%8s ","");
		if (p[i].numamps>0){
			if (p[i].a[0].amp!=NULLVAL) 
				fprintf(logfp,"%8.1f ",p[i].a[0].amp);
			else
				fprintf(logfp,"%8s ","");
			if (p[i].a[0].per!=NULLVAL)
				fprintf(logfp,"%4.1f ",p[i].a[0].per);
			else
				fprintf(logfp,"%4s ","");
		}
		else
			fprintf(logfp,"%8s %4s ","","");
		if (p[i].bodymag)
			fprintf(logfp,"%4.2fb ",p[i].bodymag);
		if (p[i].surfmag)
			fprintf(logfp,"%4.2fS ",p[i].surfmag);

		fprintf(logfp,"\n");
		if (p[i].weight > 0) ndef++;
	}
	fprintf(logfp,"ndef=%d\n",ndef);

	return;
}

/*#DOC  Title:																*/
/*#DOC    calc_delta														*/
/*#DOC  Desc:																*/
/*#DOC    Calculates delta between two points given thier direction cosines.*/
/*#DOC  Input Arguments:													*/
/*#DOC    sta_dircos[] - array of 6 direction cosines for station.			*/
/*#DOC    h_dircos[] - array of 6 direction cosines for hypocentre.			*/
/*#DOC  Return:																*/
/*#DOC    delta as a double.												*/

/*	Calculate distance in radians between station and epicentre.	*/
/*	Reference: Bullen K.E., 1963. An Introduction to the Theory		*/
/*	of Seismology, pp 154-155.										*/

/*	Expects arrays of 3 direction cosines for station and location.	*/
/*	Returns delta in degrees.										*/

double calc_delta(double sta_dircos[], double h_dircos[])
{
	double dotprod;
	double sq1,sq2,sq3;

	dotprod  = sta_dircos[1] * h_dircos[1];
	dotprod += sta_dircos[2] * h_dircos[2];
	dotprod += sta_dircos[3] * h_dircos[3];

	/* Corresponds to delta > 178 deg */
	if (dotprod < -0.9994){
		sq1 = sta_dircos[1] + h_dircos[1];
		sq2 = sta_dircos[2] + h_dircos[2];
		sq3 = sta_dircos[3] + h_dircos[3];

		dotprod = 0.5*(sq1*sq1 + sq2*sq2 + sq3*sq3) - 1;

		if (dotprod < -1) dotprod = -1;
	}
	/* Corresponds to delta < 2 deg */
	else if (dotprod > 0.9994){
		sq1 = sta_dircos[1] - h_dircos[1];
		sq2 = sta_dircos[2] - h_dircos[2];
		sq3 = sta_dircos[3] - h_dircos[3];

		dotprod = 1 - 0.5*(sq1*sq1 + sq2*sq2 + sq3*sq3);

		if (dotprod > 1) dotprod = 1;
	}

	return acos(dotprod)/TORAD;
}

/*#DOC  Title:																*/
/*#DOC    calc_esaz															*/
/*#DOC  Desc:																*/
/*#DOC    Calculates back azimuth between epicentre and station.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    sta_dircos[] - array of 6 direction cosines for station.			*/
/*#DOC    h_dircos[] - array of 6 direction cosines for hypocentre.			*/
/*#DOC    sta_lon - station longitude.										*/
/*#DOC    h_lon   - hypocentre longitude.									*/
/*#DOC    delta   - distance between the two in degrees.					*/
/*#DOC  Return:																*/
/*#DOC    azimuth as a double.												*/

double calc_esaz(double sta_dircos[], double h_dircos[], double sta_lon,
				 double h_lon, double delta)
{
	double da,dellon,azim;

	da  = (sta_dircos[1]-h_dircos[4]) * (sta_dircos[1]-h_dircos[4]);
	da += (sta_dircos[2]-h_dircos[5]) * (sta_dircos[2]-h_dircos[5]);
	da += (sta_dircos[3]-h_dircos[6]) * (sta_dircos[3]-h_dircos[6]);
	da -= 2;
	da /= 2*sin(TORAD*delta);

	if (da > 1) da = 1;
	if (da < -1) da = -1;

	azim =acos(da);

	dellon = sta_lon - h_lon;

	if ( dellon>180 || (dellon>-180 && dellon<0) )
		azim = 2*PI - azim;

	/* Rounding can put value just over 360. */
	azim /= TORAD;
	if (azim >= 360) azim -= 360;

	return azim;
}

/*#DOC  Title:																*/
/*#DOC    calc_dircos														*/
/*#DOC  Desc:																*/
/*#DOC    Calculates direction cosines given latitude and longitude.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    lat - latitude.													*/
/*#DOC    lon - longitude.													*/
/*#DOC    dircos[] - array for 6 direction cosines.							*/
/*#DOC  Return:																*/
/*#DOC    nothing															*/

void calc_dircos (double lat, double lon, double dircos[])
{
	double glat;						/* geocentric latitude */
	double coslat,sinlat,coslon,sinlon;

	lat = TORAD*lat;
	lon = TORAD*lon;

	glat = atan(ELLIPT * tan(lat));
	coslat = cos(glat);
	sinlat = sin(glat);
	coslon = cos(lon);
	sinlon = sin(lon);

	dircos[1] = coslat * coslon;
	dircos[2] = coslat * sinlon;
	dircos[3] = sinlat;
	dircos[4] = sinlat * coslon;
	dircos[5] = sinlat * sinlon;
	dircos[6] = -coslat;
} 

/*#DOC  Title:																*/
/*#DOC    dsort																*/
/*#DOC  Desc:																*/
/*#DOC    Sorts an array of doubles into descending order.					*/
/*#DOC  Input Arguments:													*/
/*#DOC    a[]   - array to be sorted.										*/
/*#DOC    left  - index of first value.										*/
/*#DOC    right - index of last value.										*/
/*#DOC  Return:																*/
/*#DOC    nothing															*/
/*#DOC  Calls:																*/
/*#DOC  	dswap() from this file and itself recursively.					*/

void dsort (double a[], int left, int right)
{
	int i,last;

	void dswap (double[], int, int);

	if(left>=right)
		return;

	dswap(a, left, (left+right)/2);
	last = left;
	for (i=left+1; i<=right; i++)
		if (a[i] < a[left])
			dswap(a, ++last, i);
	dswap(a, left, last);
	dsort(a,left,last);
	dsort(a,last+1,right);
}

/*#DOC  Title:																*/
/*#DOC    dswap ( called only from dsort(). )								*/
/*#DOC  Desc:																*/
/*#DOC    Part of the function dsort().										*/
/*#DOC  Input Arguments:													*/
/*#DOC    a[]  - array to be sorted.										*/
/*#DOC    i,j  - indexes on this array.										*/
/*#DOC  Return:																*/
/*#DOC    nothing															*/

void dswap(double a[], int i, int j)
{
	double temp;

	temp=a[i];
	a[i]=a[j];
	a[j]=temp;
}

/* Local functions called only by time routines below. */
int days_per_month (int mm, int yyyy);
int days_per_year (int yyyy);

/*#DOC  Title:																*/
/*#DOC    read_time															*/
/*#DOC  Desc:																*/
/*#DOC    Converts time from ISF format string to seconds since epoch.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    date/time as a string.											*/
/*#DOC  Return:																*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC    0 on error.														*/
/*#DOC  Calls:																*/
/*#DOC    join_time() from this file.										*/

double read_time(char *timestr)
{
	int yyyy,mm,dd,hh,mi,ss,msec;
	double sec;
	char cutstr[24];
	char *remnant;

	strcpy(cutstr,timestr);

	cutstr[4] = '\0';
	yyyy = strtol(&cutstr[0],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[7] = '\0';
	mm = strtol(&cutstr[5],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[10] = '\0';
	dd = strtol(&cutstr[8],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[13] = '\0';
	hh = strtol(&cutstr[11],&remnant,10);
	if (remnant[0]) return 0;

	cutstr[16] = '\0';
	mi = strtol(&cutstr[14],&remnant,10);
	if (remnant[0]) return 0;

	sec = strtod(&cutstr[17],&remnant);
	if (remnant[0]) return 0;

	ss = (int)sec;
	sec -= ss;
	msec = sec*1000+0.5;

	return join_time(yyyy,mm,dd,hh,mi,ss,msec);
}

/*#DOC  Title:																*/
/*#DOC    write_time														*/
/*#DOC  Desc:																*/
/*#DOC    Converts times from seconds since epoch to ISF date format.		*/
/*#DOC  Input Arguments:													*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC    out - pointer to memory put aside for 24 chars.					*/
/*#DOC  Return:																*/
/*#DOC   date/time as a string.												*/
/*#DOC  Calls:																*/
/*#DOC  	split_time() from this file.									*/

char* write_time(double time, char* out)
{
	int yyyy,mm,dd,hh,mi,ss,msec;

	/* Get constituent parts of date and time. */
	if (split_time(time,&yyyy,&mm,&dd,&hh,&mi,&ss,&msec))
		return "";

	/* Create and return the ISF string*/
	sprintf(out,"%02d-%02d-%d %02d:%02d:%02d.%03d",dd,mm,yyyy,hh,mi,ss,msec);

	return out;
}

/*#DOC  Title:																*/
/*#DOC    split_time														*/
/*#DOC  Desc:																*/
/*#DOC    Converts times from seconds since epoch to seperate ints.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC    yyyy,mm,dd,hh,mi,ss,msec - pointers for parts of date/time.		*/
/*#DOC  Return:																*/
/*#DOC    0/1 for success/failure.											*/
/*#DOC  Calls:																*/
/*#DOC  	days_per_month() and days_per_year() from this file.			*/

int split_time(double time, int *yyyy, int *mm, int *dd,int *hh, int *mi, 
															int *ss, int *msec)
{
	int year,month,hour,min;
	double day,sec;

	/* Switch debug messages from main on/off (1/0). */
	int diagnostic = 0;

	if (time == NULLVAL)
		return 1;

	if (diagnostic)
		fprintf(logfp,"split_time: time=%f ",time);

	/* Split into days before/after epoch and seconds since start of day. */

	/* Get date from day. */
	day = (int)(time/86400);
	year= EPOCH_YEAR;
	
	if (day >= 0){
		sec  = time - day*86400;
		day++;
		while (day > days_per_year(year)){
			day -= days_per_year(year);
			year++;
		}
	}
	else{
		sec  = time - day*86400;
		if (sec != 0){
			day--;
			sec+=86400;
		}
		while (day < 0 ){
			day += days_per_year(year);
			year--;
		}
	}

	for (month=1; month<13; month++){
		if (day <= days_per_month(month,year)){ break; }
		day -= days_per_month(month,year);
	}

	if (diagnostic)
		fprintf(logfp,"split_time: year=%d month=%d day=%f ",year,month,day);

	/* Get time from sec */
	min  = sec/60;
	sec -= min*60;
	hour = min/60;
	min -= hour*60;

	if (diagnostic)
		fprintf(logfp,"split_time: hour=%d min=%d sec=%f\n",hour,min,sec);

	*yyyy = year;
	*mm = month;
	*dd = day;
	*hh = hour;
	*mi = min;
	*ss = (int)sec;
	*msec = (sec-*ss)*1000;
	*msec = (int)(*msec+0.5);

	return 0;
}


/*#DOC  Title:																*/
/*#DOC    join_time															*/
/*#DOC  Desc:																*/
/*#DOC    Converts times from seperate ints to seconds since epoch.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    yyyy,mm,dd,hh,mi,ss,msec - parts of date/time.					*/
/*#DOC  Return:																*/
/*#DOC    time - time since epoch as a double.								*/
/*#DOC  Calls:																*/
/*#DOC  	days_per_month() and days_per_year() from this file.			*/

double join_time(int yyyy, int mm, int dd, int hh, int mi, int ss, int msec)
{
	int year,month;
	double day,time;

	/* Switch debug messages from main on/off (1/0). */
	int diagnostic = 0;

	if (yyyy == NULLVAL)
		return NULLVAL;

	if (diagnostic){
		fprintf(logfp,"join_time: %d %d %d ",yyyy,mm,dd);
		fprintf(logfp,"%d %d %d %d\n",hh,mi,ss,msec);
	}

	day = 0;
	if (yyyy > EPOCH_YEAR)
		for (year=EPOCH_YEAR; year<yyyy; year++)
			day += days_per_year(year);
	else
		for (year=yyyy; year<EPOCH_YEAR; year++)
			day -= days_per_year(year);

	for (month=1; month<mm; month++)
		day += days_per_month(month,yyyy);
	day += dd-1;

	time  = day*86400;
	time += hh*3600;
	time += mi*60;
	time += ss;
	if (msec != NULLVAL)
		time += msec*0.001;

	if (diagnostic)
		fprintf(logfp,"join_time: %f\n",time);

	return time;
}

/*#DOC  Title:																*/
/*#DOC    days_per_month													*/
/*#DOC  Desc:																*/
/*#DOC    Returns the number of days in a given month.						*/
/*#DOC  Input Arguments:													*/
/*#DOC    mm - month number (1-12).											*/
/*#DOC    yyyy - year.														*/
/*#DOC  Return:																*/
/*#DOC   number of days in the month.										*/

int days_per_month (int mm, int yyyy)
{
	int days_per_month[] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 };

	/* Case of typical year or not February */
	if (mm !=2 || yyyy%4 != 0){ return days_per_month[mm]; }

	/* Case of February in a typical leap year */
	if (yyyy%100 != 0){ return 29;}

	/* Case of February in last year of a typical century */
	if (yyyy%400 != 0){ return 28; }

	/* Case of February in last year of a special century */
	return 29;
}

/*#DOC  Title:																*/
/*#DOC    days_per_year ( called only from day_string(). )					*/
/*#DOC  Desc:																*/
/*#DOC    Returns the number of days in a given year.						*/
/*#DOC  Input Arguments:													*/
/*#DOC    yyyy - year.														*/
/*#DOC  Return:																*/
/*#DOC   number of days in the year.										*/

int days_per_year (int yyyy)
{
	/* Case of typical year */
	if (yyyy%4 != 0){ return 365; }

	/* Case of a typical leap year */
	if (yyyy%100 != 0){ return 366;}

	/* Case of last year of a typical century */
	if (yyyy%400 != 0){ return 365; }

	/* Case of February in last year of a special century */
	return 366;
}


/*#DOC  Title:																*/
/*#DOC    add_to_error														*/
/*#DOC  Desc:																*/
/*#DOC    Adds a string to the end of global string 'errstr'.			*/
/*#DOC  Input Arguments:													*/
/*#DOC    part_error - characters to be added.								*/
/*#DOC  Return:																*/
/*#DOC   nothing.															*/

void add_to_error(char *part_error){

	char tempstr[ERRLEN];

	strcpy(tempstr,errstr);

	if (strlen(tempstr) + strlen(part_error) > ERRLEN)
		sprintf(errstr,"%s .......",part_error);
	else
		sprintf(errstr,"%s %s",part_error,tempstr);

	return;
}

/*#DOC  Title:																*/
/*#DOC    handle_error														*/
/*#DOC  Desc:																*/
/*#DOC    Prints global string 'errstr' and resets it to null.				*/
/*#DOC  Input Arguments:													*/
/*#DOC    none.																*/
/*#DOC  Return:																*/
/*#DOC   nothing.															*/

/* Can return control to calling function without printing error by		*/
/* setting errstr to "".												*/

void handle_error(void){

	if (strcmp(errstr,"")){

		fprintf(errfp,"ISCLOC ERROR: %s\n",errstr);
		if (errfp != logfp)
			fprintf(logfp,"ISCLOC ERROR: %s\n",errstr);
	}
	return;
}
