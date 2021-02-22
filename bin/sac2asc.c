#include <stdio.h>
#include <sys/file.h>
#include "sachead.h"

/*
**
**	sac2asc
**
**	written 4/89 by Scott MacHaffie
**
**	this file reads from the standard input and writes to the standard out
**	it reads sac binary and writes sac ascii
**
**
**	sac2asc < f1 > f2
**
*/

/* used to identify program in stderr output */
static char prog[] = "sac2asc";

main(argc, argv)
int argc;
char **argv;
{
#if 0
	int inp;		/* input file handle */
	int interactive = 0;	/* interactive flag */
#endif
	long l;			/* return codes from convert functions */
	long convert_hdr();	/* convert SAC header */
	long convert_body();	/* convert SAC body */
#if 0
	char name[80];		/* name of file to convert */
	char outname[80];	/* name of output file */
	FILE *fp, *fopen();
#endif

#if 0
	/* if no arguments are given, then run in interactive mode */
	if (argc < 2)
		interactive = 1;

	while (--argc > 0 || interactive) {
		if (interactive) {
			printf("Enter file name to convert: ");
			scanf("%s", name);

			printf("Enter output file name: ");
			scanf("%s", outname);
		}
		else {
			strcpy(outname, "tmp.out");
			strcpy(name, argv[argc]);
		}

		/* input file is opened for fscanf access */
		if ((inp = open(name, O_RDONLY)) < 1) {
			fprintf(stderr, "%s: couldn't open input file\n", prog);
			exit(1);
		}

		/* output file is opened for write access (binary mode) */
		if ((fp = fopen(outname, "w")) == NULL) {
			close(inp);
			fprintf(stderr,"%s: couldn't open output file\n", prog);
			exit(2);
		}

		if ((l = convert_hdr(inp, fp)) < 0L) {
			fclose(fp);
			close(inp);
			fprintf(stderr, "%s: error converting header\n", prog);
			exit(3);
		}

		if ((l = convert_body(inp, fp, l)) < 0L) {
			fclose(fp);
			close(inp);
			fprintf(stderr, "%s: error converting body\n", prog);
			exit(4);
		}
		fclose(fp);
		close(inp);

		if (!interactive) {
			sprintf(name, "mv tmp.out %s", argv[argc]);
			system(name);
		}
		interactive = 0;
	}
#endif
	if ((l = convert_hdr(stdin, stdout)) < 0L) {
		fprintf(stderr, "%s: error converting body\n", prog);
		exit(4);
	}
	if ((l = convert_body(stdin, stdout, l)) < 0L) {
		fprintf(stderr, "%s: error converting body\n", prog);
		exit(5);
	}
	exit(0);
}

long
convert_hdr(inp, out)
/*
**
**	convert_hdr() -- read header from input file and write a binary
**	version to the output file.
**
*/
FILE *inp;	/* input file pointer */
FILE *out;	/* output file pointer */
{
	int i, j;
	long l1, l2, l3, l4, l5;
	unsigned long u1, u2, u3, u4, u5;
	float f1, f2, f3, f4, f5;
	struct SAChead2 header;

	if (fread((char *) &header, sizeof(header), 1, inp) != 1) {
	     fprintf(stderr,"%s: Couldn't read header from input\n", prog);
	     return(-1L);
	}

	/* write floating point data -- 5 numbers on a line */
	for (i = 0; i < NUM_FLOAT / 5; i++) {
		f1 = header.SACfloat[i * 5];
		f2 = header.SACfloat[i * 5 + 1];
		f3 = header.SACfloat[i * 5 + 2];
		f4 = header.SACfloat[i * 5 + 3];
		f5 = header.SACfloat[i * 5 + 4];
		fprintf(out, "%15.7f %15.7f %15.7f %15.7f %15.7f\n",f1,f2,f3,f4,f5);
	}

	for (i = 0; i < MAXINT / 5; i++) {
		l1 = header.SACint[i * 5];
		l2 = header.SACint[i * 5 + 1];
		l3 = header.SACint[i * 5 + 2];
		l4 = header.SACint[i * 5 + 3];
		l5 = header.SACint[i * 5 + 4];
		fprintf(out, "%10ld %10ld %10ld %10ld %10ld\n", l1, l2, l3, l4, l5);
	}

	u1 = header.SACun[0];
	u2 = header.SACun[1];
	u3 = header.SACun[2];
	u4 = header.SACun[3];
	u5 = header.SACun[4];
	fprintf(out, "%10lu %10lu %10lu %10lu %10lu\n", u1, u2, u3, u4, u5);

	/* this string is a different size than the others */
	for (i = 0; i < 8; i++)
		putc(header.SACstring[0][i], out);

	for (i = 1; i < MAXSTRING; i++) {
		for (j = 0; j < K_LEN; j++)
			putc(header.SACstring[i][j], out);
		if ((i + 1) % 3 == 0)
			putc('\n', out);
	}

	/* return the number of data points in the file */
	return(header.SACint[9]);
}

long
convert_body(inp, out, num)
/*
**
**	convert_body() -- convert the floating point data from ascii to binary
**
*/
FILE *inp;	/* input file pointer */
FILE *out;	/* output file pointer */
long num;	/* number of data points */
{
	long cnt;		/* number of data points read */
	float f;		/* scratch floating point variable */
	struct conv {		/* used to read a line of data at a time */
		float f1;
		float f2;
		float f3;
		float f4;
		float f5;
	} data;

	/* write lines of 5 data points */
	for (cnt = 0L; cnt < num - 5; cnt += 5) {
		if (fread((char *) &data, sizeof(data), 1, inp) != 1) {
			fprintf(stderr, "%s: Couldn't read data from", prog);
			fprintf(stderr, " input\n");
			return(-1L);
		}
		fprintf(out, "%15.1f%15.1f%15.1f%15.1f%15.1f\n",data.f1,
	     data.f2, data.f3, data.f4, data.f5);
	}

	/* write a line with < 5 data points */
	for ( ; cnt < num; cnt++) {
		if (fread((char *) &f, sizeof(f), 1, inp) != 1) {
			fprintf(stderr, "%s: Couldn't read data from", prog);
			fprintf(stderr, " input\n");
			return(-1L);
		}
		fprintf(out, "%15.1f", f);
	}
	fprintf(out, "\n");
	return(0L);
}
