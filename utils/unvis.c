/* File - src/lib/libc/gen/unvis.c 
 *   FreeBSD - August 18, 2008
 *   Revision 1.10 (Tuesday January 9, 2007)
 */

/*-
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)unvis.c	8.1 (Berkeley) 6/4/93";
#endif /* LIBC_SCCS and not lint */
/*
__FBSDID("$FreeBSD: src/lib/libc/gen/unvis.c,v 1.10 2007/01/09 00:27:56 imp Exp $");
*/
#include <sys/types.h>
#include <ctype.h>

/***** BEGIN VIS.H *****/

/*
 * modified from FreeBSD's include/vis.h (svn revision 121728)
 * by Richard Godbee <rwg@vt.edu>, 27 August 2008
 */

/*-
 * Copyright (c) 1990, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(#)vis.h       8.1 (Berkeley) 6/2/93
 * $FreeBSD$
 */

/*
 * to select alternate encoding format
 */
#define VIS_OCTAL       0x01    /* use octal \ddd format */
#define VIS_CSTYLE      0x02    /* use \[nrft0..] where appropriate */

/*
 * to alter set of characters encoded (default is to encode all
 * non-graphic except space, tab, and newline).
 */
#define VIS_SP          0x04    /* also encode space */
#define VIS_TAB         0x08    /* also encode tab */
#define VIS_NL          0x10    /* also encode newline */
#define VIS_WHITE       (VIS_SP | VIS_TAB | VIS_NL)
#define VIS_SAFE        0x20    /* only encode "unsafe" characters */

/*
 * other
 */
#define VIS_NOSLASH     0x40    /* inhibit printing '\' */
#define VIS_HTTPSTYLE   0x80    /* http-style escape % HEX HEX */
#define VIS_GLOB        0x100   /* encode glob(3) magics */

/*
 * unvis return codes
 */
#define UNVIS_VALID      1      /* character valid */
#define UNVIS_VALIDPUSH  2      /* character valid, push back passed char */
#define UNVIS_NOCHAR     3      /* valid sequence, no character produced */
#define UNVIS_SYNBAD    -1      /* unrecognized escape sequence */
#define UNVIS_ERROR     -2      /* decoder in unknown state (unrecoverable) */

/*
 * unvis flags
 */
#define UNVIS_END       1       /* no more characters */

int     strunvis(char *, const char *);
int     strunvisx(char *, const char *, int);
int     unvis(char *, int, int *, int);

/***** END VIS.H *****/

/*
 * decode driven by state machine
 */
#define	S_GROUND	0	/* haven't seen escape char */
#define	S_START		1	/* start decoding special sequence */
#define	S_META		2	/* metachar started (M) */
#define	S_META1		3	/* metachar more, regular char (-) */
#define	S_CTRL		4	/* control char started (^) */
#define	S_OCTAL2	5	/* octal digit 2 */
#define	S_OCTAL3	6	/* octal digit 3 */
#define	S_HEX2		7	/* hex digit 2 */

#define	S_HTTP		0x080	/* %HEXHEX escape */

#define	isoctal(c)	(((u_char)(c)) >= '0' && ((u_char)(c)) <= '7')
#define	ishex(c)	((((u_char)(c)) >= '0' && ((u_char)(c)) <= '9') || (((u_char)(c)) >= 'a' && ((u_char)(c)) <= 'f'))

/*
 * unvis - decode characters previously encoded by vis
 */
int
unvis(char *cp, int c, int *astate, int flag)
{

	if (flag & UNVIS_END) {
		if (*astate == S_OCTAL2 || *astate == S_OCTAL3) {
			*astate = S_GROUND;
			return (UNVIS_VALID);
		}
		return (*astate == S_GROUND ? UNVIS_NOCHAR : UNVIS_SYNBAD);
	}

	switch (*astate & ~S_HTTP) {

	case S_GROUND:
		*cp = 0;
		if (c == '\\') {
			*astate = S_START;
			return (0);
		}
		if (flag & VIS_HTTPSTYLE && c == '%') {
			*astate = S_START | S_HTTP;
			return (0);
		}
		*cp = c;
		return (UNVIS_VALID);

	case S_START:
		if (*astate & S_HTTP) {
		    if (ishex(tolower(c))) {
			*cp = isdigit(c) ? (c - '0') : (tolower(c) - 'a');
			*astate = S_HEX2;
			return (0);
		    }
		}
		switch(c) {
		case '\\':
			*cp = c;
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
			*cp = (c - '0');
			*astate = S_OCTAL2;
			return (0);
		case 'M':
			*cp = 0200;
			*astate = S_META;
			return (0);
		case '^':
			*astate = S_CTRL;
			return (0);
		case 'n':
			*cp = '\n';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 'r':
			*cp = '\r';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 'b':
			*cp = '\b';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 'a':
			*cp = '\007';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 'v':
			*cp = '\v';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 't':
			*cp = '\t';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 'f':
			*cp = '\f';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 's':
			*cp = ' ';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case 'E':
			*cp = '\033';
			*astate = S_GROUND;
			return (UNVIS_VALID);
		case '\n':
			/*
			 * hidden newline
			 */
			*astate = S_GROUND;
			return (UNVIS_NOCHAR);
		case '$':
			/*
			 * hidden marker
			 */
			*astate = S_GROUND;
			return (UNVIS_NOCHAR);
		}
		*astate = S_GROUND;
		return (UNVIS_SYNBAD);

	case S_META:
		if (c == '-')
			*astate = S_META1;
		else if (c == '^')
			*astate = S_CTRL;
		else {
			*astate = S_GROUND;
			return (UNVIS_SYNBAD);
		}
		return (0);

	case S_META1:
		*astate = S_GROUND;
		*cp |= c;
		return (UNVIS_VALID);

	case S_CTRL:
		if (c == '?')
			*cp |= 0177;
		else
			*cp |= c & 037;
		*astate = S_GROUND;
		return (UNVIS_VALID);

	case S_OCTAL2:	/* second possible octal digit */
		if (isoctal(c)) {
			/*
			 * yes - and maybe a third
			 */
			*cp = (*cp << 3) + (c - '0');
			*astate = S_OCTAL3;
			return (0);
		}
		/*
		 * no - done with current sequence, push back passed char
		 */
		*astate = S_GROUND;
		return (UNVIS_VALIDPUSH);

	case S_OCTAL3:	/* third possible octal digit */
		*astate = S_GROUND;
		if (isoctal(c)) {
			*cp = (*cp << 3) + (c - '0');
			return (UNVIS_VALID);
		}
		/*
		 * we were done, push back passed char
		 */
		return (UNVIS_VALIDPUSH);

	case S_HEX2:	/* second mandatory hex digit */
		if (ishex(tolower(c))) {
			*cp = (isdigit(c) ? (*cp << 4) + (c - '0') : (*cp << 4) + (tolower(c) - 'a' + 10));
		}
		*astate = S_GROUND;
		return (UNVIS_VALID);

	default:
		/*
		 * decoder in unknown state - (probably uninitialized)
		 */
		*astate = S_GROUND;
		return (UNVIS_SYNBAD);
	}
}

/*
 * strunvis - decode src into dst
 *
 *	Number of chars decoded into dst is returned, -1 on error.
 *	Dst is null terminated.
 */

int
strunvis(char *dst, const char *src)
{
	char c;
	char *start = dst;
	int state = 0;

	while ( (c = *src++) ) {
	again:
		switch (unvis(dst, c, &state, 0)) {
		case UNVIS_VALID:
			dst++;
			break;
		case UNVIS_VALIDPUSH:
			dst++;
			goto again;
		case 0:
		case UNVIS_NOCHAR:
			break;
		default:
			return (-1);
		}
	}
	if (unvis(dst, c, &state, UNVIS_END) == UNVIS_VALID)
		dst++;
	*dst = '\0';
	return (dst - start);
}

int
strunvisx(char *dst, const char *src, int flag)
{
	char c;
	char *start = dst;
	int state = 0;
    
	while ( (c = *src++) ) {
	again:
		switch (unvis(dst, c, &state, flag)) {
		case UNVIS_VALID:
			dst++;
			break;
		case UNVIS_VALIDPUSH:
			dst++;
			goto again;
		case 0:
		case UNVIS_NOCHAR:
			break;
		default:
			return (-1);
		}
	}
	if (unvis(dst, c, &state, UNVIS_END) == UNVIS_VALID)
		dst++;
	*dst = '\0';
	return (dst - start);
}


/* File - src/usr.bin/unvis/unvis.c
 *   FreeBSD - August 18, 2008
 *   Revision 1.9 (Wednesday September 4, 2002)
 */

/*-
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static const char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
#if 0
static char sccsid[] = "@(#)unvis.c	8.1 (Berkeley) 6/6/93";
#endif
static const char rcsid[] =
  "$FreeBSD: src/usr.bin/unvis/unvis.c,v 1.9 2002/09/04 23:29:08 dwmalone Exp $";
#endif /* not lint */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void process(FILE *, const char *);
static void usage(void);

int
main(int argc, char *argv[])
{
	FILE *fp;
	int ch;

	while ((ch = getopt(argc, argv, "?h")) != -1)
		switch((char)ch) {
		case '?':
		case 'h':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (*argv)
		while (*argv) {
			if ((fp=fopen(*argv, "r")) != NULL)
				process(fp, *argv);
			else
				fprintf(stderr, "Could not open file: %s\n", *argv);
			argv++;
		}
	else
		process(stdin, "<stdin>");
	exit(0);
}

static void
usage(void)
{
	fprintf(stderr, "usage: unvis [file ...]\n");
	exit(1);
}

void
process(FILE *fp, const char *filename)
{
	int offset = 0, c, ret;
	int state = 0;
	char outc;

	while ((c = getc(fp)) != EOF) {
		offset++;
	again:
		switch(ret = unvis(&outc, (char)c, &state, 0)) {
		case UNVIS_VALID:
			putchar(outc);
			break;
		case UNVIS_VALIDPUSH:
			putchar(outc);
			goto again;
		case UNVIS_SYNBAD:
			fprintf(stderr, "%s: offset: %d: can't decode\n", filename, offset);
			state = 0;
			break;
		case 0:
		case UNVIS_NOCHAR:
			break;
		default:
			fprintf(stderr, "bad return value (%d), can't happen\n", ret);
			exit(1);
		}
	}
	if (unvis(&outc, (char)0, &state, UNVIS_END) == UNVIS_VALID)
		putchar(outc);
}

