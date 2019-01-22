/*   Updates                      */
/*   March 95 by jh: font problems, background and forground      */
/*   Dec 96        : xcurs do not send back null char, remian in loop */
/*                   mouse sends a blank like on the pc, was l     */
/*   sep 98         ------------- verison 7.0 check, no change ------*/
/*   nov 5 98 jh    remove all '_' from routine names because of linux */
/*   mar 30 99 bmt: add more subroutine    */
/*   oct 25 01 lo  : set cursor to crosshair */
/*   1st March 2002 FMC : add crosshairs */
/*   7th March 2002 FMC : All drawing now done on pixmap which can be copied
                          into the window when required. This occurs when an
                          expose event occurs or when the crosshairs move.
                          Also window is now replotted when a resize event
                          occurs. */
/*   18th March 2002 FMC : Window only refreshed if it has really been resized.
                           When crosshairs are moved only part of the screen
                           is refreshed.
                           Added new subroutine xtextwin_ to draw text
                           directly to screen and not to pixmap. */
/*   10th April 2003 FMC: Added changes as suggested by F Tilmann:
                          - If 8bit PseudoColor not supported, check for TrueColour
			    and DirectColour before selecting black and white.
			  - Set font name instead of searching for default font.
			    Font name is passed as a parameter to xopen() from
			    the configuration file.
*/
/*   21/09/07 lot added check for screen height as otherwise
                  problem on wide screen display */

/*  22/2/2008 rrl added standard includes to avoid warnings with gcc4
                  added casts to XLookupString calls to avoid compile warnings */
/*  07/5/2008 lot problem on 64bit Linux version, changed int declaration */
/*  30/3/2009 lot avoid use of both screens for dual screen setup */
/*  06/4/2009 lot keep size in xopen if already set */
/*  21/4/2009 jh put in Wayne Crawford'ss suggestions for float to impove plotting */
/* 31/12/2010 jh dummies for dislin */
/* 22/02/2011 jh modify dummy xwindow_size*/
/* 16/04/2012 jh mouse right click will send ascii(6), like in windows */
/* 20/02/2013 lo add cursor */
/* 20/02/2014 pv added small changes due to g95 warnings */
/* 2014-04-29 pv added changes due to solaris cc warning */
/* 25/01/2018 lo add colors for spectrogram plotting and fillbox1 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

/*******************************************************************************
 *                                                                             *
 *                           Function Declarations                             *
 *                                                                             *
 ******************************************************************************/

int  fillbox_(float *x1,float *y1,float *x2,float *y2);
static void draw_xhair(XEvent *motion_event, int last_xhair);
/* void xtextwin_(char *text, long *n,long *ix, long *iy); LOT 11/4/08 */
void SetColours();
void SetForegroundColor(int red, int green, int blue);



/******************************************************************************/

char hello[] = {"Hello, World."};
char hi [] = {"Hi!"};
Display *mydisplay;
Window mywindow;
Visual myvisual;
Window rootw,pointw;
Cursor mycursor;
Colormap cmap;
int rootx,rooty,pointx,pointy;
unsigned int keybut;
int nDrawX,nDrawY;
XColor  col[7];             /* Array of colours */
XColor  dummyCol;          /* Just a dummy variable */
int nvis;                   /* Number of visuals in list */
XVisualInfo visTmp;         /* Temporary used for checking visual type */
XVisualInfo *visList;       /* list of visuals */
GC mygc;
XFontStruct *font_info;
XEvent myevent;
KeySym mykey;
XSizeHints myhint;
XWindowAttributes winAtt;
Atom wm_delete_window;
int myscreen;
float wscale,xscale,yscale;            /*scaling factor for window*/
unsigned long myforeground, mybackground;
int i;
int ih;                   /*xwindow y size in picels*/
char text[10];
int done;
static int first_xhair;  /* FMC 01-03-2002 global flag for xhair status */
static int last_x, last_y;
Pixmap mypixmap;         /* FMC 07-03-2002 all drawing is now done on pixmap */
typedef struct {
  int red;
  int green;
  int blue;
} rgbcol;
rgbcol rainbow[255];
rgbcol polar[255];
rgbcol seis[255];
XColor color[255];
int colorset;   // 1 if SetColour has been done
int colormode;
int colorchoice;  // 1 for rainbow, 2 for polar, 3 for seis

static const char *rgbhex[] = { "#FC00FF","#F700FF","#F200FF","#ED00FF","#E800FF","#E300FF","#DE00FF","#D900FF","#D400FF","#CF00FF","#CA00FF","#C500FF","#C000FF","#BB00FF","#B600FF","#B100FF","#AC00FF","#A700FF","#A200FF","#9D00FF","#9800FF","#9300FF","#8E00FF","#8900FF","#8400FF","#8000FF","#7B00FF","#7600FF","#7100FF","#6C00FF","#6700FF","#6200FF","#5D00FF","#5800FF","#5300FF","#4E00FF","#4900FF","#4400FF","#3F00FF","#3A00FF","#3500FF","#3000FF","#2B00FF","#2600FF","#2100FF","#1C00FF","#1700FF","#1200FF","#0D00FF","#0800FF","#0300FF","#0001FF","#0006FF","#000BFF","#0010FF","#0015FF","#001AFF","#001FFF","#0024FF","#0029FF","#002EFF","#0033FF","#0038FF","#003DFF","#0042FF","#0047FF","#004CFF","#0051FF","#0056FF","#005BFF","#0060FF","#0065FF","#006AFF","#006FFF","#0074FF","#0079FF","#007EFF","#0082FF","#0087FF","#008CFF","#0091FF","#0096FF","#009BFF","#00A0FF","#00A5FF","#00AAFF","#00AFFF","#00B4FF","#00B9FF","#00BEFF","#00C3FF","#00C8FF","#00CDFF","#00D2FF","#00D7FF","#00DCFF","#00E1FF","#00E6FF","#00EBFF","#00F0FF","#00F5FF","#00FAFF","#00FFFE","#00FFF9","#00FFF4","#00FFEF","#00FFEA","#00FFE5","#00FFE0","#00FFDB","#00FFD6","#00FFD1","#00FFCC","#00FFC7","#00FFC2","#00FFBD","#00FFB8","#00FFB3","#00FFAE","#00FFA9","#00FFA4","#00FF9F","#00FF9A","#00FF95","#00FF90","#00FF8B","#00FF86","#00FF81","#00FF7D","#00FF78","#00FF73","#00FF6E","#00FF69","#00FF64","#00FF5F","#00FF5A","#00FF55","#00FF50","#00FF4B","#00FF46","#00FF41","#00FF3C","#00FF37","#00FF32","#00FF2D","#00FF28","#00FF23","#00FF1E","#00FF19","#00FF14","#00FF0F","#00FF0A","#00FF05","#00FF00","#04FF00","#09FF00","#0EFF00","#13FF00","#18FF00","#1DFF00","#22FF00","#27FF00","#2CFF00","#31FF00","#36FF00","#3BFF00","#40FF00","#45FF00","#4AFF00","#4FFF00","#54FF00","#59FF00","#5EFF00","#63FF00","#68FF00","#6DFF00","#72FF00","#77FF00","#7CFF00","#80FF00","#85FF00","#8AFF00","#8FFF00","#94FF00","#99FF00","#9EFF00","#A3FF00","#A8FF00","#ADFF00","#B2FF00","#B7FF00","#BCFF00","#C1FF00","#C6FF00","#CBFF00","#D0FF00","#D5FF00","#DAFF00","#DFFF00","#E4FF00","#E9FF00","#EEFF00","#F3FF00","#F8FF00","#FDFF00","#FFFB00","#FFF600","#FFF100","#FFEC00","#FFE700","#FFE200","#FFDD00","#FFD800","#FFD300","#FFCE00","#FFC900","#FFC400","#FFBF00","#FFBA00","#FFB500","#FFB000","#FFAB00","#FFA600","#FFA100","#FF9C00","#FF9700","#FF9200","#FF8D00","#FF8800","#FF8300","#FF7F00","#FF7A00","#FF7500","#FF7000","#FF6B00","#FF6600","#FF6100","#FF5C00","#FF5700","#FF5200","#FF4D00","#FF4800","#FF4300","#FF3E00","#FF3900","#FF3400","#FF2F00","#FF2A00","#FF2500","#FF2000","#FF1B00","#FF1600","#FF1100","#FF0C00","#FF0700","#FF0200"};


/**/
/*   open a new x window    */
/**/
/*xopen_(size,type,background) */
void xopen_(size,type,cursortype, fontname)
int *size;       /* input:  window size in pixels */
int *type;       /* output: 1: color screen, 0: black white */
int *cursortype;   /* 0: arrow, 1:cross-hair pointer, 2:cross-hairs */
char *fontname;
{
  int argc;
  int  nxpix,nypix;    /* # pixels on screen */
  char **argv;
  int xsize,ysize;
  int last_xhair;         /* set to 1 if this is the last xhair
                              before window is redrawn, else 0 */
  char **fntList;            /* List of font names */
  int fix;                   /* index for accepted font */
  int fntCnt;                /* No. elements in fntList & fntInfolist */
  XFontStruct *fntInfolist;  /* List of font attributes */
  XFontStruct *defFnt;       /* Attributes for default font */
  Font f1id,f2id;            /* Font IDs */
  char *sp;                  /* pointer to first space in font name */

  *type=0;  /* default is bw */
  mydisplay = XOpenDisplay ("");
  myscreen  = DefaultScreen (mydisplay);
  mybackground = WhitePixel (mydisplay, myscreen);
  myforeground = BlackPixel (mydisplay, myscreen);
  cmap=XDefaultColormap(mydisplay,myscreen);
  colorset = 0;
  colormode = 1;   // 1 for XAllocColor and 2 for XAllocNamedColor
  colorchoice = 3;

/*  mybackground = BlackPixel (mydisplay, myscreen);
  myforeground = WhitePixel (mydisplay, myscreen);*/
/*  printf("mybackground %d\n",mybackground); */


/* --------- GET ROOT WINDOW SIZE ------vvvv--------- */

  nxpix = DisplayWidth(mydisplay,myscreen);
  nypix = DisplayHeight(mydisplay,myscreen);

/* reduce x pixels in case large number indicates dual screen */
  if (nxpix>2000)
  {
    nxpix=nxpix/2;    
  }

/* --------- GET ROOT WINDOW SIZE ------^^^^--------- */

/* scale window so it still fits with tektronics, calculate scaling factor*/
/* to be used in all subsequent operations*/

  if (*size==0) /* check if size is already set */
  { 
    *size = 90;
  } 
/*  printf("size %d\n",*size);  */
  xsize=((*size)*nxpix)/100 ;
  wscale=xsize/1024.0; 
  myhint.x      = 1;
  myhint.y      = 1;
  myhint.width  = 1024*wscale;
  xscale=wscale;                 /*initially same scale for x and y */

  ysize=((*size)*nypix)/100 ;
  wscale=ysize/780.0; 
  myhint.height = 780*wscale; 
  yscale=wscale;
  ih=myhint.height;

  nDrawY = ih;
  nDrawX = myhint.width;
  myhint.flags  = PPosition | PSize;


/* Try to get RGB colours from default colour map if color screen: */
/* Checking display type (if 8bit PseudoColor not supported, check TrueColour
   and DirectColour, and if they are no good, use black&white) */
  visTmp.depth = 8;
  visTmp.screen = myscreen;
  visTmp.class = PseudoColor;
  visList = XGetVisualInfo (mydisplay, VisualScreenMask | VisualDepthMask,
                            &visTmp, &nvis);
/*     printf("First nvis %d\n",nvis); */
  if (nvis>0)
  {
    *type=1;    /* indicate a color screen */
  }
  else
  {
    /* Try direct colour */
/*    visTmp.class = DirectColor ;*/
    visTmp.class = DirectColor ;
    visList = XGetVisualInfo (mydisplay, VisualScreenMask,
	                      &visTmp, &nvis);
/*       printf("Second nvis %d\n",nvis); */
    if (nvis>0)
    {
      *type=1;    /* indicate a color screen */
    }
    else
    {
      /* Try true colour */
/*      visTmp.class = DirectColor ;
      visTmp.class = DirectColor ;*/
      visTmp.class = TrueColor ;
      visList = XGetVisualInfo (mydisplay, VisualScreenMask,
				&visTmp, &nvis);
/* 	  printf("Third nvis %d\n",nvis); */
      if (nvis>0)
      {
	*type=1;    /* indicate a color screen */
      } 
    }
  }

  if (*type==1)
  {
    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "red",
                          &(col[3]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get red colour\n");
      exit(-1);
    }

    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "green",
                          &(col[2]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get green colour\n");
      exit(-1);
    }

    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "#0000FF",
                          &(col[1]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get blue colour\n");
      exit(-1);
    }

    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "yellow",
                          &(col[4]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get red colour\n");
      exit(-1);
    }
  }

  if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "white",
                        &(col[5]),&dummyCol))
  {
    printf ("--colour-- ERROR: Unable to get white colour\n");
    exit(-1);
  }

  if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "black",
                        &(col[6]),&dummyCol))
  {
    printf ("--colour-- ERROR: Unable to get black colour\n");
    exit(-1);
  }


  first_xhair = 1;          /* FMC 18-03-2002 initialise crosshair status */
  last_x = last_y = -1;     /* initialise crosshair position */

  /* set up window */
  mywindow = XCreateSimpleWindow (mydisplay, DefaultRootWindow (mydisplay),
                                  myhint.x, myhint.y,
                                  myhint.width, myhint.height, 5,
                                  myforeground, mybackground);

  wm_delete_window = XInternAtom(mydisplay, "WM_DELETE_WINDOW", False); 
  XSetWMProtocols(mydisplay, mywindow, &wm_delete_window, 1);  

  mygc = XCreateGC (mydisplay, mywindow, 0, 0);

  /* FMC 07-03-2002 create a pixmap the same size as the screen as this
     is the maximum size that the window can take. */
  mypixmap = XCreatePixmap(mydisplay, mywindow, nxpix, nypix,
                           DefaultDepthOfScreen(XDefaultScreenOfDisplay(mydisplay)));

  /* initialise pixmap to background colour */
/*  XSetForeground(mydisplay, mygc, WhitePixel(mydisplay, myscreen));*/
  XSetForeground(mydisplay, mygc, mybackground);
  XFillRectangle(mydisplay, mypixmap, mygc, 0, 0, nxpix, nypix);
/*  XSetForeground(mydisplay, mygc, BlackPixel(mydisplay, myscreen));*/
  XSetForeground(mydisplay, mygc, myforeground);

  /* FMC 18-03-2002 get real size of window */
  XGetWindowAttributes(mydisplay,mywindow,&winAtt);

/**/

/* --------- FONTS ---------------------vvvv--------- */

  /* FMC 11-04-2003 Allow a font name to be passed from the
     configuration file. If no font is found, search for default. */
  if (fontname)
  {
    /* fontname should not contain any spaces so remove trailing
       blanks by searching for first space */
    sp = strchr (fontname, ' ');
    *sp = '\0';

     /* search fonts for the font name which has been passed in */
     fntList = XListFontsWithInfo (mydisplay, fontname,
                                   1,&fntCnt,&fntInfolist);
  }
  
  /* either font search was unsuccessful or no font name given */
  if (!fntList || !fontname)
  {
    /* generate list of fonts */
    fntList = XListFontsWithInfo (mydisplay,"-*-*-*-r-*-*-*-*-*-*-m-*-iso8859-*",
                                  1,&fntCnt,&fntInfolist);
  }

  if (fntList)
  {
    /* check that a font has been found */
    f1id = XLoadFont (mydisplay,*(fntList));
    /* destroy list of fonts */
    XFreeFontInfo (fntList,fntInfolist,fntCnt);
  }
  else
  {
    printf ("--fonts-- ERROR: Unable to get font\n");
    exit(-1);
  }

  XSetFont (mydisplay,mygc,f1id);

/*  fntList = XListFontsWithInfo (mydisplay,"-*-*-*-r-*-*-20-*-*-*-m-*-iso8859-*"
               ,1,&fntCnt,&fntInfolist);
  if (fntList) {
    f2id = XLoadFont (mydisplay,*(fntList));
    XFreeFontInfo (fntList,fntInfolist,fntCnt);
  }
  else {
   printf ("--fonts-- ERROR: Unable to get large font\n");
   exit(-1);

  } */
/* --------- FONTS ---------------------^^^^--------- */


  XSetBackground (mydisplay, mygc, mybackground);
  XSetForeground (mydisplay, mygc, myforeground);

  XSelectInput (mydisplay, mywindow,
                ButtonPressMask |     /* mouse click event */
                KeyPressMask |        /* keyboard event */
                ExposureMask |        /* exposure event */
                StructureNotifyMask); /* resize event */

  XMapRaised (mydisplay, mywindow);

/* setup cursor, lo 31 oct 01 */
  if(*cursortype>0)
  {
    /* cursortype=1, cursortype=2 */
    mycursor= XCreateFontCursor(mydisplay, XC_crosshair);
    XDefineCursor(mydisplay,mywindow,mycursor);

    if(*cursortype==2)
    {
      /* FMC 01-03-2002 if crosshairs are required, set mask
         to recognise mouse motion */
      XSelectInput (mydisplay, mywindow,
                    ButtonPressMask |     /* mouse click event */
                    KeyPressMask |        /* keyboard event */
                    ExposureMask |        /* exposure event */
                    StructureNotifyMask | /* resize event */
                    PointerMotionMask |   /* mouse motion event */
                    LeaveWindowMask);     /* leave window event */
    }
  }
}


void xclose_()
{
  XFreeGC (mydisplay, mygc);
  XDestroyWindow (mydisplay, mywindow);
  XFreePixmap(mydisplay,mypixmap);       /* FMC 07-03-2002 */
  XCloseDisplay (mydisplay);
  /*exit(0);*/
}


/* FMC 07-03-2002 remove this subroutine as it doesn't seem to be called */
/* xmore_() */
/* { */
/*   done = 0; */
/*   while (done==0)  */
/*   { */
/*     XNextEvent (mydisplay, &myevent); */
/*     switch (myevent.type)  */
/*     { */
/*     case Expose: */
/*       if (myevent.xexpose.count ==0) */
/*         XDrawImageString (myevent.xexpose.display, myevent.xexpose.window, */
/*                           mygc, 50, 50, hello, strlen(hello) ); */
/*       break; */
/*     case MappingNotify: */
/*       XRefreshKeyboardMapping (&myevent); */
/*       break; */
/*     case ButtonPress: */
/*       XDrawImageString (myevent.xbutton.display, myevent.xbutton.window, */
/*                         mygc, myevent.xbutton.x, myevent.xbutton.y+50, hi, */
/*                         strlen(hi) ); */
/*       XDrawLine (myevent.xexpose.display, myevent.xexpose.window, mygc, */
/*                  myevent.xbutton.x, myevent.xbutton.y, */
/*                  myevent.xbutton.x, myevent.xbutton.y+20); */
/*       break; */
/*     case KeyPress: */
/*       i = XLookupString (&myevent, text, 10, &mykey, 0); */
/*       if (i==1 && text[0]=='q') done = 1; */
/*       break; */
/*     }  */ /* switch */
/*   }  */ /* while */

/* } */


void xline_()
{
/*   XDrawLine (mydisplay, mywindow, mygc, 20,20,50,50);  FMC 03-07-2002 */
  XDrawLine (mydisplay, mypixmap, mygc, 20,20,50,50);
}


/***********************************************************************/
/* move cursor to ix,iy, units are tectronics*/

void xmoveto_(ix,iy)
/* long *ix;
long *iy; LOT 11/4/08 */
float *ix,*iy;

{
  short int x,y;

  x=(*ix)*xscale;
  y=ih-*iy*yscale;

  /* XDrawLine (mydisplay, mywindow, mygc,x,y,x,y); FMC 07-03-2002
     XClearArea (mydisplay, mywindow,x,y,1,1,False); */
  XDrawLine (mydisplay, mypixmap, mygc,x,y,x,y);
}


/************************************************************************/
/* draw a line between ix1,iy1 and ix2, iy2, units are tectronics*/
void xlineto_(ix1,iy1,ix2,iy2)
/*long *ix1,*ix2;
long *iy1,*iy2; LOT 11/4/08 */
float *ix1,*ix2,*iy1,*iy2;
{
  short int x1,y1,x2,y2;
  char str[20];

  x1=*ix1*xscale;
  x2=*ix2*xscale;
  y1=ih-*iy1*yscale;
  y2=ih-*iy2*yscale;

  /* XDrawLine (mydisplay, mywindow, mygc,x1,y1,x2,y2); FMC 07-03-2002 */
  XDrawLine (mydisplay, mypixmap, mygc,x1,y1,x2,y2);
}

/***********************************************************************/
/* plot a text string text with n characters starting at position ix,iy  */
void xtext_(text,n,ix,iy)
/* long *n,*ix,*iy; LOT 11/4/08 */
int *n,*ix,*iy;
char *text;
{
/*  short int x,y; */
  int x,y;

  x=*ix*xscale;
  y=ih-*iy*yscale;

/*   XDrawString (mydisplay, mywindow, mygc,x,y,text,*n); FMC 07-03-2002 */
  XDrawString (mydisplay, mypixmap,mygc,x,y,text,*n);
}

/***********************************************************************/
/* FMC 18-03-2002 new subroutine which draws text directly on screen   */
/* instead of drawing to pixmap. This is to be used to print a message */
/* when data is loading and screen would otherwise be blank. Once data */
/* has been plotted this is copied onto the screen and text from this  */
/* subroutine is overwritten.                                          */

void xtextwin_(text,n,ix,iy)
/* long *n,*ix,*iy; LOT 11/4/08 */
int *n,*ix,*iy;
char *text;
{
/*  short int x,y; LOT 11/4/08 */
  int x,y;

/*  XSetForeground(mydisplay, mygc, BlackPixel(mydisplay, myscreen));*/
  XSetBackground (mydisplay, mygc, mybackground);
  XSetForeground (mydisplay, mygc, myforeground);

  x=*ix*xscale;
  y=ih-*iy*yscale;

  XDrawString (mydisplay, mywindow,mygc,x,y,text,*n);
}


/**********************************************************************/
/* graphic cursor  */
void xcursr_(cha,ix,iy)
/* long *ix,*iy; LOT 11/4/08 */
int *ix,*iy;
char *cha;
{
   int i;
   int last_xhair;
   int xbut;
   char in_msg[512];

   XWindowAttributes attrib;

/* FMC 07-03-2002 this routine is called when window is waiting
   for input, update contents of window to include any drawing
   etc. that has taken place since the last window update */

   XCopyArea(mydisplay, mypixmap, mywindow, mygc, 0, 0, winAtt.width, winAtt.height, 0, 0);

   /* Read keyboard/mouse and network in a loop until we get some input and return. */
   while (true) {
      if (XPending(mydisplay)) {
         /* An X11 event is pending. */
         XNextEvent(mydisplay, &myevent);
         *cha=0;
         switch (myevent.type) {
         case ClientMessage:
            if ((Atom)myevent.xclient.data.l[0] == wm_delete_window) {
               *cha=113;
               return;
            }
            break;
         case Expose:
            /* FMC 07-03-2002 part of the window has been exposed, refresh by copying pixmap contents into window */
            XCopyArea(mydisplay, mypixmap, mywindow, mygc, 0, 0, winAtt.width, winAtt.height, 0, 0);
            break;

         case ConfigureNotify:
            /* FMC 07-03-2002 resize event. Get new size of window and calculate scaling factors. Redraw window contents */
            /* FMC 18-03-2002 check old size of window. If window has not really been resized, ignore this event */
            XGetWindowAttributes(mydisplay,mywindow,&attrib);
            if ((attrib.height == winAtt.height)&&(attrib.width == winAtt.width)) break;
            XGetWindowAttributes(mydisplay,mywindow,&winAtt);
            ih=winAtt.height;
            xscale=winAtt.width/1024.0;
            yscale=winAtt.height/780.0;
            strcpy(cha,"r");       /* send back flag to replot */
            *ix=myevent.xbutton.x/xscale;
            *iy=(ih-myevent.xbutton.y)/yscale;
            return;

         case ButtonPress:
            last_xhair = 1;
            draw_xhair(&myevent, last_xhair);
            strcpy(cha," ");       /* mouse press send a blank */
            xbut=myevent.xbutton.button;
            if(xbut==3) *cha=6;  /* right */ 
            if(xbut==1) *cha=32; /* left  */  
            *ix=myevent.xbutton.x/xscale;
            *iy=(ih-myevent.xbutton.y)/yscale;
            return;

         case KeyPress:
            last_xhair = 1;
            draw_xhair(&myevent, last_xhair);
            xbut=myevent.xbutton.button;
            i = XLookupString ((XKeyEvent*)&myevent, cha, 10, &mykey, 0);   /* RL 2/2008 */
            if (!strncmp(cha,"\0",1)) {
               switch(xbut) {
               case 111:
                  *cha=2;   /* up cursor*/
                  break;
               case 116:
                  *cha=4;   /* down cursor*/
                  break;
               case 113:
                  *cha=1;   /* left cursor*/
                  break;
               case 114:
                  *cha=3;   /* right cursor*/
                  break;
               case 98:
                  *cha=2;    /* up cursor*/
                  break;
               case 104:
                  *cha=4;   /* down cursor*/
                  break;
               case 100:
                  *cha=1;   /* left cursor*/
                  break;
               case 102:
                  *cha=3;   /* right cursor*/
                  break;
               }  
            }
            if(strncmp(cha,"\0",1)==0) break; 
            *ix=myevent.xbutton.x/xscale;
            *iy=(ih-myevent.xbutton.y)/yscale;
            return;

         case MotionNotify:
            last_xhair = 0;
            draw_xhair(&myevent, last_xhair);
            break;

         case LeaveNotify:
            /* remove last xhair */
            last_xhair = 1;
            draw_xhair(&myevent, last_xhair);
            break;
         }
      } else {
         /* No X11 events are pending. */
         /* Check for network events.  */
         if (get_udp_msg(in_msg)) {
            if (!strcmp(in_msg, "QUIT")) {
               *cha=113;
               return; 
            } 
         }
         /* Sleep 50ms to avoid 100% cpu use */
         usleep(50);
      }
   }
}




/*  clear screen    */
void xclear_()
{
  /* FMC 18-03-2002 clear pixmap by drawing rectangle of background colour and copy onto screen */
  XSetForeground(mydisplay, mygc, mybackground);
  XFillRectangle(mydisplay, mypixmap, mygc, 0, 0,
                 DisplayWidth(mydisplay,myscreen),
                 DisplayHeight(mydisplay,myscreen));
  XSetForeground(mydisplay, mygc, myforeground);
  XCopyArea(mydisplay, mypixmap, mywindow, mygc, 0, 0,
            winAtt.width, winAtt.height, 0, 0);
}




void updatewindowsize_()
{
  XGetWindowAttributes(mydisplay,mywindow,&winAtt);

  ih=winAtt.height;
  xscale=winAtt.width/1024.0;
  yscale=winAtt.height/780.0;
}




void  promptx (mDSP,mGC,mWIN,fg,bg,xmin,ymin,xlen,ylen,prompt,ans,nans)
/* ----------------------------------------------------------------------- */
/* A quick hack to read a string from the keyboard in an existing X11      */
/* application                                                             */
/*                                                                         */
/* Compile with '-DFTNSTRING', or uncomment the '#define   FTNSTRING   1'  */
/* line below to strip trailing blanks in FORTRAN strings.                 */
/*                                                                         */
/*                                                                         */
/*  NOTE: - The routine wipes out all graphics in the answer area !        */
/*        - *NO* checks are performed on the input parameters !            */
/*                                                                         */
/*                                                                         */
/* 940905as: Initial version.                                              */
/* 940906as: Now possible to strip trailing blanks (FORTRAN strings).      */
/*                                                                         */


#define   FTNSTRING   1  /* Uncomment to strip trailing blanks in *prompt */

/* Input parameters: */
  Display  *mDSP;       /* Display           */
  Window   mWIN;        /* Main window       */
  GC       mGC;         /* Graphics context  */
  unsigned long fg;     /* foreground colour */
  unsigned long bg;     /* background colour */
  int      xmin;        /* X coordinate for upper left corer of answer area */
  int      ymin;        /* Y coordinate for upper left corer of answer area */
  int      xlen;        /* Width of answer area */
  int      ylen;        /* Height of answer area */
  char     *prompt;     /* Prompt string     */
  int      nans;        /* Max number of characters in answer string */


/* Output parameters: */
  char  *ans;           /* answer string     */


{


/* Variables: */
  Window   pWIN;       /* Main window       */
  XEvent pEV;          /* X event */
  KeySym pKEY;         /* Keysym */
  int pSCR;            /* Screen */
  char text[10];
  char scrSTR[250];    
  char tmpSTR[250];    
  int done;
  int i,tmp;

#ifdef FTNSTRING
  for (i=strlen(prompt)-1;i>=0;i--)    
    if(!isspace(prompt[i]))
      break;
  tmpSTR[i+1]='\0';
  for ( ; i>=0; i--)
    tmpSTR[i]=prompt[i];
#define PROMPTSTRING tmpSTR
#else
#define PROMPTSTRING prompt
#endif

  pWIN = XCreateSimpleWindow(mDSP,mWIN,xmin,ymin,xlen,ylen,2,fg,bg);
  XSelectInput (mDSP,pWIN,  KeyPressMask | ExposureMask);
  XMapRaised (mDSP,pWIN);
  XFlush (mDSP);

  ans[0] = 0;
  sprintf (scrSTR,"%s %s",PROMPTSTRING,ans);
  
  done = 0;
  while (done==0) 
  {
    XNextEvent (mDSP, &pEV);
    switch (pEV.type) 
    {
    case Expose:
      if (pEV.xexpose.count ==0) 
      {
        XSetForeground (mDSP, mGC, fg);
        XSetBackground (mDSP, mGC, bg);
        XDrawImageString (pEV.xexpose.display, pEV.xexpose.window,
                          mGC,10,20,scrSTR,strlen(scrSTR));
        XFlush (mDSP);
      }
      break;
    case KeyPress:
      i = XLookupString ((XKeyEvent*)&pEV, text, 10, &pKEY, 0);          /* RL 2/2008 */
      if (i=1) 
      {
        if (text[0]==13)    /* RETURN */
          done = 1;
        else
        {
          if ((text[0]==8 || text[0]==127) && strlen(ans)>0)
          {  /* DEL/BS */
            ans[strlen(ans)-1] = 0;
            XClearWindow (mDSP,pWIN);
          }
          else if (text[0]>31)
          {  /* only accepts printable characters */ 
            tmp = strlen(ans);
            if (tmp>=nans)
            {      /* not room for more characters */
              XFillRectangle (mDSP,pWIN,mGC,0,0,xlen,ylen);
              XSetForeground (mDSP, mGC, bg);
              XSetBackground (mDSP, mGC, fg);
              XDrawImageString (mDSP, pWIN,mGC,10,20
                                ,"Not room for more characters",28);
              XFlush (mDSP);
              sleep(2);
              XSetForeground (mDSP, mGC, fg);
              XSetBackground (mDSP, mGC, bg);
              XClearWindow (mDSP,pWIN);
              XDrawImageString (mDSP,pWIN,mGC,10,20,scrSTR,strlen(scrSTR));
            }
            else 
            {
              ans[tmp] = text[0];
              ans[tmp+1] = 0;
            }
          }
          sprintf (scrSTR,"%s %s",PROMPTSTRING,ans);
          XDrawImageString (mDSP, pWIN,mGC,10,20,scrSTR,strlen(scrSTR));
          XFlush (mDSP);
          break;
        }  
      }  
    } 
  }  

  XUnmapWindow (mDSP,pWIN);
  for(done=strlen(ans);done<nans;done++)
    ans[done]=' ';
}


void oneline1_(question,nq,answer,na,x0,y0)
/* question: prompt question                               */
/* nq:       number of characters in question              */
/* naswer:   answer                                        */
/* na        number of characters in anaswe                */
/* x0,y0     posion of string                              */
   float *x0;
   float *y0;
   int *na;
   int *nq;
   char *answer;
   char *question;
{
  int xlen;
  int  ylen;
  int ix;
  int iy;

  ix=*x0;
  iy=*y0;
  xlen=(*nq+*na)*8;
  ylen=30;
  ix=ix*xscale;
  iy=ih-iy*yscale;
  printf("%d %d %d %d\n",ix,iy,xlen,ylen);
  promptx (mydisplay,mygc,mywindow,myforeground,mybackground,ix,iy,xlen,ylen,question,answer,*na);
}


void xgetabsposition_()
{
  /* Get the current abs cursor position: */
  if (!XQueryPointer(mydisplay, DefaultRootWindow (mydisplay), &rootw, &pointw,
                     &rootx,&rooty,&pointx,&pointy,&keybut) )
    exit (-1);
}


void xmovetoalpha_()
{
 /* Move the cursor back to where is was found: */
  XWarpPointer(mydisplay, None, DefaultRootWindow(mydisplay), None, None, 0, 0,
               rootx, rooty);
  XFlush(mydisplay);
}


void xmovetographics_()
{
  XWarpPointer(mydisplay, None , mywindow, 0, 0, 0, 0, 200, 100);
  XFlush (mydisplay);
}


/*    set color      */
void setcolorx_(coll)
int *coll;
{
  XSetForeground (mydisplay, mygc, col[*coll].pixel);
}


/*    set background color      */
/*    fill rectangel  with current color, x1,y1,x2,y2 are corners      */
void setbackx_(coll)
int *coll;
{
/*   xclear_; FMC 07-03-2002*/
/*   XSetWindowBackground (mydisplay, mywindow, col[*coll].pixel); */
  XSetBackground (mydisplay, mygc, col[*coll].pixel);
  xclear_;
}


void clearwindow_(x1,y1,x2,y2)
long  *x1,*y1,*x2,*y2;
{
  int c;
  float fx1,fx2,fy1,fy2;

  c = 5;
  setcolorx_(&c);
/*  setcolorx_(&mybackground);*/
  fx1 = *x1;
  fx2 = *x2;
  fy1 = *y1;
  fy2 = *y2; 
  fillbox_(&fx1,&fy1,&fx2,&fy2); 
  c = 6;
  setcolorx_(&c);
/*  setcolorx_(&myforeground);*/
}

int  fillbox1_(x1,y1,x2,y2,colind)
float *x1,*y1,*x2,*y2;
int *colind;
{
  int xlen,ylen,x0,y0;

  SetColours();
  x0=(*x1)*xscale;
  y0=ih-(*y2)*yscale;
  xlen=((*x2)-(*x1))*xscale;
  ylen=((*y2)-(*y1))*yscale;

  XSetForeground (mydisplay, mygc, color[*colind-1].pixel);
  XDrawRectangle(mydisplay,mypixmap,mygc,x0,y0,xlen,ylen);
  XFillRectangle (mydisplay,mypixmap,mygc,x0,y0,xlen,ylen);
//  XSetForeground(mydisplay, mygc, myforeground); // back to default, needed?
}

int  fillbox_(x1,y1,x2,y2)
float *x1,*y1,*x2,*y2;
{
  int xlen,ylen,x0,y0;

  x0=(*x1)*xscale;
  y0=ih-(*y2)*yscale;
  xlen=((*x2)-(*x1))*xscale;
  ylen=((*y2)-(*y1))*yscale;

  /* XFillRectangle(mydisplay,mywindow,mygc,x0,y0,xlen,ylen); FMC 07-03-2002 */
  XFillRectangle (mydisplay,mypixmap,mygc,x0,y0,xlen,ylen);
}


void getnxypix_(x,y)
int *x,*y;
{
  *x = nDrawX;
  *y = nDrawY;
}


void fillcircle_(x1,y1,r)
long  *x1,*y1,*r;
{
  int xlen,ylen,x0,y0;

  x0=((*x1)-(*r))*xscale;
  y0=ih-((*y1)+(*r))*yscale;
  xlen=(*r)*xscale;
  ylen=(*r)*yscale;

  /* XFillArc(mydisplay,mywindow,mygc,x0,y0,2*xlen,2*ylen,0,360*64); FMC 07-03-2002 */
  XFillArc(mydisplay,mypixmap,mygc,x0,y0,2*xlen,2*ylen,0,360*64);
}


void circle_(x1,y1,r)
long  *x1,*y1,*r;
{
 int xlen,ylen,x0,y0;

  x0=((*x1)-(*r))*xscale;
  y0=ih-((*y1)+(*r))*yscale;
  xlen=(*r)*xscale;
  ylen=(*r)*yscale;

  /* XDrawArc(mydisplay,mywindow,mygc,x0,y0,2*xlen,2*ylen,0,360*64); FMC 07-03-2002 */
  XDrawArc(mydisplay,mypixmap,mygc,x0,y0,2*xlen,2*ylen,0,360*64);
}


void drawbox_(ix1,iy1,ix2,iy2)
long *ix1,*iy1;
long *ix2,*iy2;
{
  short int x1,y1,x2,y2;

  x1=*ix1*xscale;
  x2=*ix2*xscale;
  y1=ih-*iy1*yscale;
  y2=ih-*iy2*yscale;

/*  XDrawLine (mydisplay, mywindow, mygc,x1,y1,x2,y1);
    XDrawLine (mydisplay, mywindow, mygc,x2,y1,x2,y2);
    XDrawLine (mydisplay, mywindow, mygc,x1,y1,x1,y2);
    XDrawLine (mydisplay, mywindow, mygc,x1,y2,x2,y2); FMC 07-03-2002 */
  XDrawLine (mydisplay, mypixmap, mygc,x1,y1,x2,y1);
  XDrawLine (mydisplay, mypixmap, mygc,x2,y1,x2,y2);
  XDrawLine (mydisplay, mypixmap, mygc,x1,y1,x1,y2);
  XDrawLine (mydisplay, mypixmap, mygc,x1,y2,x2,y2);
}


/******************************************************************************
 * draw_xhair FMC 1st March 2002
 *            FMC 7th March 2002 changed to copy window onto pixmap,
 *                               draw crosshairs on window and remove old
 *                               crosshairs by copying pixmap onto window
 *
 * Description: called when mouse motion is detected in the window. The first
 *              time this routine is called the current window is copied onto
 *              the pixmap. Crosshairs are drawn on the window and not on the
 *              pixmap. Each subsequent time this routine is called the pixmap
 *              is copied back to the screen which removes the previous
 *              crosshairs.
 *
 * Input parameters: motion_event - the event which has triggered the callback
 *                   last_xhair - if this is the last xhair before the screen
 *                                is redrawn: 1 = yes, 0 = no
 *
 * Output parameters: the global variable first_xhair is set to 0 after first
 *                    call to this routine
 *
 * Returns: void
 *
 * Comments: if last_xhair is 1, the window is restored but a new set of
 *           crosshairs are not drawn.
 *
******************************************************************************/


void draw_xhair(XEvent *motion_event, int last_xhair)
{
  int x, y;

  /* get current mouse position */
  x = motion_event->xmotion.x;
  y = motion_event->xmotion.y;

  /* ensure crosshairs will be drawn in black */
/*  XSetForeground(mydisplay, mygc, BlackPixel(mydisplay, myscreen));*/
  XSetForeground(mydisplay, mygc, myforeground);

  if(!first_xhair)
  {
    /* restore positions where previous crosshairs were */
    XCopyArea(mydisplay, mypixmap, mywindow, mygc,
              last_x, 0, 1, winAtt.height, last_x, 0);
    XCopyArea(mydisplay, mypixmap, mywindow, mygc,
              0, last_y, winAtt.width, 1, 0, last_y);
  }
  if(last_xhair)
  {
    /* reset flags to starting values */
    first_xhair = 1;
    last_x = last_y = -1;
  }
  else
  {
    /* draw a new xhair in current mouse pos */
    XDrawLine(mydisplay, mywindow, mygc, 0, y, winAtt.width, y);
    XDrawLine(mydisplay, mywindow, mygc, x, 0, x, winAtt.height);
    first_xhair = 0;

    last_x = x;
    last_y = y;
  }
  return;
}


void SetColours() {
// set colors using either XAllocColor where colors are given by RGB values or
//                         XAllocNamedColor where colors are given by hex name
	int i;

  if ( colorset == 1 ) return;

  if ( colormode == 1 ) {
    printf ("Setting colors XAllocColor ...\n");
rainbow[0].red=253;rainbow[0].green=0;rainbow[0].blue=255;
rainbow[1].red=248;rainbow[1].green=0;rainbow[1].blue=255;
rainbow[2].red=243;rainbow[2].green=0;rainbow[2].blue=255;
rainbow[3].red=238;rainbow[3].green=0;rainbow[3].blue=255;
rainbow[4].red=233;rainbow[4].green=0;rainbow[4].blue=255;
rainbow[5].red=228;rainbow[5].green=0;rainbow[5].blue=255;
rainbow[6].red=223;rainbow[6].green=0;rainbow[6].blue=255;
rainbow[7].red=218;rainbow[7].green=0;rainbow[7].blue=255;
rainbow[8].red=213;rainbow[8].green=0;rainbow[8].blue=255;
rainbow[9].red=208;rainbow[9].green=0;rainbow[9].blue=255;
rainbow[10].red=203;rainbow[10].green=0;rainbow[10].blue=255;
rainbow[11].red=198;rainbow[11].green=0;rainbow[11].blue=255;
rainbow[12].red=193;rainbow[12].green=0;rainbow[12].blue=255;
rainbow[13].red=188;rainbow[13].green=0;rainbow[13].blue=255;
rainbow[14].red=183;rainbow[14].green=0;rainbow[14].blue=255;
rainbow[15].red=178;rainbow[15].green=0;rainbow[15].blue=255;
rainbow[16].red=173;rainbow[16].green=0;rainbow[16].blue=255;
rainbow[17].red=168;rainbow[17].green=0;rainbow[17].blue=255;
rainbow[18].red=163;rainbow[18].green=0;rainbow[18].blue=255;
rainbow[19].red=158;rainbow[19].green=0;rainbow[19].blue=255;
rainbow[20].red=153;rainbow[20].green=0;rainbow[20].blue=255;
rainbow[21].red=148;rainbow[21].green=0;rainbow[21].blue=255;
rainbow[22].red=143;rainbow[22].green=0;rainbow[22].blue=255;
rainbow[23].red=138;rainbow[23].green=0;rainbow[23].blue=255;
rainbow[24].red=133;rainbow[24].green=0;rainbow[24].blue=255;
rainbow[25].red=128;rainbow[25].green=0;rainbow[25].blue=255;
rainbow[26].red=123;rainbow[26].green=0;rainbow[26].blue=255;
rainbow[27].red=118;rainbow[27].green=0;rainbow[27].blue=255;
rainbow[28].red=113;rainbow[28].green=0;rainbow[28].blue=255;
rainbow[29].red=108;rainbow[29].green=0;rainbow[29].blue=255;
rainbow[30].red=103;rainbow[30].green=0;rainbow[30].blue=255;
rainbow[31].red=98;rainbow[31].green=0;rainbow[31].blue=255;
rainbow[32].red=93;rainbow[32].green=0;rainbow[32].blue=255;
rainbow[33].red=88;rainbow[33].green=0;rainbow[33].blue=255;
rainbow[34].red=83;rainbow[34].green=0;rainbow[34].blue=255;
rainbow[35].red=78;rainbow[35].green=0;rainbow[35].blue=255;
rainbow[36].red=73;rainbow[36].green=0;rainbow[36].blue=255;
rainbow[37].red=68;rainbow[37].green=0;rainbow[37].blue=255;
rainbow[38].red=63;rainbow[38].green=0;rainbow[38].blue=255;
rainbow[39].red=58;rainbow[39].green=0;rainbow[39].blue=255;
rainbow[40].red=53;rainbow[40].green=0;rainbow[40].blue=255;
rainbow[41].red=48;rainbow[41].green=0;rainbow[41].blue=255;
rainbow[42].red=43;rainbow[42].green=0;rainbow[42].blue=255;
rainbow[43].red=38;rainbow[43].green=0;rainbow[43].blue=255;
rainbow[44].red=33;rainbow[44].green=0;rainbow[44].blue=255;
rainbow[45].red=28;rainbow[45].green=0;rainbow[45].blue=255;
rainbow[46].red=23;rainbow[46].green=0;rainbow[46].blue=255;
rainbow[47].red=18;rainbow[47].green=0;rainbow[47].blue=255;
rainbow[48].red=13;rainbow[48].green=0;rainbow[48].blue=255;
rainbow[49].red=8;rainbow[49].green=0;rainbow[49].blue=255;
rainbow[50].red=3;rainbow[50].green=0;rainbow[50].blue=255;
rainbow[51].red=0;rainbow[51].green=1;rainbow[51].blue=255;
rainbow[52].red=0;rainbow[52].green=6;rainbow[52].blue=255;
rainbow[53].red=0;rainbow[53].green=11;rainbow[53].blue=255;
rainbow[54].red=0;rainbow[54].green=16;rainbow[54].blue=255;
rainbow[55].red=0;rainbow[55].green=21;rainbow[55].blue=255;
rainbow[56].red=0;rainbow[56].green=26;rainbow[56].blue=255;
rainbow[57].red=0;rainbow[57].green=31;rainbow[57].blue=255;
rainbow[58].red=0;rainbow[58].green=36;rainbow[58].blue=255;
rainbow[59].red=0;rainbow[59].green=41;rainbow[59].blue=255;
rainbow[60].red=0;rainbow[60].green=46;rainbow[60].blue=255;
rainbow[61].red=0;rainbow[61].green=51;rainbow[61].blue=255;
rainbow[62].red=0;rainbow[62].green=56;rainbow[62].blue=255;
rainbow[63].red=0;rainbow[63].green=61;rainbow[63].blue=255;
rainbow[64].red=0;rainbow[64].green=66;rainbow[64].blue=255;
rainbow[65].red=0;rainbow[65].green=71;rainbow[65].blue=255;
rainbow[66].red=0;rainbow[66].green=76;rainbow[66].blue=255;
rainbow[67].red=0;rainbow[67].green=81;rainbow[67].blue=255;
rainbow[68].red=0;rainbow[68].green=86;rainbow[68].blue=255;
rainbow[69].red=0;rainbow[69].green=91;rainbow[69].blue=255;
rainbow[70].red=0;rainbow[70].green=96;rainbow[70].blue=255;
rainbow[71].red=0;rainbow[71].green=101;rainbow[71].blue=255;
rainbow[72].red=0;rainbow[72].green=106;rainbow[72].blue=255;
rainbow[73].red=0;rainbow[73].green=111;rainbow[73].blue=255;
rainbow[74].red=0;rainbow[74].green=116;rainbow[74].blue=255;
rainbow[75].red=0;rainbow[75].green=121;rainbow[75].blue=255;
rainbow[76].red=0;rainbow[76].green=126;rainbow[76].blue=255;
rainbow[77].red=0;rainbow[77].green=131;rainbow[77].blue=255;
rainbow[78].red=0;rainbow[78].green=136;rainbow[78].blue=255;
rainbow[79].red=0;rainbow[79].green=141;rainbow[79].blue=255;
rainbow[80].red=0;rainbow[80].green=146;rainbow[80].blue=255;
rainbow[81].red=0;rainbow[81].green=151;rainbow[81].blue=255;
rainbow[82].red=0;rainbow[82].green=156;rainbow[82].blue=255;
rainbow[83].red=0;rainbow[83].green=161;rainbow[83].blue=255;
rainbow[84].red=0;rainbow[84].green=166;rainbow[84].blue=255;
rainbow[85].red=0;rainbow[85].green=171;rainbow[85].blue=255;
rainbow[86].red=0;rainbow[86].green=176;rainbow[86].blue=255;
rainbow[87].red=0;rainbow[87].green=181;rainbow[87].blue=255;
rainbow[88].red=0;rainbow[88].green=186;rainbow[88].blue=255;
rainbow[89].red=0;rainbow[89].green=191;rainbow[89].blue=255;
rainbow[90].red=0;rainbow[90].green=196;rainbow[90].blue=255;
rainbow[91].red=0;rainbow[91].green=201;rainbow[91].blue=255;
rainbow[92].red=0;rainbow[92].green=206;rainbow[92].blue=255;
rainbow[93].red=0;rainbow[93].green=211;rainbow[93].blue=255;
rainbow[94].red=0;rainbow[94].green=216;rainbow[94].blue=255;
rainbow[95].red=0;rainbow[95].green=221;rainbow[95].blue=255;
rainbow[96].red=0;rainbow[96].green=226;rainbow[96].blue=255;
rainbow[97].red=0;rainbow[97].green=231;rainbow[97].blue=255;
rainbow[98].red=0;rainbow[98].green=236;rainbow[98].blue=255;
rainbow[99].red=0;rainbow[99].green=241;rainbow[99].blue=255;
rainbow[100].red=0;rainbow[100].green=246;rainbow[100].blue=255;
rainbow[101].red=0;rainbow[101].green=251;rainbow[101].blue=255;
rainbow[102].red=0;rainbow[102].green=255;rainbow[102].blue=255;
rainbow[103].red=0;rainbow[103].green=255;rainbow[103].blue=250;
rainbow[104].red=0;rainbow[104].green=255;rainbow[104].blue=245;
rainbow[105].red=0;rainbow[105].green=255;rainbow[105].blue=240;
rainbow[106].red=0;rainbow[106].green=255;rainbow[106].blue=235;
rainbow[107].red=0;rainbow[107].green=255;rainbow[107].blue=230;
rainbow[108].red=0;rainbow[108].green=255;rainbow[108].blue=225;
rainbow[109].red=0;rainbow[109].green=255;rainbow[109].blue=220;
rainbow[110].red=0;rainbow[110].green=255;rainbow[110].blue=215;
rainbow[111].red=0;rainbow[111].green=255;rainbow[111].blue=210;
rainbow[112].red=0;rainbow[112].green=255;rainbow[112].blue=205;
rainbow[113].red=0;rainbow[113].green=255;rainbow[113].blue=200;
rainbow[114].red=0;rainbow[114].green=255;rainbow[114].blue=195;
rainbow[115].red=0;rainbow[115].green=255;rainbow[115].blue=190;
rainbow[116].red=0;rainbow[116].green=255;rainbow[116].blue=185;
rainbow[117].red=0;rainbow[117].green=255;rainbow[117].blue=180;
rainbow[118].red=0;rainbow[118].green=255;rainbow[118].blue=175;
rainbow[119].red=0;rainbow[119].green=255;rainbow[119].blue=170;
rainbow[120].red=0;rainbow[120].green=255;rainbow[120].blue=165;
rainbow[121].red=0;rainbow[121].green=255;rainbow[121].blue=160;
rainbow[122].red=0;rainbow[122].green=255;rainbow[122].blue=155;
rainbow[123].red=0;rainbow[123].green=255;rainbow[123].blue=150;
rainbow[124].red=0;rainbow[124].green=255;rainbow[124].blue=145;
rainbow[125].red=0;rainbow[125].green=255;rainbow[125].blue=140;
rainbow[126].red=0;rainbow[126].green=255;rainbow[126].blue=135;
rainbow[127].red=0;rainbow[127].green=255;rainbow[127].blue=130;
rainbow[128].red=0;rainbow[128].green=255;rainbow[128].blue=125;
rainbow[129].red=0;rainbow[129].green=255;rainbow[129].blue=120;
rainbow[130].red=0;rainbow[130].green=255;rainbow[130].blue=115;
rainbow[131].red=0;rainbow[131].green=255;rainbow[131].blue=110;
rainbow[132].red=0;rainbow[132].green=255;rainbow[132].blue=105;
rainbow[133].red=0;rainbow[133].green=255;rainbow[133].blue=100;
rainbow[134].red=0;rainbow[134].green=255;rainbow[134].blue=95;
rainbow[135].red=0;rainbow[135].green=255;rainbow[135].blue=90;
rainbow[136].red=0;rainbow[136].green=255;rainbow[136].blue=85;
rainbow[137].red=0;rainbow[137].green=255;rainbow[137].blue=80;
rainbow[138].red=0;rainbow[138].green=255;rainbow[138].blue=75;
rainbow[139].red=0;rainbow[139].green=255;rainbow[139].blue=70;
rainbow[140].red=0;rainbow[140].green=255;rainbow[140].blue=65;
rainbow[141].red=0;rainbow[141].green=255;rainbow[141].blue=60;
rainbow[142].red=0;rainbow[142].green=255;rainbow[142].blue=55;
rainbow[143].red=0;rainbow[143].green=255;rainbow[143].blue=50;
rainbow[144].red=0;rainbow[144].green=255;rainbow[144].blue=45;
rainbow[145].red=0;rainbow[145].green=255;rainbow[145].blue=40;
rainbow[146].red=0;rainbow[146].green=255;rainbow[146].blue=35;
rainbow[147].red=0;rainbow[147].green=255;rainbow[147].blue=30;
rainbow[148].red=0;rainbow[148].green=255;rainbow[148].blue=25;
rainbow[149].red=0;rainbow[149].green=255;rainbow[149].blue=20;
rainbow[150].red=0;rainbow[150].green=255;rainbow[150].blue=15;
rainbow[151].red=0;rainbow[151].green=255;rainbow[151].blue=10;
rainbow[152].red=0;rainbow[152].green=255;rainbow[152].blue=5;
rainbow[153].red=0;rainbow[153].green=255;rainbow[153].blue=0;
rainbow[154].red=4;rainbow[154].green=255;rainbow[154].blue=0;
rainbow[155].red=9;rainbow[155].green=255;rainbow[155].blue=0;
rainbow[156].red=14;rainbow[156].green=255;rainbow[156].blue=0;
rainbow[157].red=19;rainbow[157].green=255;rainbow[157].blue=0;
rainbow[158].red=24;rainbow[158].green=255;rainbow[158].blue=0;
rainbow[159].red=29;rainbow[159].green=255;rainbow[159].blue=0;
rainbow[160].red=34;rainbow[160].green=255;rainbow[160].blue=0;
rainbow[161].red=39;rainbow[161].green=255;rainbow[161].blue=0;
rainbow[162].red=44;rainbow[162].green=255;rainbow[162].blue=0;
rainbow[163].red=49;rainbow[163].green=255;rainbow[163].blue=0;
rainbow[164].red=54;rainbow[164].green=255;rainbow[164].blue=0;
rainbow[165].red=59;rainbow[165].green=255;rainbow[165].blue=0;
rainbow[166].red=64;rainbow[166].green=255;rainbow[166].blue=0;
rainbow[167].red=69;rainbow[167].green=255;rainbow[167].blue=0;
rainbow[168].red=74;rainbow[168].green=255;rainbow[168].blue=0;
rainbow[169].red=79;rainbow[169].green=255;rainbow[169].blue=0;
rainbow[170].red=84;rainbow[170].green=255;rainbow[170].blue=0;
rainbow[171].red=89;rainbow[171].green=255;rainbow[171].blue=0;
rainbow[172].red=94;rainbow[172].green=255;rainbow[172].blue=0;
rainbow[173].red=99;rainbow[173].green=255;rainbow[173].blue=0;
rainbow[174].red=104;rainbow[174].green=255;rainbow[174].blue=0;
rainbow[175].red=109;rainbow[175].green=255;rainbow[175].blue=0;
rainbow[176].red=114;rainbow[176].green=255;rainbow[176].blue=0;
rainbow[177].red=119;rainbow[177].green=255;rainbow[177].blue=0;
rainbow[178].red=124;rainbow[178].green=255;rainbow[178].blue=0;
rainbow[179].red=129;rainbow[179].green=255;rainbow[179].blue=0;
rainbow[180].red=134;rainbow[180].green=255;rainbow[180].blue=0;
rainbow[181].red=139;rainbow[181].green=255;rainbow[181].blue=0;
rainbow[182].red=144;rainbow[182].green=255;rainbow[182].blue=0;
rainbow[183].red=149;rainbow[183].green=255;rainbow[183].blue=0;
rainbow[184].red=154;rainbow[184].green=255;rainbow[184].blue=0;
rainbow[185].red=159;rainbow[185].green=255;rainbow[185].blue=0;
rainbow[186].red=164;rainbow[186].green=255;rainbow[186].blue=0;
rainbow[187].red=169;rainbow[187].green=255;rainbow[187].blue=0;
rainbow[188].red=174;rainbow[188].green=255;rainbow[188].blue=0;
rainbow[189].red=179;rainbow[189].green=255;rainbow[189].blue=0;
rainbow[190].red=184;rainbow[190].green=255;rainbow[190].blue=0;
rainbow[191].red=189;rainbow[191].green=255;rainbow[191].blue=0;
rainbow[192].red=194;rainbow[192].green=255;rainbow[192].blue=0;
rainbow[193].red=199;rainbow[193].green=255;rainbow[193].blue=0;
rainbow[194].red=204;rainbow[194].green=255;rainbow[194].blue=0;
rainbow[195].red=209;rainbow[195].green=255;rainbow[195].blue=0;
rainbow[196].red=214;rainbow[196].green=255;rainbow[196].blue=0;
rainbow[197].red=219;rainbow[197].green=255;rainbow[197].blue=0;
rainbow[198].red=224;rainbow[198].green=255;rainbow[198].blue=0;
rainbow[199].red=229;rainbow[199].green=255;rainbow[199].blue=0;
rainbow[200].red=234;rainbow[200].green=255;rainbow[200].blue=0;
rainbow[201].red=239;rainbow[201].green=255;rainbow[201].blue=0;
rainbow[202].red=244;rainbow[202].green=255;rainbow[202].blue=0;
rainbow[203].red=249;rainbow[203].green=255;rainbow[203].blue=0;
rainbow[204].red=254;rainbow[204].green=255;rainbow[204].blue=0;
rainbow[205].red=255;rainbow[205].green=252;rainbow[205].blue=0;
rainbow[206].red=255;rainbow[206].green=247;rainbow[206].blue=0;
rainbow[207].red=255;rainbow[207].green=242;rainbow[207].blue=0;
rainbow[208].red=255;rainbow[208].green=237;rainbow[208].blue=0;
rainbow[209].red=255;rainbow[209].green=232;rainbow[209].blue=0;
rainbow[210].red=255;rainbow[210].green=227;rainbow[210].blue=0;
rainbow[211].red=255;rainbow[211].green=222;rainbow[211].blue=0;
rainbow[212].red=255;rainbow[212].green=217;rainbow[212].blue=0;
rainbow[213].red=255;rainbow[213].green=212;rainbow[213].blue=0;
rainbow[214].red=255;rainbow[214].green=207;rainbow[214].blue=0;
rainbow[215].red=255;rainbow[215].green=202;rainbow[215].blue=0;
rainbow[216].red=255;rainbow[216].green=197;rainbow[216].blue=0;
rainbow[217].red=255;rainbow[217].green=192;rainbow[217].blue=0;
rainbow[218].red=255;rainbow[218].green=187;rainbow[218].blue=0;
rainbow[219].red=255;rainbow[219].green=182;rainbow[219].blue=0;
rainbow[220].red=255;rainbow[220].green=177;rainbow[220].blue=0;
rainbow[221].red=255;rainbow[221].green=172;rainbow[221].blue=0;
rainbow[222].red=255;rainbow[222].green=167;rainbow[222].blue=0;
rainbow[223].red=255;rainbow[223].green=162;rainbow[223].blue=0;
rainbow[224].red=255;rainbow[224].green=157;rainbow[224].blue=0;
rainbow[225].red=255;rainbow[225].green=152;rainbow[225].blue=0;
rainbow[226].red=255;rainbow[226].green=147;rainbow[226].blue=0;
rainbow[227].red=255;rainbow[227].green=142;rainbow[227].blue=0;
rainbow[228].red=255;rainbow[228].green=137;rainbow[228].blue=0;
rainbow[229].red=255;rainbow[229].green=132;rainbow[229].blue=0;
rainbow[230].red=255;rainbow[230].green=127;rainbow[230].blue=0;
rainbow[231].red=255;rainbow[231].green=122;rainbow[231].blue=0;
rainbow[232].red=255;rainbow[232].green=117;rainbow[232].blue=0;
rainbow[233].red=255;rainbow[233].green=112;rainbow[233].blue=0;
rainbow[234].red=255;rainbow[234].green=107;rainbow[234].blue=0;
rainbow[235].red=255;rainbow[235].green=102;rainbow[235].blue=0;
rainbow[236].red=255;rainbow[236].green=97;rainbow[236].blue=0;
rainbow[237].red=255;rainbow[237].green=92;rainbow[237].blue=0;
rainbow[238].red=255;rainbow[238].green=87;rainbow[238].blue=0;
rainbow[239].red=255;rainbow[239].green=82;rainbow[239].blue=0;
rainbow[240].red=255;rainbow[240].green=77;rainbow[240].blue=0;
rainbow[241].red=255;rainbow[241].green=72;rainbow[241].blue=0;
rainbow[242].red=255;rainbow[242].green=67;rainbow[242].blue=0;
rainbow[243].red=255;rainbow[243].green=62;rainbow[243].blue=0;
rainbow[244].red=255;rainbow[244].green=57;rainbow[244].blue=0;
rainbow[245].red=255;rainbow[245].green=52;rainbow[245].blue=0;
rainbow[246].red=255;rainbow[246].green=47;rainbow[246].blue=0;
rainbow[247].red=255;rainbow[247].green=42;rainbow[247].blue=0;
rainbow[248].red=255;rainbow[248].green=37;rainbow[248].blue=0;
rainbow[249].red=255;rainbow[249].green=32;rainbow[249].blue=0;
rainbow[250].red=255;rainbow[250].green=27;rainbow[250].blue=0;
rainbow[251].red=255;rainbow[251].green=22;rainbow[251].blue=0;
rainbow[252].red=255;rainbow[252].green=17;rainbow[252].blue=0;
rainbow[253].red=255;rainbow[253].green=12;rainbow[253].blue=0;
rainbow[254].red=255;rainbow[254].green=7;rainbow[254].blue=0;
rainbow[255].red=255;rainbow[255].green=2;rainbow[255].blue=0;

polar[0].red=1;polar[0].green=1;polar[0].blue=255;
polar[1].red=3;polar[1].green=3;polar[1].blue=255;
polar[2].red=5;polar[2].green=5;polar[2].blue=255;
polar[3].red=7;polar[3].green=7;polar[3].blue=255;
polar[4].red=9;polar[4].green=9;polar[4].blue=255;
polar[5].red=11;polar[5].green=11;polar[5].blue=255;
polar[6].red=13;polar[6].green=13;polar[6].blue=255;
polar[7].red=15;polar[7].green=15;polar[7].blue=255;
polar[8].red=17;polar[8].green=17;polar[8].blue=255;
polar[9].red=19;polar[9].green=19;polar[9].blue=255;
polar[10].red=21;polar[10].green=21;polar[10].blue=255;
polar[11].red=23;polar[11].green=23;polar[11].blue=255;
polar[12].red=25;polar[12].green=25;polar[12].blue=255;
polar[13].red=27;polar[13].green=27;polar[13].blue=255;
polar[14].red=29;polar[14].green=29;polar[14].blue=255;
polar[15].red=31;polar[15].green=31;polar[15].blue=255;
polar[16].red=33;polar[16].green=33;polar[16].blue=255;
polar[17].red=35;polar[17].green=35;polar[17].blue=255;
polar[18].red=37;polar[18].green=37;polar[18].blue=255;
polar[19].red=39;polar[19].green=39;polar[19].blue=255;
polar[20].red=41;polar[20].green=41;polar[20].blue=255;
polar[21].red=43;polar[21].green=43;polar[21].blue=255;
polar[22].red=45;polar[22].green=45;polar[22].blue=255;
polar[23].red=47;polar[23].green=47;polar[23].blue=255;
polar[24].red=49;polar[24].green=49;polar[24].blue=255;
polar[25].red=51;polar[25].green=51;polar[25].blue=255;
polar[26].red=53;polar[26].green=53;polar[26].blue=255;
polar[27].red=55;polar[27].green=55;polar[27].blue=255;
polar[28].red=57;polar[28].green=57;polar[28].blue=255;
polar[29].red=59;polar[29].green=59;polar[29].blue=255;
polar[30].red=61;polar[30].green=61;polar[30].blue=255;
polar[31].red=63;polar[31].green=63;polar[31].blue=255;
polar[32].red=65;polar[32].green=65;polar[32].blue=255;
polar[33].red=67;polar[33].green=67;polar[33].blue=255;
polar[34].red=69;polar[34].green=69;polar[34].blue=255;
polar[35].red=71;polar[35].green=71;polar[35].blue=255;
polar[36].red=73;polar[36].green=73;polar[36].blue=255;
polar[37].red=75;polar[37].green=75;polar[37].blue=255;
polar[38].red=77;polar[38].green=77;polar[38].blue=255;
polar[39].red=79;polar[39].green=79;polar[39].blue=255;
polar[40].red=81;polar[40].green=81;polar[40].blue=255;
polar[41].red=83;polar[41].green=83;polar[41].blue=255;
polar[42].red=85;polar[42].green=85;polar[42].blue=255;
polar[43].red=87;polar[43].green=87;polar[43].blue=255;
polar[44].red=89;polar[44].green=89;polar[44].blue=255;
polar[45].red=91;polar[45].green=91;polar[45].blue=255;
polar[46].red=93;polar[46].green=93;polar[46].blue=255;
polar[47].red=95;polar[47].green=95;polar[47].blue=255;
polar[48].red=97;polar[48].green=97;polar[48].blue=255;
polar[49].red=99;polar[49].green=99;polar[49].blue=255;
polar[50].red=101;polar[50].green=101;polar[50].blue=255;
polar[51].red=103;polar[51].green=103;polar[51].blue=255;
polar[52].red=105;polar[52].green=105;polar[52].blue=255;
polar[53].red=107;polar[53].green=107;polar[53].blue=255;
polar[54].red=109;polar[54].green=109;polar[54].blue=255;
polar[55].red=111;polar[55].green=111;polar[55].blue=255;
polar[56].red=113;polar[56].green=113;polar[56].blue=255;
polar[57].red=115;polar[57].green=115;polar[57].blue=255;
polar[58].red=117;polar[58].green=117;polar[58].blue=255;
polar[59].red=119;polar[59].green=119;polar[59].blue=255;
polar[60].red=121;polar[60].green=121;polar[60].blue=255;
polar[61].red=123;polar[61].green=123;polar[61].blue=255;
polar[62].red=125;polar[62].green=125;polar[62].blue=255;
polar[63].red=127;polar[63].green=127;polar[63].blue=255;
polar[64].red=129;polar[64].green=129;polar[64].blue=255;
polar[65].red=130;polar[65].green=130;polar[65].blue=255;
polar[66].red=132;polar[66].green=132;polar[66].blue=255;
polar[67].red=134;polar[67].green=134;polar[67].blue=255;
polar[68].red=136;polar[68].green=136;polar[68].blue=255;
polar[69].red=138;polar[69].green=138;polar[69].blue=255;
polar[70].red=140;polar[70].green=140;polar[70].blue=255;
polar[71].red=142;polar[71].green=142;polar[71].blue=255;
polar[72].red=144;polar[72].green=144;polar[72].blue=255;
polar[73].red=146;polar[73].green=146;polar[73].blue=255;
polar[74].red=148;polar[74].green=148;polar[74].blue=255;
polar[75].red=150;polar[75].green=150;polar[75].blue=255;
polar[76].red=152;polar[76].green=152;polar[76].blue=255;
polar[77].red=154;polar[77].green=154;polar[77].blue=255;
polar[78].red=156;polar[78].green=156;polar[78].blue=255;
polar[79].red=158;polar[79].green=158;polar[79].blue=255;
polar[80].red=160;polar[80].green=160;polar[80].blue=255;
polar[81].red=162;polar[81].green=162;polar[81].blue=255;
polar[82].red=164;polar[82].green=164;polar[82].blue=255;
polar[83].red=166;polar[83].green=166;polar[83].blue=255;
polar[84].red=168;polar[84].green=168;polar[84].blue=255;
polar[85].red=170;polar[85].green=170;polar[85].blue=255;
polar[86].red=172;polar[86].green=172;polar[86].blue=255;
polar[87].red=174;polar[87].green=174;polar[87].blue=255;
polar[88].red=176;polar[88].green=176;polar[88].blue=255;
polar[89].red=178;polar[89].green=178;polar[89].blue=255;
polar[90].red=180;polar[90].green=180;polar[90].blue=255;
polar[91].red=182;polar[91].green=182;polar[91].blue=255;
polar[92].red=184;polar[92].green=184;polar[92].blue=255;
polar[93].red=186;polar[93].green=186;polar[93].blue=255;
polar[94].red=188;polar[94].green=188;polar[94].blue=255;
polar[95].red=190;polar[95].green=190;polar[95].blue=255;
polar[96].red=192;polar[96].green=192;polar[96].blue=255;
polar[97].red=194;polar[97].green=194;polar[97].blue=255;
polar[98].red=196;polar[98].green=196;polar[98].blue=255;
polar[99].red=198;polar[99].green=198;polar[99].blue=255;
polar[100].red=200;polar[100].green=200;polar[100].blue=255;
polar[101].red=202;polar[101].green=202;polar[101].blue=255;
polar[102].red=204;polar[102].green=204;polar[102].blue=255;
polar[103].red=206;polar[103].green=206;polar[103].blue=255;
polar[104].red=208;polar[104].green=208;polar[104].blue=255;
polar[105].red=210;polar[105].green=210;polar[105].blue=255;
polar[106].red=212;polar[106].green=212;polar[106].blue=255;
polar[107].red=214;polar[107].green=214;polar[107].blue=255;
polar[108].red=216;polar[108].green=216;polar[108].blue=255;
polar[109].red=218;polar[109].green=218;polar[109].blue=255;
polar[110].red=220;polar[110].green=220;polar[110].blue=255;
polar[111].red=222;polar[111].green=222;polar[111].blue=255;
polar[112].red=224;polar[112].green=224;polar[112].blue=255;
polar[113].red=226;polar[113].green=226;polar[113].blue=255;
polar[114].red=228;polar[114].green=228;polar[114].blue=255;
polar[115].red=230;polar[115].green=230;polar[115].blue=255;
polar[116].red=232;polar[116].green=232;polar[116].blue=255;
polar[117].red=234;polar[117].green=234;polar[117].blue=255;
polar[118].red=236;polar[118].green=236;polar[118].blue=255;
polar[119].red=238;polar[119].green=238;polar[119].blue=255;
polar[120].red=240;polar[120].green=240;polar[120].blue=255;
polar[121].red=242;polar[121].green=242;polar[121].blue=255;
polar[122].red=244;polar[122].green=244;polar[122].blue=255;
polar[123].red=246;polar[123].green=246;polar[123].blue=255;
polar[124].red=248;polar[124].green=248;polar[124].blue=255;
polar[125].red=250;polar[125].green=250;polar[125].blue=255;
polar[126].red=252;polar[126].green=252;polar[126].blue=255;
polar[127].red=254;polar[127].green=254;polar[127].blue=255;
polar[128].red=255;polar[128].green=254;polar[128].blue=254;
polar[129].red=255;polar[129].green=252;polar[129].blue=252;
polar[130].red=255;polar[130].green=250;polar[130].blue=250;
polar[131].red=255;polar[131].green=248;polar[131].blue=248;
polar[132].red=255;polar[132].green=246;polar[132].blue=246;
polar[133].red=255;polar[133].green=244;polar[133].blue=244;
polar[134].red=255;polar[134].green=242;polar[134].blue=242;
polar[135].red=255;polar[135].green=240;polar[135].blue=240;
polar[136].red=255;polar[136].green=238;polar[136].blue=238;
polar[137].red=255;polar[137].green=236;polar[137].blue=236;
polar[138].red=255;polar[138].green=234;polar[138].blue=234;
polar[139].red=255;polar[139].green=232;polar[139].blue=232;
polar[140].red=255;polar[140].green=230;polar[140].blue=230;
polar[141].red=255;polar[141].green=228;polar[141].blue=228;
polar[142].red=255;polar[142].green=226;polar[142].blue=226;
polar[143].red=255;polar[143].green=224;polar[143].blue=224;
polar[144].red=255;polar[144].green=222;polar[144].blue=222;
polar[145].red=255;polar[145].green=220;polar[145].blue=220;
polar[146].red=255;polar[146].green=218;polar[146].blue=218;
polar[147].red=255;polar[147].green=216;polar[147].blue=216;
polar[148].red=255;polar[148].green=214;polar[148].blue=214;
polar[149].red=255;polar[149].green=212;polar[149].blue=212;
polar[150].red=255;polar[150].green=210;polar[150].blue=210;
polar[151].red=255;polar[151].green=208;polar[151].blue=208;
polar[152].red=255;polar[152].green=206;polar[152].blue=206;
polar[153].red=255;polar[153].green=204;polar[153].blue=204;
polar[154].red=255;polar[154].green=202;polar[154].blue=202;
polar[155].red=255;polar[155].green=200;polar[155].blue=200;
polar[156].red=255;polar[156].green=198;polar[156].blue=198;
polar[157].red=255;polar[157].green=196;polar[157].blue=196;
polar[158].red=255;polar[158].green=194;polar[158].blue=194;
polar[159].red=255;polar[159].green=192;polar[159].blue=192;
polar[160].red=255;polar[160].green=190;polar[160].blue=190;
polar[161].red=255;polar[161].green=188;polar[161].blue=188;
polar[162].red=255;polar[162].green=186;polar[162].blue=186;
polar[163].red=255;polar[163].green=184;polar[163].blue=184;
polar[164].red=255;polar[164].green=182;polar[164].blue=182;
polar[165].red=255;polar[165].green=180;polar[165].blue=180;
polar[166].red=255;polar[166].green=178;polar[166].blue=178;
polar[167].red=255;polar[167].green=176;polar[167].blue=176;
polar[168].red=255;polar[168].green=174;polar[168].blue=174;
polar[169].red=255;polar[169].green=172;polar[169].blue=172;
polar[170].red=255;polar[170].green=170;polar[170].blue=170;
polar[171].red=255;polar[171].green=168;polar[171].blue=168;
polar[172].red=255;polar[172].green=166;polar[172].blue=166;
polar[173].red=255;polar[173].green=164;polar[173].blue=164;
polar[174].red=255;polar[174].green=162;polar[174].blue=162;
polar[175].red=255;polar[175].green=160;polar[175].blue=160;
polar[176].red=255;polar[176].green=158;polar[176].blue=158;
polar[177].red=255;polar[177].green=156;polar[177].blue=156;
polar[178].red=255;polar[178].green=154;polar[178].blue=154;
polar[179].red=255;polar[179].green=152;polar[179].blue=152;
polar[180].red=255;polar[180].green=150;polar[180].blue=150;
polar[181].red=255;polar[181].green=148;polar[181].blue=148;
polar[182].red=255;polar[182].green=146;polar[182].blue=146;
polar[183].red=255;polar[183].green=144;polar[183].blue=144;
polar[184].red=255;polar[184].green=142;polar[184].blue=142;
polar[185].red=255;polar[185].green=140;polar[185].blue=140;
polar[186].red=255;polar[186].green=138;polar[186].blue=138;
polar[187].red=255;polar[187].green=136;polar[187].blue=136;
polar[188].red=255;polar[188].green=134;polar[188].blue=134;
polar[189].red=255;polar[189].green=132;polar[189].blue=132;
polar[190].red=255;polar[190].green=130;polar[190].blue=130;
polar[191].red=255;polar[191].green=129;polar[191].blue=129;
polar[192].red=255;polar[192].green=127;polar[192].blue=127;
polar[193].red=255;polar[193].green=125;polar[193].blue=125;
polar[194].red=255;polar[194].green=123;polar[194].blue=123;
polar[195].red=255;polar[195].green=121;polar[195].blue=121;
polar[196].red=255;polar[196].green=119;polar[196].blue=119;
polar[197].red=255;polar[197].green=117;polar[197].blue=117;
polar[198].red=255;polar[198].green=115;polar[198].blue=115;
polar[199].red=255;polar[199].green=113;polar[199].blue=113;
polar[200].red=255;polar[200].green=111;polar[200].blue=111;
polar[201].red=255;polar[201].green=109;polar[201].blue=109;
polar[202].red=255;polar[202].green=107;polar[202].blue=107;
polar[203].red=255;polar[203].green=105;polar[203].blue=105;
polar[204].red=255;polar[204].green=103;polar[204].blue=103;
polar[205].red=255;polar[205].green=101;polar[205].blue=101;
polar[206].red=255;polar[206].green=99;polar[206].blue=99;
polar[207].red=255;polar[207].green=97;polar[207].blue=97;
polar[208].red=255;polar[208].green=95;polar[208].blue=95;
polar[209].red=255;polar[209].green=93;polar[209].blue=93;
polar[210].red=255;polar[210].green=91;polar[210].blue=91;
polar[211].red=255;polar[211].green=89;polar[211].blue=89;
polar[212].red=255;polar[212].green=87;polar[212].blue=87;
polar[213].red=255;polar[213].green=85;polar[213].blue=85;
polar[214].red=255;polar[214].green=83;polar[214].blue=83;
polar[215].red=255;polar[215].green=81;polar[215].blue=81;
polar[216].red=255;polar[216].green=79;polar[216].blue=79;
polar[217].red=255;polar[217].green=77;polar[217].blue=77;
polar[218].red=255;polar[218].green=75;polar[218].blue=75;
polar[219].red=255;polar[219].green=73;polar[219].blue=73;
polar[220].red=255;polar[220].green=71;polar[220].blue=71;
polar[221].red=255;polar[221].green=69;polar[221].blue=69;
polar[222].red=255;polar[222].green=67;polar[222].blue=67;
polar[223].red=255;polar[223].green=65;polar[223].blue=65;
polar[224].red=255;polar[224].green=63;polar[224].blue=63;
polar[225].red=255;polar[225].green=61;polar[225].blue=61;
polar[226].red=255;polar[226].green=59;polar[226].blue=59;
polar[227].red=255;polar[227].green=57;polar[227].blue=57;
polar[228].red=255;polar[228].green=55;polar[228].blue=55;
polar[229].red=255;polar[229].green=53;polar[229].blue=53;
polar[230].red=255;polar[230].green=51;polar[230].blue=51;
polar[231].red=255;polar[231].green=49;polar[231].blue=49;
polar[232].red=255;polar[232].green=47;polar[232].blue=47;
polar[233].red=255;polar[233].green=45;polar[233].blue=45;
polar[234].red=255;polar[234].green=43;polar[234].blue=43;
polar[235].red=255;polar[235].green=41;polar[235].blue=41;
polar[236].red=255;polar[236].green=39;polar[236].blue=39;
polar[237].red=255;polar[237].green=37;polar[237].blue=37;
polar[238].red=255;polar[238].green=35;polar[238].blue=35;
polar[239].red=255;polar[239].green=33;polar[239].blue=33;
polar[240].red=255;polar[240].green=31;polar[240].blue=31;
polar[241].red=255;polar[241].green=29;polar[241].blue=29;
polar[242].red=255;polar[242].green=27;polar[242].blue=27;
polar[243].red=255;polar[243].green=25;polar[243].blue=25;
polar[244].red=255;polar[244].green=23;polar[244].blue=23;
polar[245].red=255;polar[245].green=21;polar[245].blue=21;
polar[246].red=255;polar[246].green=19;polar[246].blue=19;
polar[247].red=255;polar[247].green=17;polar[247].blue=17;
polar[248].red=255;polar[248].green=15;polar[248].blue=15;
polar[249].red=255;polar[249].green=13;polar[249].blue=13;
polar[250].red=255;polar[250].green=11;polar[250].blue=11;
polar[251].red=255;polar[251].green=9;polar[251].blue=9;
polar[252].red=255;polar[252].green=7;polar[252].blue=7;
polar[253].red=255;polar[253].green=5;polar[253].blue=5;
polar[254].red=255;polar[254].green=3;polar[254].blue=3;
polar[255].red=255;polar[255].green=1;polar[255].blue=1;

seis[0].red=0;seis[0].green=1;seis[0].blue=206;
seis[1].red=0;seis[1].green=4;seis[1].blue=208;
seis[2].red=0;seis[2].green=7;seis[2].blue=209;
seis[3].red=0;seis[3].green=10;seis[3].blue=211;
seis[4].red=0;seis[4].green=13;seis[4].blue=213;
seis[5].red=0;seis[5].green=15;seis[5].blue=215;
seis[6].red=0;seis[6].green=18;seis[6].blue=216;
seis[7].red=0;seis[7].green=21;seis[7].blue=218;
seis[8].red=0;seis[8].green=24;seis[8].blue=220;
seis[9].red=0;seis[9].green=27;seis[9].blue=222;
seis[10].red=0;seis[10].green=30;seis[10].blue=223;
seis[11].red=0;seis[11].green=32;seis[11].blue=225;
seis[12].red=0;seis[12].green=35;seis[12].blue=227;
seis[13].red=0;seis[13].green=38;seis[13].blue=229;
seis[14].red=0;seis[14].green=41;seis[14].blue=230;
seis[15].red=0;seis[15].green=44;seis[15].blue=232;
seis[16].red=0;seis[16].green=46;seis[16].blue=234;
seis[17].red=0;seis[17].green=49;seis[17].blue=236;
seis[18].red=0;seis[18].green=52;seis[18].blue=238;
seis[19].red=0;seis[19].green=55;seis[19].blue=239;
seis[20].red=0;seis[20].green=58;seis[20].blue=241;
seis[21].red=0;seis[21].green=60;seis[21].blue=243;
seis[22].red=0;seis[22].green=63;seis[22].blue=245;
seis[23].red=0;seis[23].green=66;seis[23].blue=246;
seis[24].red=0;seis[24].green=69;seis[24].blue=248;
seis[25].red=0;seis[25].green=72;seis[25].blue=250;
seis[26].red=0;seis[26].green=75;seis[26].blue=252;
seis[27].red=0;seis[27].green=77;seis[27].blue=253;
seis[28].red=0;seis[28].green=80;seis[28].blue=255;
seis[29].red=0;seis[29].green=86;seis[29].blue=250;
seis[30].red=0;seis[30].green=92;seis[30].blue=245;
seis[31].red=0;seis[31].green=97;seis[31].blue=239;
seis[32].red=0;seis[32].green=103;seis[32].blue=234;
seis[33].red=0;seis[33].green=108;seis[33].blue=229;
seis[34].red=0;seis[34].green=114;seis[34].blue=224;
seis[35].red=0;seis[35].green=120;seis[35].blue=219;
seis[36].red=0;seis[36].green=125;seis[36].blue=214;
seis[37].red=0;seis[37].green=131;seis[37].blue=209;
seis[38].red=0;seis[38].green=137;seis[38].blue=204;
seis[39].red=0;seis[39].green=142;seis[39].blue=199;
seis[40].red=0;seis[40].green=148;seis[40].blue=194;
seis[41].red=0;seis[41].green=153;seis[41].blue=188;
seis[42].red=0;seis[42].green=159;seis[42].blue=183;
seis[43].red=0;seis[43].green=165;seis[43].blue=178;
seis[44].red=0;seis[44].green=170;seis[44].blue=173;
seis[45].red=0;seis[45].green=176;seis[45].blue=168;
seis[46].red=0;seis[46].green=182;seis[46].blue=163;
seis[47].red=0;seis[47].green=187;seis[47].blue=158;
seis[48].red=0;seis[48].green=193;seis[48].blue=153;
seis[49].red=0;seis[49].green=198;seis[49].blue=148;
seis[50].red=0;seis[50].green=204;seis[50].blue=143;
seis[51].red=0;seis[51].green=210;seis[51].blue=137;
seis[52].red=0;seis[52].green=215;seis[52].blue=132;
seis[53].red=0;seis[53].green=221;seis[53].blue=127;
seis[54].red=0;seis[54].green=227;seis[54].blue=122;
seis[55].red=0;seis[55].green=232;seis[55].blue=117;
seis[56].red=0;seis[56].green=238;seis[56].blue=112;
seis[57].red=2;seis[57].green=240;seis[57].blue=108;
seis[58].red=5;seis[58].green=241;seis[58].blue=105;
seis[59].red=8;seis[59].green=241;seis[59].blue=103;
seis[60].red=11;seis[60].green=242;seis[60].blue=100;
seis[61].red=15;seis[61].green=242;seis[61].blue=97;
seis[62].red=18;seis[62].green=243;seis[62].blue=94;
seis[63].red=21;seis[63].green=243;seis[63].blue=91;
seis[64].red=24;seis[64].green=244;seis[64].blue=89;
seis[65].red=27;seis[65].green=245;seis[65].blue=86;
seis[66].red=30;seis[66].green=245;seis[66].blue=83;
seis[67].red=34;seis[67].green=246;seis[67].blue=80;
seis[68].red=37;seis[68].green=246;seis[68].blue=77;
seis[69].red=40;seis[69].green=247;seis[69].blue=75;
seis[70].red=43;seis[70].green=247;seis[70].blue=72;
seis[71].red=46;seis[71].green=248;seis[71].blue=69;
seis[72].red=49;seis[72].green=248;seis[72].blue=66;
seis[73].red=53;seis[73].green=249;seis[73].blue=63;
seis[74].red=56;seis[74].green=249;seis[74].blue=60;
seis[75].red=59;seis[75].green=250;seis[75].blue=58;
seis[76].red=62;seis[76].green=250;seis[76].blue=55;
seis[77].red=65;seis[77].green=251;seis[77].blue=52;
seis[78].red=68;seis[78].green=251;seis[78].blue=49;
seis[79].red=72;seis[79].green=252;seis[79].blue=46;
seis[80].red=75;seis[80].green=252;seis[80].blue=44;
seis[81].red=78;seis[81].green=253;seis[81].blue=41;
seis[82].red=81;seis[82].green=254;seis[82].blue=38;
seis[83].red=84;seis[83].green=254;seis[83].blue=35;
seis[84].red=87;seis[84].green=255;seis[84].blue=32;
seis[85].red=91;seis[85].green=255;seis[85].blue=30;
seis[86].red=97;seis[86].green=255;seis[86].blue=29;
seis[87].red=103;seis[87].green=255;seis[87].blue=28;
seis[88].red=108;seis[88].green=255;seis[88].blue=27;
seis[89].red=114;seis[89].green=255;seis[89].blue=26;
seis[90].red=120;seis[90].green=255;seis[90].blue=25;
seis[91].red=126;seis[91].green=255;seis[91].blue=23;
seis[92].red=132;seis[92].green=255;seis[92].blue=22;
seis[93].red=137;seis[93].green=255;seis[93].blue=21;
seis[94].red=143;seis[94].green=255;seis[94].blue=20;
seis[95].red=149;seis[95].green=255;seis[95].blue=19;
seis[96].red=155;seis[96].green=255;seis[96].blue=18;
seis[97].red=161;seis[97].green=255;seis[97].blue=17;
seis[98].red=166;seis[98].green=255;seis[98].blue=16;
seis[99].red=172;seis[99].green=255;seis[99].blue=15;
seis[100].red=178;seis[100].green=255;seis[100].blue=14;
seis[101].red=184;seis[101].green=255;seis[101].blue=13;
seis[102].red=190;seis[102].green=255;seis[102].blue=12;
seis[103].red=195;seis[103].green=255;seis[103].blue=11;
seis[104].red=201;seis[104].green=255;seis[104].blue=10;
seis[105].red=207;seis[105].green=255;seis[105].blue=9;
seis[106].red=213;seis[106].green=255;seis[106].blue=8;
seis[107].red=219;seis[107].green=255;seis[107].blue=7;
seis[108].red=224;seis[108].green=255;seis[108].blue=6;
seis[109].red=230;seis[109].green=255;seis[109].blue=5;
seis[110].red=236;seis[110].green=255;seis[110].blue=3;
seis[111].red=242;seis[111].green=255;seis[111].blue=2;
seis[112].red=248;seis[112].green=255;seis[112].blue=1;
seis[113].red=253;seis[113].green=255;seis[113].blue=0;
seis[114].red=255;seis[114].green=255;seis[114].blue=0;
seis[115].red=255;seis[115].green=255;seis[115].blue=0;
seis[116].red=255;seis[116].green=255;seis[116].blue=0;
seis[117].red=255;seis[117].green=255;seis[117].blue=0;
seis[118].red=255;seis[118].green=255;seis[118].blue=0;
seis[119].red=255;seis[119].green=255;seis[119].blue=0;
seis[120].red=255;seis[120].green=255;seis[120].blue=0;
seis[121].red=255;seis[121].green=255;seis[121].blue=0;
seis[122].red=255;seis[122].green=255;seis[122].blue=0;
seis[123].red=255;seis[123].green=255;seis[123].blue=0;
seis[124].red=255;seis[124].green=255;seis[124].blue=0;
seis[125].red=255;seis[125].green=255;seis[125].blue=0;
seis[126].red=255;seis[126].green=255;seis[126].blue=0;
seis[127].red=255;seis[127].green=255;seis[127].blue=0;
seis[128].red=255;seis[128].green=255;seis[128].blue=0;
seis[129].red=255;seis[129].green=255;seis[129].blue=0;
seis[130].red=255;seis[130].green=255;seis[130].blue=0;
seis[131].red=255;seis[131].green=255;seis[131].blue=0;
seis[132].red=255;seis[132].green=255;seis[132].blue=0;
seis[133].red=255;seis[133].green=255;seis[133].blue=0;
seis[134].red=255;seis[134].green=255;seis[134].blue=0;
seis[135].red=255;seis[135].green=255;seis[135].blue=0;
seis[136].red=255;seis[136].green=255;seis[136].blue=0;
seis[137].red=255;seis[137].green=255;seis[137].blue=0;
seis[138].red=255;seis[138].green=255;seis[138].blue=0;
seis[139].red=255;seis[139].green=255;seis[139].blue=0;
seis[140].red=255;seis[140].green=255;seis[140].blue=0;
seis[141].red=255;seis[141].green=255;seis[141].blue=0;
seis[142].red=255;seis[142].green=254;seis[142].blue=0;
seis[143].red=255;seis[143].green=251;seis[143].blue=0;
seis[144].red=255;seis[144].green=248;seis[144].blue=0;
seis[145].red=255;seis[145].green=245;seis[145].blue=0;
seis[146].red=255;seis[146].green=242;seis[146].blue=0;
seis[147].red=255;seis[147].green=239;seis[147].blue=0;
seis[148].red=255;seis[148].green=236;seis[148].blue=0;
seis[149].red=255;seis[149].green=233;seis[149].blue=0;
seis[150].red=255;seis[150].green=230;seis[150].blue=0;
seis[151].red=255;seis[151].green=227;seis[151].blue=0;
seis[152].red=255;seis[152].green=224;seis[152].blue=0;
seis[153].red=255;seis[153].green=221;seis[153].blue=0;
seis[154].red=255;seis[154].green=218;seis[154].blue=0;
seis[155].red=255;seis[155].green=215;seis[155].blue=0;
seis[156].red=255;seis[156].green=212;seis[156].blue=0;
seis[157].red=255;seis[157].green=209;seis[157].blue=0;
seis[158].red=255;seis[158].green=206;seis[158].blue=0;
seis[159].red=255;seis[159].green=203;seis[159].blue=0;
seis[160].red=255;seis[160].green=200;seis[160].blue=0;
seis[161].red=255;seis[161].green=197;seis[161].blue=0;
seis[162].red=255;seis[162].green=194;seis[162].blue=0;
seis[163].red=255;seis[163].green=191;seis[163].blue=0;
seis[164].red=255;seis[164].green=188;seis[164].blue=0;
seis[165].red=255;seis[165].green=185;seis[165].blue=0;
seis[166].red=255;seis[166].green=182;seis[166].blue=0;
seis[167].red=255;seis[167].green=179;seis[167].blue=0;
seis[168].red=255;seis[168].green=176;seis[168].blue=0;
seis[169].red=255;seis[169].green=173;seis[169].blue=0;
seis[170].red=255;seis[170].green=171;seis[170].blue=0;
seis[171].red=255;seis[171].green=168;seis[171].blue=0;
seis[172].red=255;seis[172].green=165;seis[172].blue=0;
seis[173].red=255;seis[173].green=162;seis[173].blue=0;
seis[174].red=255;seis[174].green=159;seis[174].blue=0;
seis[175].red=255;seis[175].green=156;seis[175].blue=0;
seis[176].red=255;seis[176].green=153;seis[176].blue=0;
seis[177].red=255;seis[177].green=150;seis[177].blue=0;
seis[178].red=255;seis[178].green=147;seis[178].blue=0;
seis[179].red=255;seis[179].green=144;seis[179].blue=0;
seis[180].red=255;seis[180].green=141;seis[180].blue=0;
seis[181].red=255;seis[181].green=138;seis[181].blue=0;
seis[182].red=255;seis[182].green=135;seis[182].blue=0;
seis[183].red=255;seis[183].green=132;seis[183].blue=0;
seis[184].red=255;seis[184].green=129;seis[184].blue=0;
seis[185].red=255;seis[185].green=126;seis[185].blue=0;
seis[186].red=255;seis[186].green=123;seis[186].blue=0;
seis[187].red=255;seis[187].green=120;seis[187].blue=0;
seis[188].red=255;seis[188].green=117;seis[188].blue=0;
seis[189].red=255;seis[189].green=114;seis[189].blue=0;
seis[190].red=255;seis[190].green=111;seis[190].blue=0;
seis[191].red=255;seis[191].green=108;seis[191].blue=0;
seis[192].red=255;seis[192].green=105;seis[192].blue=0;
seis[193].red=255;seis[193].green=102;seis[193].blue=0;
seis[194].red=255;seis[194].green=99;seis[194].blue=0;
seis[195].red=255;seis[195].green=96;seis[195].blue=0;
seis[196].red=255;seis[196].green=93;seis[196].blue=0;
seis[197].red=255;seis[197].green=90;seis[197].blue=0;
seis[198].red=255;seis[198].green=87;seis[198].blue=0;
seis[199].red=255;seis[199].green=84;seis[199].blue=0;
seis[200].red=255;seis[200].green=81;seis[200].blue=0;
seis[201].red=255;seis[201].green=78;seis[201].blue=0;
seis[202].red=255;seis[202].green=75;seis[202].blue=0;
seis[203].red=255;seis[203].green=72;seis[203].blue=0;
seis[204].red=255;seis[204].green=69;seis[204].blue=0;
seis[205].red=255;seis[205].green=66;seis[205].blue=0;
seis[206].red=255;seis[206].green=63;seis[206].blue=0;
seis[207].red=255;seis[207].green=60;seis[207].blue=0;
seis[208].red=255;seis[208].green=57;seis[208].blue=0;
seis[209].red=255;seis[209].green=54;seis[209].blue=0;
seis[210].red=255;seis[210].green=51;seis[210].blue=0;
seis[211].red=255;seis[211].green=48;seis[211].blue=0;
seis[212].red=255;seis[212].green=45;seis[212].blue=0;
seis[213].red=255;seis[213].green=42;seis[213].blue=0;
seis[214].red=255;seis[214].green=39;seis[214].blue=0;
seis[215].red=255;seis[215].green=36;seis[215].blue=0;
seis[216].red=255;seis[216].green=33;seis[216].blue=0;
seis[217].red=255;seis[217].green=30;seis[217].blue=0;
seis[218].red=255;seis[218].green=27;seis[218].blue=0;
seis[219].red=255;seis[219].green=24;seis[219].blue=0;
seis[220].red=255;seis[220].green=21;seis[220].blue=0;
seis[221].red=255;seis[221].green=18;seis[221].blue=0;
seis[222].red=255;seis[222].green=15;seis[222].blue=0;
seis[223].red=255;seis[223].green=12;seis[223].blue=0;
seis[224].red=255;seis[224].green=9;seis[224].blue=0;
seis[225].red=255;seis[225].green=6;seis[225].blue=0;
seis[226].red=255;seis[226].green=3;seis[226].blue=0;
seis[227].red=255;seis[227].green=0;seis[227].blue=0;
seis[228].red=252;seis[228].green=0;seis[228].blue=0;
seis[229].red=249;seis[229].green=0;seis[229].blue=0;
seis[230].red=246;seis[230].green=0;seis[230].blue=0;
seis[231].red=243;seis[231].green=0;seis[231].blue=0;
seis[232].red=240;seis[232].green=0;seis[232].blue=0;
seis[233].red=237;seis[233].green=0;seis[233].blue=0;
seis[234].red=234;seis[234].green=0;seis[234].blue=0;
seis[235].red=231;seis[235].green=0;seis[235].blue=0;
seis[236].red=228;seis[236].green=0;seis[236].blue=0;
seis[237].red=225;seis[237].green=0;seis[237].blue=0;
seis[238].red=222;seis[238].green=0;seis[238].blue=0;
seis[239].red=219;seis[239].green=0;seis[239].blue=0;
seis[240].red=216;seis[240].green=0;seis[240].blue=0;
seis[241].red=213;seis[241].green=0;seis[241].blue=0;
seis[242].red=210;seis[242].green=0;seis[242].blue=0;
seis[243].red=207;seis[243].green=0;seis[243].blue=0;
seis[244].red=204;seis[244].green=0;seis[244].blue=0;
seis[245].red=201;seis[245].green=0;seis[245].blue=0;
seis[246].red=198;seis[246].green=0;seis[246].blue=0;
seis[247].red=195;seis[247].green=0;seis[247].blue=0;
seis[248].red=192;seis[248].green=0;seis[248].blue=0;
seis[249].red=189;seis[249].green=0;seis[249].blue=0;
seis[250].red=186;seis[250].green=0;seis[250].blue=0;
seis[251].red=183;seis[251].green=0;seis[251].blue=0;
seis[252].red=180;seis[252].green=0;seis[252].blue=0;
seis[253].red=177;seis[253].green=0;seis[253].blue=0;
seis[254].red=174;seis[254].green=0;seis[254].blue=0;
seis[255].red=171;seis[255].green=0;seis[255].blue=0;
// find pixels for rainbow colours
    for(i=0;i<255;i++) {
      color[i].flags=DoRed|DoGreen|DoBlue;
      if (colorchoice == 1 ) {
        color[i].red = rainbow[i].red*256;
        color[i].green = rainbow[i].green*256;
        color[i].blue = rainbow[i].blue*256;
      } else if (colorchoice == 2 ) {
        color[i].red = polar[i].red*256;
        color[i].green = polar[i].green*256;
        color[i].blue = polar[i].blue*256;
      } else if (colorchoice == 3 ) {
        color[i].red = seis[i].red*256;
        color[i].green = seis[i].green*256;
        color[i].blue = seis[i].blue*256;
      }
      color[i].flags = DoRed|DoGreen|DoBlue;
      XAllocColor(mydisplay, cmap, &color[i]);
    }
    printf ("... Done\n");
  } else {
    printf ("Setting colors using XAllocNamedColor ...\n");
    for(i=0;i<255;i++) {
      if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), rgbhex[i],
                          &(color[i]),&dummyCol))
      {
        printf ("--colour-- ERROR: Unable to set colours\n");
        exit(-1);
      }
    }
    printf ("... Done \n");
  } 
  colorset = 1;
}

/*             dummies for dislin */

void xopen_window_(id)
int  *id;
{
}

void xclose_window_(id)
int  *id;
{
}

void xwindow_title_(text)
char *text;
{
}
void xget_screen_size_(nxres,nyres) 
int *nxres;
int *nyres;
{
}

void xwindow_size_(size,ixpos,iypos) 
int *ixpos;
int *iypos;
float *size;
{
}


/*
void SetForegroundColor(int red, int green, int blue)
{
   XColor color;

   color.red = red*256;
   color.green = green*256;
   color.blue = blue*256;
   printf("rgb %d %d %d \n",color.red,color.green,color.blue);
   color.flags = DoRed|DoGreen|DoBlue;
   XAllocColor(mydisplay, cmap, &color);
   XSetForeground(mydisplay, mygc, color.pixel);
}
*/


