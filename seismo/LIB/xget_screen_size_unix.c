#include <X11/Xutil.h>
Display *mydisplay;
Window mywindow;
Cursor mycursor;
int myscreen;

xget_screen_size_unix_(nxpix,nypix)
  int  *nxpix, *nypix;
{
  mydisplay = XOpenDisplay ("");
  myscreen  = DefaultScreen (mydisplay);

/* --------- GET ROOT WINDOW SIZE ------vvvv--------- */

  *nxpix = DisplayWidth(mydisplay,myscreen);
  *nypix = DisplayHeight(mydisplay,myscreen);
}
~

