/*   get screen size on pc, in pixels */

#include <stdio.h>
#include <windows.h>

void xget_screen_size_pc_(nxres,nyres) 
int *nxres;
int *nyres;
{

/*  int SM_CXSCREEN;
  int SM_CYSCREEN; */
	
  *nxres = GetSystemMetrics (SM_CXSCREEN);
  *nyres = GetSystemMetrics (SM_CYSCREEN);
/*   printf("c %d %d \n",*nxres,*nyres); */
}
