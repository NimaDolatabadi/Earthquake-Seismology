c  some computer dependent subroutines, this is for pc
c  1993
c  24 1 95 by jh  uib
cJAB(BGS)Feb95 : Modify "topdir" to return correct path-root
cJAB(BGS)Aug95 : Add random number generator
c aug 96 by jh : add editor choice, allow e.g. d: for seisan_top
c mar 3 97     : change print plot command to seisanpr when sneding ps
c                plot to printer
c mar 6        : put in get_env_cal
c oct 3, 97    : put and get a general strings routines
c                --------   version 7.0 check ---------------------------
c aug 9  98    : remove doubel delcarations of a variable, 5 char data base
c nov 2        : add linux to routine computer type
c nov 9        : change x dummy calls to not have '_'
c may 3 99 bmt : change systemc subroutine
c may 26   bmt : bug in systemc fixed (odir,o...)
c may 29   jh  : terminate string in systemc
c jun 11       : standard editor to edit
c          lo  : added get_env_psscale
c sep 15   bmt : change systemc subroutine
c oct 2    bmt : add eev_act, get_detailds subroutine
c jan 18   bmt : add put_env_seistop
c sep 18 00 lo : length of text in systemc variable
c may 07 03 lo : add iasp91_filename
c may 10 03 lo : add dummy xtextwin and changed xopen
c jul 14 03 jh : iasp91_win to iasp91_windows
c jul 21       : edit to notepad
c oct 30 08 jh : comment out dflib, not use anymore, gave troubel with
c                seisan.exe
c nov 12 08 jh : put it back in, affect graphics and system calls
c may 10 10 jh : add dummy routine fstat
c dec    10 jh : gfortran, little left 
c**************************************************************************      
c
c

      subroutine computer_type(sun,pc,linux)
c
c   which computer
c
      implicit none
      logical pc,sun,linux
      pc=.true.
      sun=.false.
      linux=.false.
      return
      end

      subroutine xget_screen_size_unix(nx,ny)
c  dummy for pc
      implicit none
      integer nx,ny
      return
      end

