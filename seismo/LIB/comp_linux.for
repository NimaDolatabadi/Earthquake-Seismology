c  computer dependent subroutines including dummy routines to be
c  able to use same programs on pc and sun
c
c    ***** this file is for linux ******
c
c
c  Updates:
c
c--------------------------------------------------------------------
c  nov5 by     jh       version 7.0 check
c  dec 27 2010 jh: gfortran pc
c  feb 21 2011 jh: add dummy xget_window_size_pc
c  apr 21 2011 jh: add dummy polar_plot_dis
c

      subroutine computer_type(sun,pc,linux)
c   which computer
      implicit none
      logical pc,sun,linux
      pc=.false.
      sun=.false.
      linux=.true.
      return
      end

cc      integer function JIAND(i1,i2)
cc      implicit none
cc      integer i1,i2
cc      jiand=iand(i1,i2)
cc      return
cc      end 

cc      subroutine iasp91_filename(name)
c set name of iasp91 files
cc      character*(*) name
cc      name='IASP91_linux'
cc      return
cc      end

c
c   dummy for linux
c
        subroutine initt
        return
        end
  
        subroutine selwin(i)
        integer i
        return
        end

        subroutine swgwth(i)
        integer i
        return
        end

        subroutine dwgtxt(text1,text2)
        character*80, text1,text2
        return
        end

        subroutine xget_screen_size_pc(nx,ny)
c
c   only needed if dislin is used with linux
c
        integer nx,ny
        return
        end
c
c
        subroutine polar_plot_dis(n,pnumber,tnumber,azimuth,nbin) 
c
c   makes a polar plot
c
c      n: number of bins
c      pnumber: number of p values in each bin
c      tnumber: ---------t------------------
c      azimuth: azimuth values for each bin
c      nbin: bin size in degrees      

      IMPLICIT NONE
      integer n
      real pnumber(*),tnumber(*),azimuth(*)
      integer nbin

      return
      end
