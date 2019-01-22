c   computer dependent subroutines including dummy routines to be
c  able to use same programs on pc, linux  and sun
c-----------------------------------------------------------------
c  5 11 98    version 7.0 check
c  10 5 10 jh: add dummy fstat
c  27 12 10 jh: remove iasp routine, now in general
c  30 12 10 jh: add new dummies
c  23 02 11 jh: add dummy xget_screen_size_pc
c  21 04 11 jh: add dummy polar_plot_dis
c   6  2 12 pv: add selwin
c----------------------------------------------------------------
c
c    ***** this file is for sun ******
c
c
c  Updates:
c

      subroutine computer_type(sun,pc,linux)
c   which computer
      implicit none
      logical pc,sun,linux
      pc=.false.
      sun=.true.
      linux=.false.
      return
      end

c  size of file, return 0
c
      subroutine fstat(unit,file_status,i)
      implicit none
      integer unit,i
      integer file_status(13)
      file_status(8)=0
      return
      end

c
c   dummy for sun 
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
 
