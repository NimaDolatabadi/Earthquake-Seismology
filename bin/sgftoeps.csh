#!/bin/csh -f

# Start with a sac-format plot file (.sgf).  Calls sgftops to produce a .ps
#   file rotated from landscape to portrait format and scaled by 0.75
#   (based on the 'normal" .sgf format, a panel 10" wide and 7.5" high).
#   Earlier versions (before June 2006) used gs and epstool to calculate the
#   Boundingbox.  This version uses only gs.
# Call is sgftoeps.csh ABCD  -- input file: ABCD.sgf. No preview created.
#   Assumes write access to current directory.
# Can make this produce a PDF file if one has a program like epstopdf.
#    See comments at end of script

if ($#argv == 0) then
  echo Syntax is sgftoeps.csh ABCD
  echo where ABCD.sgf is the input sac-format graphics file.
  echo Assumes sgftops is in path.
  echo Output is ABCD.eps.  Assumes write access to current directory.
  echo Can be modified to produce a PDF file if program like epstopdf exists
  exit
endif

set scalefact = 0.75

while ($#argv > 0)
  switch ($1)
    case -scale
      set scalefact = $2
      shift
      breaksw
#Earlier version allowed a -scale 0.xx.  Still works, but not advertised.
    default
      set source = $1 #filename of the input SAC sgf file (source.sgf)
      breaksw
  endsw
  shift
end

if ($?source == 0) then
  echo no input file specified
  exit
else if (! -f ${source}.sgf) then
  echo ${source}.sgf not found
  exit
endif

sgftops ${source}.sgf ${source}.ps 1.0 s << END > /dev/null
0.5
0.5
0
$scalefact
END

if (! -f ${source}.ps) exit

set ops = '-sDEVICE=bbox -dSAFER -dBATCH -dNOPAUSE'
set newbbox = `gs ${ops} ${source}.ps |& grep '^%%BoundingBox: '`
cat ${source}.ps | sed -e "s/^%%BoundingBox:.*/${newbbox}/" \
  -e "s/^%%Creator: /%%Creator: sgftoeps.csh + /" > ${source}.eps

\rm ${source}.ps
# To produce a PDF file instead of a EPS file, replace the previous line with
#  epstopdf ${source}.eps
#  \rm ${source}.ps ${source}.eps
exit

