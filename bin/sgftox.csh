#!/bin/csh -f
#
# Purpose: To display a .sgf file on the screen
# This script may have one of several names: sgftox.csh, sgf2x.csh, sgf2x
# Syntax sgf2x ABCD, where ABCD.sgf is the file to be displayed
# sgf2x has the functionality of the old sac program sgfplot.
# Calls sgftoeps.csh -- except puts ABCD.eps file in /tmp so do not need
#   to own current directory.  Erases all files created.
#
if ($#argv == 0) then
  echo "This file has one of several names:  sgftox.csh, sgf2x.csh, sgf2x"
  echo "Syntax is sgf2x ABCD"
  echo "where ABCD.sgf is the input sac graphics file."
  echo "Output has a tight Boundingbox.  Requires and uses gs."
  echo "Assumes sgftops is in path."
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

sgftops ${source}.sgf /tmp/${source}.ps 2.0 s << END > /dev/null
0.5
0.5
0
$scalefact
END

if (! -f /tmp/${source}.ps) exit

set ops = '-sDEVICE=bbox -dSAFER -dBATCH -dNOPAUSE'
set newbbox = `gs ${ops} /tmp/${source}.ps |& grep '^%%BoundingBox: '`
cat /tmp/${source}.ps | sed -e "s/^%%BoundingBox:.*/${newbbox}/" \
  -e "s/^%%Creator: /%%Creator: sgftoeps.csh + /" > /tmp/${source}.eps
\rm /tmp/${source}.ps
gs -sDEVICE=x11 -dBATCH -q /tmp/$source.eps
\rm /tmp/$source.eps
exit

