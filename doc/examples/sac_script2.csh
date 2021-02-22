#!/bin/csh

foreach file ( *SAC )
   sac <<EOF
     echo on
     read $file
     rmean 
     rtrend
     lp co 1.0 p 2 n 4
     write ${file}.filtered
     quit
EOF
end
