#!/usr/bin/env perl

open(SAC, "| sac ") or die "Error opening sac";
print SAC "echo on\n";
foreach $file ( glob("*.SAC") ) {
    print SAC qq[
       read $file
       rmean
       rtrend
       lp co 1.0 p 2 n 4
       write ${file}.filtered
     ];
} 
print SAC "quit\n";
close(SAC);
