#!/usr/bin/env perl

open(SAC, "| sac ") or die "Error opening sac";
print SAC "fg seismo\n";
print SAC "lh columns 2\n";
print SAC "quit\n";
close(SAC);

