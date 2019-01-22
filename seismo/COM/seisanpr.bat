@echo off
rem   script to send ps plot to printer in seisan, soem posibilities
rem
rem   next is a network printer with name lpr
rem
rem lpr %1
rem
rem   could be that more spec must be given like
rem
rem lpr -S citrin.geo.uib.no -P sps %1
rem
rem   next is normal printer
rem
rem print %1
rem
rem   next is used in windows which does not have a print command

copy %1 prn

rem   or

copy %1 lpt1

rem
rem   the printer might have to be defined as e.g.
rem
rem net use lpt3: "\\Amadeus\HP LaserJet 4 (ALK)"  
rem
rem   where Amadeus is print server and the rest is the defined name. 
rem   lpt3 is a virtual 
rem   printer port and must not be in use before.
rem   If the printer is only connected locally, then the name
rem   Amadeus must be replace by the local computer name
rem   and the printer must has status as shared
rem
rem   the print command will be
rem
rem copy %1 lpt3

copy %1 lpt1

@echo on
rem
rem   jh apr 2004
