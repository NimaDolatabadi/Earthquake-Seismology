function SaveResults2(azimuth,incidence,Polariz)
% This function is called to write BAZ information, polarization,
% and incidence angle information into the 3 current SAC file headers.


global CurrentSet		% Index of current 3-comp set being analyzed
global THeaderData		% Array of header data passed in from SAC


% First do the Z-picks
index=CurrentSet*3 ;
THeaderData(5,index)=azimuth;
THeaderData(6,index)=incidence;
THeaderData(25,index)=Polariz;



% Now do the E-picks
index=CurrentSet*3 - 2;
THeaderData(5,index)=azimuth;
THeaderData(6,index)=incidence;
THeaderData(25,index)=Polariz;



% Now do the N-picks
index=CurrentSet*3 - 1;
THeaderData(5,index)=azimuth;
THeaderData(6,index)=incidence;
THeaderData(25,index)=Polariz;
