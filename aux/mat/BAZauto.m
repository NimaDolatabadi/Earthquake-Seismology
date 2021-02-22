% Function to automatically calculate BAZ, incidence and degree of polarization for
% 3-component sets loaded from SAC.

global NumSets			% number of current 3-comp sets

NumSets = SacCheck();
if NumSets < 1
  quitP
  error('Number of Data Sets are less than 1')
end

PPMWINLENGTH = winLength;

global CurrentSet		% Index of current 3-comp set being analyzed
for CurrentSet = 1:NumSets
   GetFiles2;
   PPMWINSTART = getReferenceTime(CurrentSet,reference);
   [azimuth,incidence,Polariz] = getWinPolariz(PPMWINSTART,PPMWINLENGTH);
   disp([azimuth,incidence,Polariz])
   SaveResults2(azimuth,incidence,Polariz);
end   
