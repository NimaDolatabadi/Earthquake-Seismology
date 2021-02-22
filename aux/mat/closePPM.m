function closePPM
% This function is called to close the particle motion plot window.
% The significance of the PPM3ACTIVE variable is that when true
% operations such as resize, move and filter send a signal to update
% the window.

global TRUE
global FALSE

global PPM3ACTIVE		% true if ppm3 window is open

PPM3ACTIVE=FALSE;
hclose(2)