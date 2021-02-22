function ButtonRelease
% This function is called when a mouse button is released anywhere in Figure 1
% It affects picking and zooming operations.

global TRUE
global FALSE

global ANCHOR			% True if button is down and object selected
global hCROSS			% handle to cross marking selected point on PPM path
global MAKECROSS		% True if button press was on seismogram


if MAKECROSS
   delete(hCROSS);
end
ANCHOR=FALSE;
