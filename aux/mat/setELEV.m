function setELEV
% This function is called during rotation of the 3-D particle motion plot.
% it gets the current elevation component of the viewing angle from the
% text control, and sets the view angle using that value.

global hELEV			% handle to elevation text
global ppmAxis			% Handle to PPM axis

VIEW=get(ppmAxis,'view');
VIEW(2)=str2num(get(hELEV,'string'));
set(ppmAxis,'view',VIEW)