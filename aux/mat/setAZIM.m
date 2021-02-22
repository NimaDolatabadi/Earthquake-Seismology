function setAZIM
% This function is called during rotation of the 3-D particle motion plot.
% it gets the current azimuth component of the viewing angle from the
% text control, and sets the view angle using that value.


global hAZIM			% handle to azimuth text
global ppmAxis			% Handle to PPM axis

VIEW=get(ppmAxis,'view');
VIEW(1)=str2num(get(hAZIM,'string'));
set(ppmAxis,'view',VIEW)