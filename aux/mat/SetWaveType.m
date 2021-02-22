function SetWaveType
% The ML calculations can model P-wave arrivals or SH arrivals. The choice
% is set using a list-control, and this is the callback function for that control.


global hFIG                     % array of handles to figures
global WAVETYPE                 % index to wavetype to model

h=findobj(hFIG(4),'Tag','WaveType');
WAVETYPE=get(h,'value');
