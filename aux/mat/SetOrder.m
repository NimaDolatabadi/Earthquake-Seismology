function SetOrder
% This si the call back function for the filter order edit control on the
% filter design GUI. It validates the new filter order and updates the
% transfer function plot.


global ORDER			% filter order
global LCORNER			% filter low corner
global HCORNER			% filter high corner
global hFiltPlot		% handle to the line showing filter response
global hEDITORDER		% handle to edit control for filter order
global dtn			% delta T for North component


newOrder=str2num(get(hEDITORDER,'string'));
if newOrder < 2
   ORDER=2;
   set(hEDITORDER,'string','2');
else
   ORDER=round(newOrder);
end

[b,a]=butter(ORDER,[LCORNER*dtn*2 HCORNER*dtn*2]);
N      = 5000;  % number of points in filter response
F      = 1 / dtn;
[H, W] = freqz(b, a, N, F);

set(hFiltPlot, 'Xdata', W, 'ydata',abs(H))
