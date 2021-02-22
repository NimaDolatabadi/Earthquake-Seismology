function SetHighCorner
% This is the callback function for the  high-corner edit control on the filter 
% design window. It validates the new frequency, designs the filter, updates
% the transfer function plot, and updates the high corner frequency line.


global ORDER			% filter order
global LCORNER			% filter low corner
global HCORNER			% filter high corner
global hFiltPlot		% handle to the line showing filter response
global dtn			% delta T for North component
global hEDITHCORNER		% handle to high-corner edit control
global hLCORNER			% handle to low corner line
global hHCORNER			% handle to high corner line

newCorner=str2num(get(hEDITHCORNER,'string'));
if newCorner < LCORNER+.1
   newCorner = LCORNER+.1;
elseif newCorner >1/2/dtn-.1
   newCorner = 1/2/dtn-.1;
end
HCORNER=newCorner;
set(hEDITHCORNER,'string',num2str(HCORNER));

set(hLCORNER,'visible','off')
set(hHCORNER,'visible','off')

[b,a]=butter(ORDER,[LCORNER*dtn*2 HCORNER*dtn*2]);

N      = 5000;  % number of points in filter response
F      = 1 / dtn;
[H, W] = freqz(b, a, N, F);

set(hFiltPlot, 'Xdata', W, 'ydata',abs(H))
set(hLCORNER,'visible','on')
set(hHCORNER,'xdata',[HCORNER HCORNER])
set(hHCORNER,'visible','on')
