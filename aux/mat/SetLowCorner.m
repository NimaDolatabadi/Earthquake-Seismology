function SetLowCorner
% This is the callback function for the  low-corner edit control on the filter 
% design window. It validates the new frequency, designs the filter, updates
% the transfer function plot, and updates the low corner frequency line.

global ORDER			% filter order
global LCORNER			% filter low corner
global HCORNER			% filter high corner
global hFiltPlot		% handle to the line showing filter response
global dtn			% delta T for North component
global hEDITLCORNER		% handle to low-corner edit control
global hLCORNER			% handle to low corner line
global hHCORNER			% handle to high corner line

newCorner=str2num(get(hEDITLCORNER,'string'));
if newCorner < .02
   newCorner = .02;
elseif newCorner >HCORNER -.1
   newCorner = HCORNER -.1
end
LCORNER=newCorner;
set(hEDITLCORNER,'string',num2str(LCORNER));

set(hLCORNER,'visible','off')
set(hHCORNER,'visible','off')

[b,a]=butter(ORDER,[LCORNER*dtn*2 HCORNER*dtn*2]);

N      = 5000;  % number of points in filter response
F      = 1 / dtn;
[H, W] = freqz(b, a, N, F);



set(hFiltPlot, 'Xdata', W, 'ydata',abs(H))
set(hHCORNER,'visible','on')
set(hLCORNER,'xdata',[LCORNER LCORNER])
set(hLCORNER,'visible','on')
