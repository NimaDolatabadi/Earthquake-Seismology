function SetFilter
% This function sets up the GUI for adjusting filter parameters. This GUI
% displays the (magnitude) transfer function of the current filter and edit fields
% for adjusting corner frequencies and filter order. It also displays the current
% corner frequencies on the transfer function plot as lines. The lines are
% active components in the GUI. They can be dragged to adjust corner frequencies 
% up or down.
global dtn			% delta T for North component
global TRUE
global FALSE

global hFIG			% array of handles to figures
global ORDER			% filter order
global LCORNER			% filter low corner
global HCORNER			% filter high corner
global hFiltPlot		% handle to the line showing filter response
global hEDITORDER		% handle to edit control for filter order
global hEDITLCORNER		% handle to low-corner edit control
global hEDITHCORNER		% handle to high-corner edit control
global hLCORNER			% handle to low corner line
global hHCORNER			% handle to high corner line
global DATAREAD			% if true, then seismograms have been read into work space.


if ~DATAREAD,return,end

makeFigure(5,[500 200 555 500],'Adjust Filter Parameters');
h5=hFIG(5);
set(h5,'Resize','off')
set(h5,'windowbuttondownfcn','MainButtonPress','interruptible','on')
set(h5,'windowButtonMotionFcn','MainMouseMove')
set(h5,'windowButtonUpFcn','MainButtonRelease')


% Get coefficients for current filter and calculate transfer function.
[b,a]  = butter(ORDER,[LCORNER*dtn*2 HCORNER*dtn*2]);
N      = 5000;  % number of points in filter response
F      = 1 / dtn;
[H, W] = freqz(b, a, N, F);

% plot transfer function and corner frequency lines.
hFiltPlot=plot(W,abs(H),'k');
set(hFiltPlot,'erasemode','background')
set(gca,'xcolor','k','ycolor','k')
set(gca,'xscale','log')
set(gca,'xgrid','on')
set(gca,'ygrid','on')
set(gca,'position',[.1 .1 .7 .8])
xlimits=get(gca,'xlim');
xlimits(1)=xlimits(1)/10;
xlimits(2)=xlimits(2)*10;
set(gca,'xlim',xlimits)
set(gca,'ylim',[0 1.2])
xlabel('Frequency (Hz)')
hx=get(gca,'xlabel');
set(hx,'color','k','fontsize',10)
hLCORNER=line([LCORNER LCORNER],[0 1.2],'color','b');
set(hLCORNER,'LineWidth',3,'erasemode','xor')
hHCORNER=line([HCORNER HCORNER],[0 1.2],'color','r');
set(hHCORNER,'LineWidth',3,'erasemode','xor')



% Set up the edit controls...
hFrame=uicontrol('style','frame','position',[450,50,100,400]);
uicontrol('style','text','position',[460 400 80 30],'string','Low Corner');
hEDITLCORNER=uicontrol('style','edit','position',[460 380 80 20],'string',num2str(LCORNER));
set( hEDITLCORNER,'BackgroundColor','w','callback','SetLowCorner')

uicontrol('style','text','position',[460 340 80 30],'string','High Corner');
hEDITHCORNER=uicontrol('style','edit','position',[460 320 80 20],'string',num2str(HCORNER));
set( hEDITHCORNER,'BackgroundColor','w','callback','SetHighCorner')

uicontrol('style','text','position',[460 280 80 30],'string','Order');
hEDITORDER=uicontrol('style','edit','position',[460 260 80 20],'string',num2str(ORDER));
set( hEDITORDER,'BackgroundColor','w','callback','SetOrder')

btnCLOSEFILT=uicontrol('style','pushbutton','position',...
             [470 60 60 30],'string','Close','callback','hclose(5)');

