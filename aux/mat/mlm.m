function mlm
% This function sets up the GUI for the ML calculation. The to portion of the screen
% is a frame with a number of edit controls for setting parameters for the ML
% calculation. The frame also includes list controls for the colormap, interpolation,
% and wavetype, and buttons for starting the calculation, printing the results,
% and quitting the ML GUI.


global hFIG			% array of handles to figures
global TRUE
global FALSE

global t			% time for all 3 components

global yn			% data for North component
global ye			% data for East component
global yz			% data for Vert component
global PPMWINLENGTH		% length of window for Particle motion plots
global PPMWINSTART		% start of window for Particle motion plots

global X_MLM			% array of x values for MLM analysis
global Y_MLM			% array of y values for MLM analysis
global Z_MLM			% array of z values for MLM analysis
global T_MLM			% array of t values for MLM analysis
global DATAREAD			% if true, then seismograms have been read into work space.
global WAVETYPE			% index to wavetype to model


if ~DATAREAD,return,end
dt=t(2)-t(1);

% strings for list-controls...
colormap= ...
  ['hsv   '     %- Hue-saturation-value color map.
   'gray  '     %- Linear gray-scale color map.
   'hot   '     %- Black-red-yellow-white color map.
   'cool  '     %- Shades of cyan and magenta color map.
   'bone  '     %- Gray-scale with a tinge of blue color map.
   'copper'     %- Linear copper-tone color map.
   'pink  '     %- Pastel shades of pink color map.
   'prism '     %- Prism color map.
   'jet   '     %- A variant of HSV.
   'flag  '];   %- Alternating red, white, blue, and black color map.

shading= ...
   ['flat  '
    'interp'];

WaveType= ...
   ['P   '
    'SH  '];
WAVETYPE=1;	% default is P-wave    


    
% Set up viewing area and UI controls
makeFigure(4,[400 200 555 700],'Polarization Results');
h4=hFIG(4);
set(h4,'Resize','off')

set(h4,'DefaultAxesFontSize',8)

[Y,startIndex]=min(abs(t-PPMWINSTART));
[Y,endIndex]=min(abs(t-PPMWINSTART-PPMWINLENGTH));
X_MLM=ye(startIndex:endIndex);
X_MLM=X_MLM-mean(X_MLM);
Y_MLM=yn(startIndex:endIndex);
Y_MLM=Y_MLM-mean(Y_MLM);
Z_MLM=yz(startIndex:endIndex);
Z_MLM=Z_MLM-mean(Z_MLM);
T_MLM=t(startIndex:endIndex);



FrmClr=[.3 .3 .5];
EditClr=[1 1 1];
frame=uicontrol('style','frame','position',[10 550 535 140]);
set(frame,'BackgroundColor',FrmClr)

% Make the default window length 1.5 * period of dominant wave in analysis window...
WinSecs=getWinLength(X_MLM,Y_MLM,Z_MLM,T_MLM);
h=uicontrol('style','text','position',[20 665 100 20],'string','Win-Len (s)');
set(h,'ForegroundColor','y','BackGroundColor',FrmClr)
h=uicontrol('style','edit','position',[20 640 100 20],'string',num2str(WinSecs));
set(h,'BackGroundColor',EditClr,'Tag','WinLen')

% Make default time step give 100 columns in AZ-Time matrix unless there are
% fewer than that many points in window, in which case you get the number of points in window.

PntsInWin = endIndex-startIndex+1;
if PntsInWin < 100
   TimeStep=dt;
else
   TimeStep=(t(endIndex)-t(startIndex))/100;
end

h=uicontrol('style','text','position',[140 665 100 20],'string','Time Step (s)');
set(h,'ForegroundColor','y','BackGroundColor',FrmClr)
h=uicontrol('style','edit','position',[140 640 100 20],'string',num2str(TimeStep));
set(h,'BackGroundColor',EditClr,'Tag','TimeStep')

h=uicontrol('style','text','position',[260 665 100 20],'string','Azimuth Step');
set(h,'ForegroundColor','y','BackGroundColor',FrmClr)
h=uicontrol('style','edit','position',[260 640 100 20],'string','4');
set(h,'BackGroundColor',EditClr,'Tag','AzimStep')

h=uicontrol('style','text','position',[20 600 100 20],'string','Incidence Start');
set(h,'ForegroundColor','y','BackGroundColor',FrmClr)
h=uicontrol('style','edit','position',[20 575 100 20],'string','10');
set(h,'BackGroundColor',EditClr,'Tag','IncidStrt')

h=uicontrol('style','text','position',[140 600 100 20],'string','Incidence Step');
set(h,'ForegroundColor','y','BackGroundColor',FrmClr)
h=uicontrol('style','edit','position',[140 575 100 20],'string','4');
set(h,'BackGroundColor',EditClr,'Tag','IncidStep')

h=uicontrol('style','text','position',[260 600 100 20],'string','Incidence Stop');
set(h,'ForegroundColor','y','BackGroundColor',FrmClr)
h=uicontrol('style','edit','position',[260 575 100 20],'string','50');
set(h,'BackGroundColor',EditClr,'Tag','IncidStop')


% Set up list controls and command buttons...
uicontrol('style','popupmenu','position',[385 645 70 25],'string',colormap,'Tag',...
          'ColorMap','value',3,'callback','MLMcallback(''colormap'')');

uicontrol('style','popupmenu','position',[385 605 70 25],'string',shading,'Tag',...
          'Shading','callback','MLMcallback(''shading'')');

uicontrol('style','popupmenu','position',[385 565 70 25],'string',WaveType,'Tag',...
          'WaveType','callback','SetWaveType');

uicontrol('style','pushbutton','position',[465 645 70 25],'string','Calculate',...
          'callback','CalcMLM');

uicontrol('style','pushbutton','position',[465 605 70 25],'string','Print',...
          'callback','MLMcallback(''print'')');

uicontrol('style','pushbutton','position',[465 565 70 25],'string','Close',...
          'callback','hclose(4)');





function period=getWinLength(x,y,z,t)
% the ML calculation should be done using windows that are short enough
% to give good time resolution and minimize the effects of multiple arrivals
% while being long enough to provide a good sample. This routine calculates
% the power spectrum of the data, finds the frequency with the maximum power,
% and returns the corresponding period multiplied by 1.5 as a default analysis 
% window length.

p=spectrum([x;y;z]);
dt=t(2)-t(1);
[Y,I]=max(p);
fmax=1/2/dt;
period=1.5*length(p)/I(1)/fmax(1);




