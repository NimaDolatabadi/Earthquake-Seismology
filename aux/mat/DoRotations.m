function DoRotations(angle)
%DOROTATIONS
%    DoRotations(angle)
%    Rotate horizontal traces by angle
%    Do both backup traces, and displayed traces
%    Update component information shown on seismogram plot.
%    Update header values.

global DATAREAD                 % if true, then seismograms have been read into work space.
if exist('DATAREAD','var') == 0 | isempty(DATAREAD) | DATAREAD == 0
  error('Data has not been read into the Matlab Workspace')
end


global RotationCorrection       % Subtract this angle from all computed azimuths
global ynback                   % un-modified copy of the north trace
global yeback                   % un-modified copy of the east trace
global hNORTHTRACE              % handle to North trace
global hEASTTRACE               % handle to East trace
global hVERTTRACE               % handle to Vert trace
global hMAGNIFY                 % handle to magnify slider
global hTRACES                  % handle to the trace plot on main screen
global ORDER                    % filter order
global LCORNER                  % filter low corner
global HCORNER                  % filter high corner
global dtn                      % delta T for North component
global yn                       % data for North component
global ye                       % data for East component
global EVDATA			% string data describing seismograms
global THeaderData		% Array of header data passed in from SAC
global Axis1Angle		% Angle of first horizontal axis
global Axis2Angle		% Angle of second horizontal axis


% First rotate the raw (backup) traces...
th=-angle*pi/180;
cs=cos(th);
sn=sin(th);
R=[cs -sn;sn cs];
Result=R*[yeback';ynback'];
yeback=Result(1,:);
ynback=Result(2,:);
yeback=yeback';
ynback=ynback';


% Get the current magnification for later use in redrawing seismograms.
value=get(hMAGNIFY,'value');
Mag=exp(value);      

% Get the axis limits and associated trace indices
limits=get(hTRACES,'xlim');
tmin=limits(1);
tmax=limits(2);


% Now update the component orientation data and strings
Axis1Angle = Axis1Angle - angle;
Axis1Angle = setDomain(Axis1Angle );
EVDATA(4,21:25)='     ';
IDstring=[EVDATA(4,1:20) num2str(round(Axis1Angle))];
EVDATA(4,1:length(IDstring))=IDstring;


Axis2Angle = Axis2Angle - angle;
Axis2Angle = setDomain(Axis2Angle );
EVDATA(6,21:25)='     ';
IDstring=[EVDATA(6,1:20) num2str(round(Axis2Angle))];
EVDATA(6,1:length(IDstring))=IDstring;


% turn off display of objects in trace window
for j=1:6		% text
   tag=num2str(j);
   h=findobj(gcf,'Tag',tag);
   set(h,'visible','off')
   if j == 4
      set(h,'string',EVDATA(4,:))
   end
   if j == 6
      set(h,'string',EVDATA(6,:))
   end
end
hPickTxt=findobj(gcf,'Tag','PickText');
hPickLine=findobj(gcf,'Tag','PickLine');

for j=1:length(hPickTxt)
   set(hPickTxt(j),'visible','off')
end
for j=1:length(hPickLine)
   set(hPickLine(j),'visible','off')
end

hPPMpatch=findobj(gcf,'Tag','PPMpatch');
hPPMhandle=findobj(gcf,'Tag','PPMhandle');
set(hPPMpatch,'visible','off')
set(hPPMhandle,'visible','off')

Tdata=get(hNORTHTRACE,'xdata');
[Y,Imin]=min(abs(Tdata-tmin));
[Y,Imax]=min(abs(Tdata-tmax));


% Now apply the current filter and display
dt=dtn;
[b,a]=butter(ORDER,[LCORNER*dt*2 HCORNER*dt*2]);

yn=filtfilt(b,a,ynback);
Ymax=max(yn(Imin:Imax));
Ymin=min(yn(Imin:Imax));
ScaleFactor=2/(Ymax-Ymin);
set(hNORTHTRACE,'ydata',(yn-mean(yn))*ScaleFactor*Mag+4);

ye=filtfilt(b,a,yeback);
Ymax=max(ye(Imin:Imax));
Ymin=min(ye(Imin:Imax));
ScaleFactor=2/(Ymax-Ymin);
set(hEASTTRACE,'ydata',(ye-mean(ye))*ScaleFactor*Mag+2);


% Make everything visible again.

for j=1:length(hPickTxt)
   set(hPickTxt(j),'visible','on')
end
for j=1:length(hPickLine)
   set(hPickLine(j),'visible','on')
end

% Turn the seismogram info text on   
   for j=1:6
      tag=num2str(j);
      h=findobj(gcf,'Tag',tag);
      set(h,'visible','on')
   end


set(hPPMpatch,'visible','on')
set(hPPMhandle,'visible','on')


drawnow
