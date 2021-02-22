function ppm3
% This function is called to set up the particle motion plot GUI screen. It
% creates two viewing areas, one for a display of the Z-component of motion
% within the analysis window, and one for the 3-D particle motion plot. It
% also adds a frame with controls for modifying the viewing angle of the 
% particle motion plot and for quitting the particle motion plot.


global hFIG			% array of handles to figures
global TRUE
global FALSE
global SeisAxis			% Handle to seismogram axis
global ppmAxis			% Handle to PPM axis
global EAST			% Handle to east text
global NORTH			% Handle to North Text
global VERT			% Handle to Z Text
global ANCHOR			% True if button is down and object selected
global hAZIM			% handle to azimuth text
global hELEV			% handle to elevation text
global hSEIS			% handle to seismogram line segments
global hPPM3			% handle to PPM line segments
global MAX			% Max value of the PPM plot
global MIN			% Min value of the PPM plot

global t			% time for all 3 components

global yn			% data for North component
global ye			% data for East component
global yz			% data for Vert component
global PPMWINLENGTH		% length of window for Particle motion plots
global PPMWINSTART		% start of window for Particle motion plots
global PPM3ACTIVE		% true if ppm3 window is open
global DATAREAD			% if true, then seismograms have been read into work space.
global RotationCorrection	% Subtract this angle from all computed azimuths


if ~DATAREAD,return,end



ANCHOR=FALSE;		%callback function for mouse clicks checks this global

% Set up viewing area and UI controls
makeFigure(2,[400 100 555 700],'Particle Motion Plot');
h2=hFIG(2);
set(h2,'Resize','off')
clf
set(h2,'windowbuttondownfcn','DispatchButtonPress','interruptible','on')
set(h2,'windowButtonMotionFcn','DispatchMouseMove')
set(h2,'windowButtonUpFcn','ButtonRelease')
set(h2,'resize','off')
set(h2,'DefaultAxesFontSize',8)

frame=uicontrol('style','frame','position',[435 10 110 140]);
uicontrol('style','text','position',[440 120 100 20],'string','Viewing Angles');
uicontrol('style','text','position',[440 95 100 20],'string','AZIM      ELEV');
hAZIM=uicontrol('style','edit','position',[440 70 40 20],'string','20','callback','setAZIM');
hELEV=uicontrol('style','edit','position',[495 70 40 20],'string','30','callback','setELEV');
uicontrol('style','pushbutton','position',[465 20 50 20],'string','Close',...
          'callback','closePPM');


% get data within analysis window...
[Y,startIndex]=min(abs(t-PPMWINSTART));
[Y,endIndex]=min(abs(t-PPMWINSTART-PPMWINLENGTH));
startIndex=round(startIndex);
endIndex=round(endIndex);
x=ye(startIndex:endIndex);
x=x-mean(x);
y=yn(startIndex:endIndex);
y=y-mean(y);
z=yz(startIndex:endIndex);
z=z-mean(z);
tp=t(startIndex:endIndex);
nsegs=10;


% Correct for seismometer component rotation if > five degrees 
if abs(RotationCorrection) > 5.0
   th=RotationCorrection*pi/180;
   sn=sin(th);
   cs=cos(th);
   Arot=[cs -sn;sn cs];
   Vec=[x';y'];
   Vec=Arot*Vec;
   x=Vec(1,:);
   x=x';
   y=Vec(2,:);
   y=y'; 
end


% Set up the progressive colors for line plots
pntsPerSeg=round(length(tp)/nsegs);
colormap cool
map=colormap;
map(1:4,:)=[];
[rows,cols]=size(map);


% Plot the seismogram in upper half of window
SeisAxis=subplot(2,1,1);

hSEIS=zeros(nsegs-1,1);
for j=1:nsegs-1
   colorrow=j*rows/nsegs;
   color=map(colorrow,:);
   startindex=(j-1)*pntsPerSeg+1;
   endindex=startindex+pntsPerSeg+1;
   h=line(tp(startindex:endindex),z(startindex:endindex));
   hSEIS(j)=h;
   set(h,'color',color)
end
set(SeisAxis,'xcolor','k','ycolor','k','box','on')
title('Z-component of motion')
ht=get(SeisAxis,'title');
set(ht,'color','k');
xlabel('time (s)')
hl=get(SeisAxis,'xlabel');
set(hl,'color','k')


% Do the particle-motion plot in lower half of window
ppmAxis=subplot(2,1,2);

set(ppmAxis,'xcolor','k','ycolor','k','zcolor','k')
title('3-D Particle motion')
ht=get(ppmAxis,'title');
set(ht,'color','k');
MAX=max([max(x), max(y), max(z)]);
MIN=min([min(x), min(y), min(z)]);
Arrowlength=(MAX-MIN)/15;
minwidth=1;
maxwidth=6;
widthIncr=(maxwidth-minwidth)/nsegs;
hPPM3=zeros(nsegs-1,1);
for j=1:nsegs-1
   colorrow=j*rows/nsegs;
   color=map(colorrow,:);
   startindex=(j-1)*pntsPerSeg+1;
   endindex=startindex+pntsPerSeg;
   h=line(x(startindex:endindex),y(startindex:endindex),z(startindex:endindex),'color','k');
   hPPM3(j)=h;
   set(h,'color',color)
   set(h,'linewidth',(j-1)*widthIncr+minwidth)
   
   % Get direction cosines for end of line segment and use to put an oriented arrow on plot.
   x0=x(endindex-1);
   y0=y(endindex-1);
   z0=z(endindex-1);
   dx=x(endindex)-x0;
   dy=y(endindex)-y0;
   dz=z(endindex)-z0;
   orient=[dx;dy;dz];
   length=norm(orient);
   orient=orient/length;
   arrow3d(Arrowlength,x0,y0,z0,orient)
end

% Place E-N-Z axes in plot
limits=[MIN, MAX];
set(gca,'xlim',limits,'ylim',limits,'zlim',limits);
axis('square')
line([0 MAX], [0,0],[0,0],'color','k')
EAST=text(MAX,0,0,'E','color','k');
line([0,0],[0,MAX],[0,0],'color','k')
NORTH=text(0,MAX,0,'N','color','k');
line([0,0],[0,0],[0,MAX],'color','k')
VERT=text(0,0,MAX,'Z','color','k');
set(gca,'box','on')
view(str2num(get(hAZIM,'string')),str2num(get(hELEV,'string')))
PPM3ACTIVE=TRUE;



