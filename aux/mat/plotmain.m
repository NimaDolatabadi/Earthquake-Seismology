function plotmain
% This function plots the seismograms in the main GUI window. It is called
% anytime new data are read into the workspace.


global TRUE
global FALSE
global t			% time for all 3 components

global yn			% data for North component
global dtn			% delta T for North component
global datan			% header variables for North component
global stringdatan		% string data for North component

global ye			% data for East component
global dte			% delta T for East component
global datae			% header variables for East component
global stringdatae		% string data for East component

global yz			% data for Vert component
global dtz			% delta T for Vert component
global dataz			% header variables for Vert component
global stringdataz		% string data for Vert component
global hNORTHTRACE		% handle to North trace
global hEASTTRACE		% handle to East trace
global hVERTTRACE		% handle to Vert trace

global hCOMPASS			% handle to the compass plot
global hDIP			% handle to the dip cartoon plot
global hTRACES			% handle to the trace plot on main screen
global PPMWINLENGTH		% length of window for Particle motion plots
global PPMWINSTART		% start of window for Particle motion plots
global hPOLARARROW		% handle to arrow showing polarization azimuth
global PLOTMADE			% true if seismograms loaded and plotted.
global hPOLARDIPARROW		% handle to arrow showing dip
global hBAZTEXT			% handle to BAZ text.
global hRESULT			% handle to display panel for BAZ and incidence
global OLDCURSOR		% saved cursor type
global hPICK			% handle to pick button frame
global hDELPICK			% handle to pick delete button
global DATAREAD			% if true, then seismograms have been read into work space.
global EVDATA			% string data describing seismograms
global hMAGNIFY			% handle to magnify slider


DATAREAD=TRUE;


% Set up the 3 plot axis systems...
SetUpCompass


if ~isempty(hDIP)
   subplot(hDIP)
else			
   SetUpDipMeter;
end   



if ~isempty(hTRACES)
   subplot(hTRACES)			
   cla reset
end   


% plot the seismograms...
hTRACES=subplot('position',[.1 .08 .85 .53]);
set(hTRACES,'Tag','TrcePlot');
hNORTHTRACE=line(t,yn/max(yn)+4,'color','b','erasemode','background','Tag','Ntrce');
hEASTTRACE=line(t,ye/max(ye)+2,'color','g','erasemode','background','Tag','Etrce');
hVERTTRACE=line(t,yz/max(yz),'color','k','erasemode','background','Tag','Vtrce');
% add descriptive info from headers...
for j=1:3
   pltref=2*(j-1);
   text(min(t),pltref+.85,EVDATA(2*j,:),'color','k','fontsize',8,'Tag',num2str(2*j));
   text(min(t),pltref+.70,EVDATA(2*(j-1)+1,:),'color','k','fontsize',8,'Tag',num2str(2*(j-1)+1));
end   

% plot the analysis window marker...   
SetUpPPMWin(t,dataz,datan,datae);

% fix up the axes...
set(gca,'xcolor','k','ycolor','k','box','on')
set(gca,'xlim',[min(t),max(t)],'ylim',[-1,5])
InitLimitStack;
yy=get(gca,'YTickLabel');
yy(1,:)='  ';
yy(2,:)='Z '; 
yy(3,:)='  ';
yy(4,:)='1 ';
yy(5,:)='  ';
yy(6,:)='2 ';
yy(7,:)='  ';

set(gca,'YTickLabel',yy)
xlabel('time (sec)')
hLabel=get(gca,'xlabel');
set(hLabel,'color','k','fontsize',10)

% Turn on the BAZ and incidence angle displays
for j=1:15
   set(hRESULT(j),'visible','on')
end
Winpolariz			% get polarization in window
PLOTMADE=TRUE;
OLDCURSOR=get(gcf,'pointer');


% Add picks to plot...
set(hPICK,'visible','on')
set(hDELPICK,'visible','on')
SetPicksFromFile(t,dataz,datan,datae);

handle=findobj(gcf,'type','uimenu','Tag','Fsave');
set(handle,'enable','on')
handle=findobj(gcf,'type','uimenu','Tag','BPfilt');
set(handle,'enable','on')
handle=findobj(gcf,'type','uimenu','Tag','TrcePolariz');
set(handle,'enable','on')

% Add the magnify slider control to plot...
hMAGNIFY=uicontrol('style','slider','position',[530,205,15,70],...
'callback','magnifyTrces','Tag','Magnify','value',0,'min',-2,'max',2);



function SetUpPPMWin(t,dataz,datan,datae)
% This function is called after a new set of seismograms are loaded to
% set up the default analysis window for particle-motion plotting.
% The window is intended to start at the P-wave arrival and be of length
% PPMWINLENGTH. P-wave arrivals are looked for in header variables, A
% and in T0 - T9. Header variable A is stored in position 9 of
% dataz,datan,datae and T0-T9 are in data(zne) 11-20. If no P-wave
% picks are found in any of the three files, then the analysis window
% is put in the middle of the trace.


global PPMWINLENGTH		% length of window for Particle motion plots
global PPMWINSTART		% start of window for Particle motion plots
global hPPMPATCH		% handle to graphical representation of PPM window
global hPPMEXPAND		% handle to expand symbol on PPM window
global stringdatae		% string data for East component
global stringdatan		% string data for North component
global stringdataz		% string data for Vert component

% First see if there are P-wave picks within time window t.


PPMWINSTART=[];
if dataz(9) >= t(1) & dataz(9) <= t(length(t))
   PPMWINSTART=dataz(9);
elseif datan(9) >= t(1) & datan(9) <= t(length(t))
   PPMWINSTART=datan(9);
elseif datae(9) >= t(1) & datae(9) <= t(length(t))
   PPMWINSTART=datae(9);
elseif ~isempty(findstr(stringdataz(49:128),'Pn'))
   index=findstr(stringdataz(49:128),'Pn');
   if fix(index/8)*8 == index
      index=fix(index/8);
   else
      index=fix(index/8)+1;
   end
   value=dataz(10+index);
   if value >= t(1) & value <=t(length(t))
      PPMWINSTART=value;
   end

elseif ~isempty(findstr(stringdataz(49:128),'Pg'))
   index=findstr(stringdataz(49:128),'Pg');
   if fix(index/8)*8 == index
      index=fix(index/8);
   else
      index=fix(index/8)+1;
   end
   value=dataz(10+index);
   if value >= t(1) & value <=t(length(t))
      PPMWINSTART=value;
   end
   
elseif ~isempty(findstr(stringdatan(49:128),'Pn'))
   index=findstr(stringdatan(49:128),'Pn');
   if fix(index/8)*8 == index
      index=fix(index/8);
   else
      index=fix(index/8)+1;
   end
   value=datan(10+index);
   if value >= t(1) & value <=t(length(t))
      PPMWINSTART=value;
   end

elseif ~isempty(findstr(stringdatan(49:128),'Pg'))
   index=findstr(stringdatan(49:128),'Pg');
   if fix(index/8)*8 == index
      index=fix(index/8);
   else
      index=fix(index/8)+1;
   end
   value=datan(10+index);
   if value >= t(1) & value <=t(length(t))
      PPMWINSTART=value;
   end
   
elseif ~isempty(findstr(stringdatae(49:128),'Pn'))
   index=findstr(stringdatae(49:128),'Pn');
   if fix(index/8)*8 == index
      index=fix(index/8);
   else
      index=fix(index/8)+1;
   end
   value=datae(10+index);
   if value >= t(1) & value <=t(length(t))
      PPMWINSTART=value;
   end

elseif ~isempty(findstr(stringdatae(49:128),'Pg'))
   index=findstr(stringdatae(49:128),'Pg');
   if fix(index/8)*8 == index
      index=fix(index/8);
   else
      index=fix(index/8)+1;
   end
   value=datae(10+index);
   if value >= t(1) & value <=t(length(t))
      PPMWINSTART=value;
   end
end

if isempty(PPMWINSTART)
   PPMWINSTART=( t( length(t) ) +t(1) )/2;   
end



xlimits=[PPMWINSTART,PPMWINSTART,PPMWINSTART+PPMWINLENGTH,PPMWINSTART+PPMWINLENGTH];
ylimits=[-1,7,7,-1];
hPPMPATCH=patch(xlimits,ylimits,'m','erasemode','xor','Tag','PPMpatch');
hPPMEXPAND=text(PPMWINSTART+PPMWINLENGTH,3,'<->','Tag','PPMhandle');
set(hPPMEXPAND,'color','k','fontsize',16,'erasemode','xor')






function SetUpDipMeter
% On the main GUI screen, the upper part of the screen contains a frame
% with a display of back azimuth and incidence angle for the current
% analysis window. The incidence angle is shown on a cartoon-like
% dip meter, and this function is called to draw the dip meter.

global hDIP			% handle to the dip cartoon plot

hDIP=subplot('position',[.675 .65 .25 .20]);
set(hDIP,'xcolor','k','ycolor','k','box','on')
set(hDIP,'xtick',[],'ytick',[])
set(hDIP,'xlim',[-1 1],'ylim',[-1 1]);
Diameter=0.8;
Cpoints=mkcircle([0,0,0],Diameter,100);
hold on
plot(Cpoints(1,:),Cpoints(2,:),'k');
hBORDER=patch([-1 -1 1 1],[0 1 1 0],'c');
hHORIZ=line([-1 1], [0 0],'color','k');
set(hHORIZ,'LineWidth',3.0)

r1=.8;

for angle=90:30:270
   th=angle*pi/180;
   if angle == 0 | angle == 90 | angle == 180 | angle == 270
      r2=.6;
   else
      r2=.7;
   end
   x1=r1*sin(th);
   y1=r1*cos(th);
   x2=r2*sin(th);
   y2=r2*cos(th);
   line([x1 x2],[y1 y2],'color','k')
end
Cpoints=mkcircle([0,0,0],.05,20);
patch(Cpoints(1,:),Cpoints(2,:),'r');


line([.1 .1 .4 .4],[0 .3 .3 0],'color','k');
line([.05 .25 .45 ],[.25 .5 .25],'color','k');





function SetUpCompass
% On the main GUI screen, the upper part of the screen contains a frame
% with a display of back azimuth and incidence angle for the current
% analysis window. The back azimuth is shown on a pseudo-compass, and
% this function is called to draw the compass.

global hCOMPASS			% handle to the compass plot
global RotationCorrection       % Subtract this angle from all computed azimuths
global hAXIS_SYMB		% handle to seismometer axes symbol


if isempty(hCOMPASS)
   hCOMPASS=subplot('position',[.08 .65 .25 .20]);
   set(hCOMPASS,'xcolor','k','ycolor','k','box','on')
   set(hCOMPASS,'xtick',[],'ytick',[])
   set(hCOMPASS,'xlim',[-1 1],'ylim',[-1 1]);
   hBORDER=patch([-1 -1 1 1],[-1 1 1 -1],'c');
   Diameter=0.8;
   Cpoints=mkcircle([0,0,0],Diameter,100);
   hCircle=patch(Cpoints(1,:),Cpoints(2,:),'w');
   text(-.05,Diameter+.1,'N','color','k')
   text(Diameter+.05,0,'E','color','k')
   text(-.05,-Diameter-.1,'S','color','k')
   text(-1,0,'W','color','k')
   Cpoints=mkcircle([0,0,0],.05,20);
   patch(Cpoints(1,:),Cpoints(2,:),'r');
   angle=0;
   r1=.8;
   for angle=0:30:360
      th=angle*pi/180;
      if angle == 0 | angle == 90 | angle == 180 | angle == 270
         r2=.6;
      else
         r2=.7;
      end
      x1=r1*sin(th);
      y1=r1*cos(th);
      x2=r2*sin(th);
      y2=r2*cos(th);
      line([x1 x2],[y1 y2],'color','k')
   end

% Plot the seismometer axis symbol...
   hAXIS_SYMB=mkAxisSymb([0,0],0.7,-RotationCorrection,'b');
else
   subplot(hCOMPASS)
   delete(hAXIS_SYMB)
   hAXIS_SYMB=mkAxisSymb([0,0],0.7,-RotationCorrection,'b');
end




function SetPicksFromFile(t,dataz,datan,datae)
% When SAC files are read into the workspace, this function is called to find
% any picks that match the types in its pick list, plot those picks on the seismograms,
% and add the picks to the pick arrays. The pick list is in the string array
% PickTypes, and for each trace, the function cycles through PickTypes looking
% for a match in the appropriate seismogram header. When a match is found,
% the pick is plotted and the appropriate array is updated.


global stringdatae              % string data for East component
global stringdatan              % string data for North component
global stringdataz              % string data for Vert component
global hActivePick              % handle to currently active pick
global NumZpicks                % number of picks on Z channel
global NumEpicks                % number of picks on E channel
global NumNpicks                % number of picks on N channel
global hZpick                   % array of handles to Z pick markers
global hEpick                   % array of handles to E pick markers
global hNpick                   % array of handles to N pick markers
global PickTypes                % array of strings for pick types


[M,N]=size(PickTypes);
%disp([' in SetPicksFromFile string is ' stringdataz])
for j=1:M
% first look for picks on Z trace
   pickTime=FindPickInfo(stringdataz,PickTypes(j,:),t,dataz);
   if ~isempty(pickTime)
      pickPos=[pickTime,0];
      hNewPick=CreatePickMarker2(pickPos,PickTypes(j,:));
      if ~isempty(hActivePick)
         set(hActivePick(2),'color','k')
      end
      hActivePick=hNewPick;
      NumZpicks=NumZpicks+1;
      hZpick(:,NumZpicks)=hNewPick;
   end
   
% Now look for picks on N trace
   pickTime=FindPickInfo(stringdatan,PickTypes(j,:),t,datan);
   if ~isempty(pickTime)
      pickPos=[pickTime,4];
      hNewPick=CreatePickMarker2(pickPos,PickTypes(j,:));
      if ~isempty(hActivePick)
         set(hActivePick(2),'color','k')
      end
      hActivePick=hNewPick;
      NumNpicks=NumNpicks+1;
      hNpick(:,NumNpicks)=hNewPick;
   end

% Finally look for picks on E trace
   pickTime=FindPickInfo(stringdatae,PickTypes(j,:),t,datae);
   if ~isempty(pickTime)
      pickPos=[pickTime,2];
      hNewPick=CreatePickMarker2(pickPos,PickTypes(j,:));
      if ~isempty(hActivePick)
         set(hActivePick(2),'color','k')
      end
      hActivePick=hNewPick;
      NumEpicks=NumEpicks+1;
      hEpick(:,NumEpicks)=hNewPick;
   end

   
end



function InitLimitStack
% In order to allow the user to undo any zooms, every time a zoom operation
% is performed, the old plot limits are pushed onto a stack. This
% routine is used to initialize the stack when data are first loaded.

global StackIndex                       % Index into zoom stack
global Stack                            % Array of xlimits

StackIndex=0;
Stack=[];




function time=FindPickInfo(stringdata,PickTypes,t,data)
% This function looks through string data read from the SAC header
% looking for strings that match any of the contents of the Pick list
% control. If a match is found it returns the associated time.
time=[];

% First check the "A" and "KA" fields.
if data(9) >= t(1) & data(9) <= t(length(t))
   if ~isempty(findstr(stringdata(41:48),PickTypes))
      time=data(9);
      return
   end
end
% Now check the USER and KUSER fields
if ~isempty(findstr(stringdata(49:128),PickTypes))
   index=findstr(stringdata(49:128),PickTypes);
   if fix(index/8)*8 == index
      index=fix(index/8);
   else
      index=fix(index/8)+1;
   end
   value=data(10+index);
   if value >= t(1) & value <=t(length(t))
      time=value(1);
      return
   end
end



function handle=CreatePickMarker2(Position,string)
% After a new pick has been read this function plots the pick
% at the correct location as a line with a type string.
% This function is used when picks are created from information
% in the data files (no mouse action involved).

handle=zeros(2,1);
Yval=Position(2);
handle(1)=line([Position(1,1),Position(1,1)],[Yval-.5,Yval+.5],...
          'color','k','erasemode','xor','Tag','PickLine');
handle(2)=text(Position(1,1),Yval-.65,string,...
          'color','r','erasemode','xor','Tag','PickText');
