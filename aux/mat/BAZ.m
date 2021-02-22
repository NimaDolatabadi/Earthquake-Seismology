%
% This is the main function of BAZ. It initializes a number of global
% variables used by different routines, and sets up the GUI

global NumSets			% number of current 3-comp sets
NumSets = SacCheck();
if NumSets < 1
  quitP
  error('Number of Data Sets are less than 1')
end

% ....................... variable declarations ........................
global hFIG			% array of handles to figures
global TRUE
global FALSE
global GUIRUNNING		% if true, then GUI is still running so stay in event loop.

global ZOOMANCHOR		% If true, a zoom is in progress
global UNZOOM			% un-zoom current display limits
global PPMWINLENGTH		% length of window for Particle motion plots
global EXPANDANCHOR		% if true then we are changing size of ppm window
global MOVEANCHOR		% if true then we are moving ppm window
global hPOLARARROW		% handle to arrow showing polarization azimuth
global hCOMPASS			% handle to the compass plot
global hDIP			% handle to the dip cartoon plot
global hTRACES			% handle to the trace plot on main screen
global hPOLARDIPARROW		% handle to arrow showing dip
global ORDER			% filter order
global LCORNER			% filter low corner
global HCORNER			% filter high corner
global DRAGHIGHCORNER		% if true, the high corner line in filter-set window is being dragged
global DRAGLOWCORNER		% if true, the low corner line in filter-set window is being dragged
global ROTATETRACES		% if true a trace rotation is being selected
global PLOTMADE			% true if seismograms loaded and plotted.
global PPM3ACTIVE		% true if ppm3 window is open
global hINCIDTEXT		% handle to angle of incidence text
global hBAZTEXT			% handle to BAZ text.
global hELLIPTEXT		% handle to ellipticity text.
global hRESULT			% handle to display panel for BAZ and incidence
global DOPICK			% if true picking is in progress
global hPICK			% handle to pick button frame
global PickTypes		% array of strings for pick types

global hActivePick		% handle to currently active pick
global NumZpicks		% number of picks on Z channel
global NumEpicks		% number of picks on E channel
global NumNpicks		% number of picks on N channel
global hZpick			% array of handles to Z pick markers
global hEpick			% array of handles to E pick markers
global hNpick			% array of handles to N pick markers
global WAITTODISMISS		% if true the pick-type box is still open
global MOVEPICK			% if true pick is being moved
global hDELPICK			% handle to pick delete button
global DATAREAD			% if true, then seismograms have been read into work space.


global CurrentSet		% Index of current 3-comp set being analyzed





FALSE               = 0;
TRUE                = 1;
CurrentSet          = 1;
GUIRUNNING          = TRUE;
ZOOMANCHOR          = FALSE;
EXPANDANCHOR        = FALSE;
MOVEANCHOR          = FALSE;
ROTATETRACES        = FALSE;
UNZOOM              = FALSE;
PPMWINLENGTH        = 3.0;
hPOLARARROW         = [];
hPOLARDIPARROW      = [];
hCOMPASS            = [];
hDIP                = [];
hTRACES             = [];
ORDER               = 2;
LCORNER             = 0.05;
HCORNER             = 1.0;
DRAGHIGHCORNER      = FALSE;
DRAGLOWCORNER       = FALSE;
PLOTMADE            = FALSE;
PPM3ACTIVE          = FALSE;
DOPICK              = FALSE;
PickTypes           = ['Pn';'Pg';'Sn';'S ';'Lg';'Rg'];

hActivePick         = [];	
NumZpicks           = 0;	
NumEpicks           = 0;	
NumNpicks           = 0;	
hZpick              = [];	
hEpick              = [];	
hNpick              = [];	
WAITTODISMISS       = FALSE;
MOVEPICK            = FALSE;
DATAREAD            = FALSE;


% Initialize figure1 of the GUI
set(0,'defaultFigureNumberTitle','off');
set(0,'defaultfigurecolor','w')
hFIG                = zeros(10,1);
p                   = get(0,'ScreenSize');
Width               = p(3);
Height              = p(4);
bot                 = (Height - 700) / 2;
left                = (Width - 555) / 2;
makeFigure(1,[left bot 555 700],'Signal Polarization and BAZ estimator');
h1=hFIG(1);
set(h1,'Resize','off')
set(h1,'windowbuttondownfcn','MainButtonPress','interruptible','on')
set(h1,'windowButtonMotionFcn','MainMouseMove')
set(h1,'windowButtonUpFcn','MainButtonRelease')
set(h1,'KeyPressFcn','DispatchKeys')

% Set up menu items
hFILE=uimenu('label','File  ');
   uimenu(hFILE,'label','Previous','callback','PrevGroup','Tag','mPrevGroup','enable','off');
   hNext=uimenu(hFILE,'label','Next','callback','NextGroup','Tag','mNextGroup');
   hFILESAVE=uimenu(hFILE,'label','Save Results','callback','SaveResults','Tag','Fsave');
   uimenu(hFILE,'label','Exit','Separator','on','callback','quitP');
   if NumSets == 1,set(hNext,'enable','off'),end

hACTION=uimenu('label','Action  ');
   hWINLENGTH=uimenu(hACTION,'label','Adjust Filter','callback','SetFilter');
   hAUTOROTATE=uimenu(hACTION,'label','Band-Pass Filter','callback','filterTraces','Tag','BPfilt');
   hPOLARIZATION=uimenu(hACTION,'label','Polarization','callback','mlm','Tag','TrcePolariz');


hHELP=uimenu('label','Help');
   hOVERVIEW=uimenu(hHELP,'label','Overview','callback','dispHelp(1)');
   hMAINSCR=uimenu(hHELP,'label','Main Screen','callback','dispHelp(2)');
   hFILTER=uimenu(hHELP,'label','Filtering','callback','dispHelp(3)');
   hPARTMOT=uimenu(hHELP,'label','Particle Motion','callback','dispHelp(4)');
   hPOLARIZ=uimenu(hHELP,'label','Polarization','callback','dispHelp(5)');
   
   hABOUT=uimenu(hHELP,'label','About','callback','About');

     


% set up tool-bar
frame=uicontrol('style','frame','position',[20 640 520 40]);
frmClr=[.3 .3 .5];
set(frame','backgroundcolor',frmClr)
btnREAD=uicontrol('style','pushbutton','position',[35 645 60 30],'string','<< Prev'...
        ,'callback','PrevGroup','Tag','PrevGroup','enable','off');
btnREAD=uicontrol('style','pushbutton','position',[95 645 60 30],'string','Next >>'...
        ,'callback','NextGroup','Tag','NextGroup');
          if NumSets == 1,set(btnREAD,'enable','off'),end
        
btnAUTOROTATE=uicontrol('style','pushbutton','position',[175 645 100 30],'string'...
        ,'Polarization','callback','mlm');
btnSETPPMWIN=uicontrol('style','pushbutton','position',[285 645 60 30],'string'...
        ,'Filter','callback','filterTraces');
btnPPM=uicontrol('style','pushbutton','position',[355 645 50 30],'string'...
        ,'PPM','callback','ppm3');
btnWRITE=uicontrol('style','pushbutton','position',[415 645 60 30],'string'...
        ,'Update','callback','SaveResults');
btnQUIT=uicontrol('style','pushbutton','position',[485 645 40 30],'string'...
        ,'Quit','callback','quitP');


% Set up a master frame for the back azimuth and incidence angle controls...
hRESULT(1)=uicontrol('style','frame','position',[20 437 24 195],'backgroundcolor',frmClr,'visible','off');
hRESULT(2)=uicontrol('style','frame','position',[514 437 26 195],'backgroundcolor',frmClr,'visible','off');
hRESULT(3)=uicontrol('style','frame','position',[182 445 194 190],'backgroundcolor',frmClr,'visible','off');
hRESULT(4)=uicontrol('style','frame','position',[20 437 520 20],'backgroundcolor',frmClr,'visible','off');
hRESULT(5)=uicontrol('style','frame','position',[20 596 520 40],'backgroundcolor',frmClr,'visible','off');
hRESULT(6)=uicontrol('style','frame','position',[190 455 180 90],'visible','off');
hRESULT(7)=uicontrol('style','text','position',[205 510 100 20],'string','BACK AZIM','visible','off');
hRESULT(8)=uicontrol('style','text','position',[205 488 100 20],'string','INCIDENCE','visible','off');
hRESULT(14)=uicontrol('style','text','position',[205 465 100 20],'string','ELLIPTICITY','visible','off');
set(hRESULT(7),'HorizontalAlignment','left')
set(hRESULT(8),'HorizontalAlignment','left')
set(hRESULT(14),'HorizontalAlignment','left')
hRESULT(9)=uicontrol('style','frame','position',[310 515 50 20],'backgroundcolor',[1 1 1],'visible','off');
hRESULT(10)=uicontrol('style','frame','position',[310 490 50 20],'backgroundcolor',[1 1 1],'visible','off');
hRESULT(15)=uicontrol('style','frame','position',[310 465 50 20],'backgroundcolor',[1 1 1],'visible','off');

hBAZTEXT=uicontrol('style','text','position',[313 517 45 17],'backgroundcolor',[1 1 1],'Tag','hBAZIM');
hRESULT(11)=uicontrol('style','text','position',[65 600 100 20],'string','Back Azimuth','visible','off');
set(hRESULT(11),'ForegroundColor','y','BackGroundColor',frmClr)

hINCIDTEXT=uicontrol('style','text','position',[313 492 45 17],'backgroundcolor',[1 1 1],'Tag','hINCID');
hRESULT(12)=uicontrol('style','text','position',[390 600 120 20],'string','Incidence Angle','visible','off');
set(hRESULT(12),'ForegroundColor','y','BackGroundColor',frmClr)

hRESULT(13)=uicontrol('style','text','position',[205 600 145 20],'string','No data','visible','off');
set(hRESULT(13),'ForegroundColor','y','BackGroundColor',frmClr,'Tag','GroupInfo')

hELLIPTEXT=uicontrol('style','text','position',[313 467 45 17],'backgroundcolor',[1 1 1],'Tag','hELLIP');


% place controls for setting pick type and deleting picks.       
hPICK=uicontrol('style','popupmenu','position',[205 548 60 20],'string',PickTypes,...
'visible','off','callback','SetPickType');

hDELPICK=uicontrol('style','pushbutton','position',[300 548 60 20],'string','Delete',...
'visible','off','callback','DeletePick');

hROTBAZ=uicontrol('style','pushbutton','position',[225 572 120 20],'string','Rotate to BAZ',...
'visible','on','callback','RotateToBAZ');

GetFiles;


% Start infinite loop until exit...
GUIRUNNING=TRUE;
while TRUE
   drawnow
   if ~GUIRUNNING,break,end
end
close all hidden;


