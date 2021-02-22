function MainButtonPress

% This function handles button presses that occur within the main window of
% the BAZ GUI and within the Filter design window. Within the main window, 
% a button press can signify the start of a zoom, unzoom, pick, analysis 
% window move, or analysis window resize. In the filter window, button presses 
% signal the start of a drag of the corner frequency markers.
global TRUE
global FALSE
global ZOOMANCHOR		% If true, a zoom is in progress
global ZOOMANCHORPOINT		% point where zoom began
global hRECT                    % handle to current rectangle
global UNZOOM			% un-zoom current display limits
global hPPMPATCH		% handle to graphical representation of PPM window
global hPPMEXPAND		% handle to expand symbol on PPM window
global EXPANDANCHOR		% if true then we are changing size of ppm window
global EXPANDANCHORPOINT	% point where expand drag started
global OLDPATCHEND		% patch end before drag started
global MOVEANCHOR		% if true then we are moving ppm window
global MOVEANCHORPOINT		% point where move drag started
global hTRACES			% handle to the trace plot on main screen

global hLCORNER			% handle to low corner line
global hHCORNER			% handle to high corner line
global DRAGHIGHCORNER		% if true, the high corner line in filter-set window is being dragged
global DRAGLOWCORNER		% if true, the low corner line in filter-set window is being dragged
global DOPICK			% if true picking is in progress
global OLDCURSOR		% saved cursor type
global hCROSSHAIR		% handle to pick crosshair
global hActivePick		% handle to currently active pick
global MOVEPICK			% if true pick is being moved
global MOVEPICKDELTA		% Pick value -X value of mouse position when pick move was started
global DATAREAD			% if true, then seismograms have been read into work space.

global hAXIS_SYMB		% handle to seismometer axes symbol
global ROTATETRACES		% if true a trace rotation is being selected
global AXIS_ROTATE_START	% point where axis rotation started

if ~DATAREAD,return,end

button = get(gcf, 'SelectionType'); % mouse button number
if strcmp(button,'normal') %left button , so this is a zoom or a selection
   curObj=get(gcf,'CurrentObject');
   if curObj == hPPMPATCH	% moving the analysis window
      subplot(hTRACES)
      MOVEANCHOR=TRUE;
      curPnt=get(gca,'CurrentPoint');
      MOVEANCHORPOINT=curPnt(1,1);
      xData=get(hPPMPATCH,'Xdata');
      OLDPATCHEND=xData(3:3);
      return
   elseif curObj == hPPMEXPAND	% change the size of the analysis window
      subplot(hTRACES)
      EXPANDANCHOR=TRUE;
      curPnt=get(gca,'CurrentPoint');
      EXPANDANCHORPOINT=curPnt(1,1);
      xData=get(hPPMPATCH,'Xdata');
      OLDPATCHEND=xData(3:3);
      return
   elseif ~isempty(hHCORNER ) & curObj == hHCORNER	% dragging the BP filter high-corner control
      curPnt=get(gca,'CurrentPoint');
      DRAGHIGHCORNER=TRUE;
      return
   elseif ~isempty(hLCORNER) & curObj == hLCORNER	% dragging the BP filter low-corner control
      curPnt=get(gca,'CurrentPoint');
      DRAGLOWCORNER=TRUE;
      return
   elseif curObj == hAXIS_SYMB				% clicked on seis axis handle
      AXIS_ROTATE_START=get(gca,'CurrentPoint');
      ROTATETRACES=TRUE;
      return
   else
      object=MatchPick(curObj);	% Selecting an existing pick?
      if ~isempty(object)	% object is a pick
         if ~isempty(hActivePick),set(hActivePick(2),'color','k'),end
         hActivePick=object;
         Xdata=get(object(1),'xdata');
         set(object(2),'color','r');
         MOVEPICK=TRUE;
         curPnt=get(gca,'CurrentPoint');
         MOVEPICKDELTA=curPnt(1,1)-Xdata(1);
         return
      end
   end
   ZOOMANCHOR=TRUE;
   curPnt=get(gca,'CurrentPoint');
   ZOOMANCHORPOINT=curPnt(1,1:2);
   corner2=ZOOMANCHORPOINT;
   hRECT=drawRect(ZOOMANCHORPOINT,corner2,'w');
elseif strcmp(button,'alt') %right button , so this is an un-zoom 
   UNZOOM=TRUE;
elseif strcmp(button,'extend') %middle button , so this is a pick
   DOPICK=TRUE;
   OLDCURSOR=get(gcf,'pointer');
   set(gcf,'pointer','crosshair')
   curPnt=get(gca,'CurrentPoint');
   hCROSSHAIR=crosshair(curPnt(1,1:2));     
end




function object=MatchPick(curObj)
% This function is called after a mouse button press to determine whether
% the current object selected by the mouse is one of the existing picks.
% If so a handle to the pick is returned.

global NumZpicks                % number of picks on Z channel
global NumEpicks                % number of picks on E channel
global NumNpicks                % number of picks on N channel
global hZpick                   % array of handles to Z pick markers
global hEpick                   % array of handles to E pick markers
global hNpick                   % array of handles to N pick markers

object=[];
for j=1:NumZpicks
   if curObj == hZpick(2,j)
      object=hZpick(:,j);
      return
   end
end

for j=1:NumEpicks
   if curObj == hEpick(2,j)
      object=hEpick(:,j);
      return
   end
end

for j=1:NumNpicks
   if curObj == hNpick(2,j)
      object=hNpick(:,j);
      return
   end
end
