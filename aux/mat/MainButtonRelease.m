function MainButtonRelease
% This function handles button releases in the main and filter-design
% windows. The action to be taken depends on the state of the logical
% globals listed below. The function is basically just an if...elseif...
% construct in which each of these globals is checked and appropriate
% action is taken when one is found to be true.

global TRUE
global FALSE
global hFIG			% array of handles to figures

global ZOOMANCHOR		% If true, a zoom is in progress
global EXPANDANCHOR		% if true then we are changing size of ppm window
global MOVEANCHOR		% if true then we are moving ppm window
global DRAGHIGHCORNER		% if true, the high corner line in filter-set window is being dragged
global DRAGLOWCORNER		% if true, the low corner line in filter-set window is being dragged
global DOPICK			% if true picking is in progress
global WAITTODISMISS		% if true the pick-type box is still open
global MOVEPICK			% if true pick is being moved
global DATAREAD			% if true, then seismograms have been read into work space.

global hRECT                    % handle to current rectangle
global ZOOMANCHORPOINT		% point where zoom began
global UNZOOM			% un-zoom current display limits
global hCOMPASSARROW		% handle to arrow showing orientation of North component
global ROTATEAZANCHOR		% if true the azimuth arrow is being rotated
global ROTATEAZANCHORPOINT	% coordinates of point where azimuth rotation began
global hEDITHCORNER		% handle to high-corner edit control
global hEDITLCORNER		% handle to low-corner edit control
global hHCORNER			% handle to high corner line
global hLCORNER			% handle to low corner line
global PPM3ACTIVE		% true if ppm3 window is open
global OLDCURSOR		% saved cursor type
global hCROSSHAIR		% handle to pick crosshair
global hPICK			% handle to pick button frame
global hActivePick		% handle to currently active pick
global NumZpicks		% number of picks on Z channel
global NumEpicks		% number of picks on E channel
global NumNpicks		% number of picks on N channel
global hZpick			% array of handles to Z pick markers
global hEpick			% array of handles to E pick markers
global hNpick			% array of handles to N pick markers
global hMAGNIFY			% handle to magnify slider
global ROTATETRACES		% if true a trace rotation is selected
global RotationCorrection       % Subtract this angle from all computed azimuths
global TOTAL_ANGLE		% total rotation angle selected so far

if ~DATAREAD,return,end


if  ZOOMANCHOR			% Zoom in to new axis limits
   ZOOMANCHOR=FALSE;
   delete(hRECT)
   corner2=get(gca,'CurrentPoint');
   dy=abs(corner2(1,2)-ZOOMANCHORPOINT(1,2));
   dx=abs(corner2(1,1)-ZOOMANCHORPOINT(1,1));
   if dy < 2 | dx == 0
      return
   end
   
% Save old axis limits on stack
   PushLimits(get(gca,'xlim'));   
   tmin=min([corner2(1,1),ZOOMANCHORPOINT(1,1)]);
   tmax=max([corner2(1,1),ZOOMANCHORPOINT(1,1)]);
   set(gca,'xlim',[tmin,tmax])
elseif EXPANDANCHOR   
   EXPANDANCHOR=FALSE;
   if PPM3ACTIVE 
      UpdatePPMplot
      drawnow
      figure(hFIG(1))
   end
   Winpolariz
   return   
elseif MOVEANCHOR
   MOVEANCHOR=FALSE;
   if PPM3ACTIVE
      UpdatePPMplot
      drawnow
      figure(hFIG(1))
   end
   Winpolariz
   return
elseif UNZOOM
   UNZOOM=FALSE;
   limits=PopLimitStack;
   if isempty(limits),return,end;
   set(gca,'xlim',limits);
   tmin=limits(1);
   tmax=limits(2);
elseif ROTATEAZANCHOR
   ROTATEAZANCHOR=FALSE;
   set(hCOMPASSARROW, 'FaceColor','b')   
   return
elseif DRAGHIGHCORNER
   DRAGHIGHCORNER=FALSE;
   Xdata=get(hHCORNER,'Xdata');
   newCorner=Xdata(1,1);
   set(hEDITHCORNER,'string',num2str(newCorner));
   SetHighCorner
   return   
elseif DRAGLOWCORNER
   DRAGLOWCORNER=FALSE;
   Xdata=get(hLCORNER,'Xdata');
   newCorner=Xdata(1,1);
   set(hEDITLCORNER,'string',num2str(newCorner));
   SetLowCorner
   return   
elseif DOPICK
   DOPICK=FALSE;
   set(gcf,'pointer',OLDCURSOR)
   delete(hCROSSHAIR)
   pickPos=get(gca,'CurrentPoint');
   [hNewPick,TrceIndex]=CreatePickMarker(pickPos);
   if ~isempty(hActivePick)
      set(hActivePick(2),'color','k')
   end
   hActivePick=hNewPick;
   if TrceIndex == 1
      NumZpicks=NumZpicks+1;
      hZpick(:,NumZpicks)=hNewPick;
   elseif TrceIndex == 2
      NumEpicks=NumEpicks+1;
      hEpick(:,NumEpicks)=hNewPick;
   else
      NumNpicks=NumNpicks+1;
      hNpick(:,NumNpicks)=hNewPick;
   end
   
   curPnt=get(gcf,'CurrentPoint');
   position=get(hPICK,'position');
   position(1)=curPnt(1,1)-5;
   position(2)=curPnt(1,2)-5;
   set(hPICK,'position',position);
   WAITTODISMISS=TRUE;
   return
elseif MOVEPICK
   MOVEPICK=FALSE;
   return
elseif ROTATETRACES
   ROTATETRACES=FALSE;
   RotationAngle = TOTAL_ANGLE - RotationCorrection;
   RotationCorrection = setDomain(TOTAL_ANGLE);
   DoRotations(RotationAngle)    
   
   return   
end   
   
value=get(hMAGNIFY,'value');
Mag=exp(value);      
RedrawSeis([tmin tmax],Mag)



function PushLimits(limits)
% In order to allow the user to undo any zooms, every time a zoom operation
% is performed, the old plot limits are pushed onto a stack. This
% routine is used to add a set of axis limits to the stack when 
% a zoom is initiated.

global StackIndex                       % Index into zoom stack
global Stack                            % Array of xlimits

StackIndex=StackIndex+1;
Stack(StackIndex,:)=limits;



function limits=PopLimitStack
% In order to allow the user to undo any zooms, every time a zoom operation
% is performed, the old plot limits are pushed onto a stack. This routine
% is called during an unzoom, to get the limits to redraw the figure, and
% to adjust the stack.

global StackIndex                       % Index into zoom stack
global Stack                            % Array of xlimits

limits=[];
if StackIndex < 1,return,end;
limits=Stack(StackIndex,:);
Stack(StackIndex,:)=[];
StackIndex=StackIndex-1;





function [handle,index]=CreatePickMarker(Position)
% After a new pick has been created, this function plots the pick
% at the correct location as a line with a type string.
% the handle is returned for later modification of pick marker,
% and the index is returned so the proper pick array can be augmented.

global hPICK                    % handle to pick button frame

handle=zeros(2,1);
Svec=[0;2;4];
[Y,I]=min(abs(Svec-Position(1,2)));
Yval=Svec(I);
handle(1)=line([Position(1,1),Position(1,1)],[Yval-.5,Yval+.5],...
          'color','k','erasemode','xor','Tag','PickLine');
PickIndex=get(hPICK,'value');   % type of pick 
string=get(hPICK,'string');
handle(2)=text(Position(1,1),Yval-.65,string(PickIndex,:),...
          'color','r','erasemode','xor','Tag','PickText');
index=Yval/2+1;
