function MainMouseMove
% This function is called whenever a mouse move is detected in either the main or 
% filter-design windows. A mouse move is significant if any object is being dragged
% as in picking, zooming, adjusting the analysis window, or adjusting the filter
% corner frequencies.

global TRUE
global FALSE
global hFIG			% array of handles to figures
global hRECT                    % handle to current rectangle
global ZOOMANCHOR		% If true, a zoom is in progress
global ZOOMANCHORPOINT		% point where zoom began
global EXPANDANCHOR		% if true then we are changing size of ppm window
global EXPANDANCHORPOINT	% point where expand drag started

global PPMWINLENGTH		% length of window for Particle motion plots
global PPMWINSTART		% start of window for Particle motion plots
global hPPMPATCH		% handle to graphical representation of PPM window
global hTRACES			% handle to the trace plot on main screen
global hCOMPASS			% handle to the compass plot
global OLDPATCHEND		% patch end before drag started
global hPPMEXPAND		% handle to expand symbol on PPM window
global MOVEANCHOR		% if true then we are moving ppm window
global MOVEANCHORPOINT		% point where move drag started
global ROTATETRACES		% if true a trace rotation is selected
global AXIS_ROTATE_START	% point where axis rotation started
global hAXIS_SYMB		% handle to seismometer axes symbol
global RotationCorrection       % Subtract this angle from all computed azimuths
global TOTAL_ANGLE		% total rotation angle selected so far

global hLCORNER			% handle to low corner line
global hHCORNER			% handle to high corner line
global DRAGHIGHCORNER		% if true, the high corner line in filter-set window is being dragged
global DRAGLOWCORNER		% if true, the low corner line in filter-set window is being dragged
global PLOTMADE			% true if seismograms loaded and plotted.
global hCROSSHAIR		% handle to pick crosshair
global DOPICK			% if true picking is in progress
global WAITTODISMISS		% if true the pick-type box is still open
global hPICK			% handle to pick button frame
global hActivePick		% handle to currently active pick
global MOVEPICK			% if true pick is being moved
global MOVEPICKDELTA		% Pick value -X value of mouse position when pick move was started
global DATAREAD			% if true, then seismograms have been read into work space.

if ~DATAREAD,return,end

if DOPICK
   delete(hCROSSHAIR)
   curPnt=get(gca,'CurrentPoint');
   hCROSSHAIR=crosshair(curPnt(1,1:2));     
   return
end

if WAITTODISMISS
   bounds=get(hPICK,'position');
   point=get(gcf,'CurrentPoint');
   if point(1) < bounds(1) | point(1) > (bounds(1)+bounds(3)) ...
      | point(2) < bounds(2) | point(2) > (bounds(2) +bounds(4))
      bounds(1)=205;
      bounds(2)=548;
      set(hPICK,'position',bounds)
      WAITTODISMISS=FALSE;
      deleteDuplicatePick
   end
   return
end

if MOVEPICK
   curPnt=get(gca,'CurrentPoint');
   NewXpos=curPnt(1,1)-MOVEPICKDELTA;
   xdata(1)=NewXpos;
   xdata(2)=xdata(1);
   set(hActivePick(1),'xdata',xdata);
   position=get(hActivePick(2),'position');
   position(1)=NewXpos;
   set(hActivePick(2),'position',position);
   return
end


% Take care of mofifying pointer shape for the drag and expand handles
% in figure 1.

if gcf == hFIG(1) & PLOTMADE
   hReg=get(hPPMEXPAND,'extent');
   hRegX=get(hPPMPATCH,'Xdata');
   hRegY=get(hPPMPATCH,'Ydata');
   pos=get(gca,'currentpoint');
   if (pos(1,1) > hReg(1)) & (pos(1,1) < hReg(1) + hReg(3)) & (pos(1,2) > hReg(2))...
      & (pos(1,2) < hReg(2) + hReg(4))
        set(gcf,'pointer','fleur')
   elseif (pos(1,1) > hRegX(1)) & (pos(1,1) < hRegX(3) ) & (pos(1,2) > hRegY(1))...
      & (pos(1,2) < hRegY(2))
        set(gcf,'pointer','cross')
   else
        set(gcf,'pointer','arrow')
   end
end

   
if  ZOOMANCHOR
   delete(hRECT)
   corner2=get(hTRACES,'CurrentPoint');
   hRECT=drawRect(ZOOMANCHORPOINT,corner2,'k');
   return
end

if EXPANDANCHOR
   curPnt=get(hTRACES,'CurrentPoint');
   dx=curPnt(1,1)-EXPANDANCHORPOINT;
   curPatchEnd=OLDPATCHEND+dx;
   xData=get(hPPMPATCH,'Xdata');
   if curPatchEnd-xData(1:1) <= 0,return,end;
      
   PPMWINLENGTH=curPatchEnd-xData(1:1);
   xData(3:3)=curPatchEnd;
   xData(4:4)=curPatchEnd;
   set(hPPMPATCH,'Xdata',xData);
   textPos=get(hPPMEXPAND,'Position');
   textPos(1)=curPatchEnd;
   set(hPPMEXPAND,'Position',textPos);
   drawnow
   return
end   

if MOVEANCHOR
   curPnt=get(hTRACES,'CurrentPoint');
   dx=curPnt(1,1)-MOVEANCHORPOINT;
   curPatchEnd=OLDPATCHEND+dx;
   xData=get(hPPMPATCH,'Xdata');
      
   xData(3:3)=curPatchEnd;
   xData(4:4)=curPatchEnd;
   PPMWINSTART=curPatchEnd-PPMWINLENGTH;
   xData(1:1)=PPMWINSTART;
   xData(2:2)=PPMWINSTART;
   set(hPPMPATCH,'Xdata',xData);
   textPos=get(hPPMEXPAND,'Position');
   textPos(1)=curPatchEnd;
   set(hPPMEXPAND,'Position',textPos);
   drawnow
   return
end   

if ROTATETRACES
   curPnt=get(hCOMPASS,'CurrentPoint');
   curAngle=getAngle(curPnt,AXIS_ROTATE_START);
   delete(hAXIS_SYMB)
   TOTAL_ANGLE=RotationCorrection+curAngle;
   hAXIS_SYMB=mkAxisSymb([0,0],0.7,-TOTAL_ANGLE,'b');
   drawnow
   return
end
   
if DRAGHIGHCORNER
   curPnt=get(gca,'CurrentPoint');
   Xdata=[curPnt(1,1) curPnt(1,1)];
   set(hHCORNER,'Xdata',Xdata);
end

if DRAGLOWCORNER
   curPnt=get(gca,'CurrentPoint');
   Xdata=[curPnt(1,1) curPnt(1,1)];
   set(hLCORNER,'Xdata',Xdata);
end



function angle=getAngle(newPnt,origPnt)

Vnew=newPnt(1,1:2);
Vold=origPnt(1,1:2);
Vnew=Vnew/norm(Vnew);
Vold=Vold/norm(Vold);
dot=sum(Vnew.*Vold);
cross=Vold(1)*Vnew(2)-Vnew(1)*Vold(2);
angle=acos(dot)*180/pi*sign(cross);

