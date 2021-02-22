function DispatchButtonPress
% This function handles button presses that take place within the particle-
% motion plot. If the mouse click is on the seismogram in the top part of 
% the figure, it signals a request to plot a cross on the correct point in 
% the particle motion plot. If it is a left-click on the "E" axis text, it
% signals a clockwise rotation around Z (Right click signals counter-
% clockwise rotation). Clicks on the "Z" handle signals rotations around
% a horizontal axis.

global TRUE
global FALSE
global SeisAxis			% Handle to seismogram axis
global ppmAxis			% Handle to PPM axis
global EAST			% Handle to east text
global VERT			% Handle to Z Text
global ANCHOR			% True if button is down and object selected
global hAZIM			% handle to azimuth text
global hELEV			% handle to elevation text
global hSEIS			% handle to seismogram line segments
global hPPM3			% handle to PPM line segments
global hCROSS			% handle to cross marking selected point on PPM path
global MAKECROSS		% True if button press was on seismogram
global MAX			% Max value of the PPM plot
global MIN			% Min value of the PPM plot

curObj=get(gcf,'CurrentObject');
MAKECROSS=0;
CrossWidth=(MAX-MIN)/5;

% First see if mouse click was on the seismogram.
if strcmp( get(curObj,'Type'), 'line' )
   ClickedOnSeis=FALSE;
   for j=1:length(hSEIS)
      if curObj == hSEIS(j)
         ClickedOnSeis=1;
         curSegment=j;
         break;
      end
   end
   if ClickedOnSeis	% If so put up a cross on the PPM plot
      MAKECROSS=TRUE;
      curPnt=get(SeisAxis,'CurrentPoint');
      % Get the index from the time vector
      Tdata=get(curObj,'Xdata');
      curTime=curPnt(1,1);
      [Y,I]=min(abs(Tdata-curTime));
      
      Xdata=get(hPPM3(curSegment),'Xdata');
      Ydata=get(hPPM3(curSegment),'Ydata');
      Zdata=get(hPPM3(curSegment),'Zdata');
      if I > length(Xdata),I=length(Xdata);end;
      
      % These are the coordinates.
      Xtarget=Xdata(I);
      Ytarget=Ydata(I);
      Ztarget=Zdata(I);
      subplot(ppmAxis);
      hCROSS=mk3Dcross(Xtarget,Ytarget,Ztarget,CrossWidth,'r');
      ANCHOR=TRUE;
      return;
   end
end


button = get(gcf, 'SelectionType'); % mouse button number

% See if a horizontal rotation is selected.
if curObj == EAST
      ANCHOR=TRUE;
      while ANCHOR
         VIEW=get(ppmAxis,'view');
         if strcmp(button,'normal') %left button , so clockwise
            VIEW(1)=VIEW(1)+5;
         else			    % counter-clockwise
            VIEW(1)=VIEW(1)-5;
         end
         set(hAZIM,'string',num2str(VIEW(1)));
         set(ppmAxis,'view',VIEW)
         drawnow
         if ~ ANCHOR
            break;
         end
      end
end

% Check for vertical rotation
if curObj == VERT
      ANCHOR=TRUE;
      while ANCHOR
         VIEW=get(ppmAxis,'view');
         if strcmp(button,'normal')
            VIEW(2)=VIEW(2)+5;
         else
            VIEW(2)=VIEW(2)-5;
         end
         set(hELEV,'string',num2str(VIEW(2)));
         set(ppmAxis,'view',VIEW)
         drawnow
         if ~ ANCHOR
            break;
         end
      end
end




function handle = mk3Dcross(x0,y0,z0,size,color)
% This function is used to display a 3-D cross on the particle motion plot.
% The location of the cross corresponds to the position of the mouse cursor
% in the seismogram plot in the top of the PPM window

handle=zeros(3,1);
handle(1)=line([x0,x0],[y0,y0],[z0+size,z0-size],'color',color,'erasemode','xor');
handle(2)=line([x0,x0],[y0+size,y0-size],[z0,z0],'color',color,'erasemode','xor');
handle(3)=line([x0+size,x0-size],[y0,y0],[z0,z0],'color',color,'erasemode','xor');
