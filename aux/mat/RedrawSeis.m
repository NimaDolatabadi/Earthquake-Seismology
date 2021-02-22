function RedrawSeis(xlim,Mag)
% Various operations such as changing magnification, zooming, filtering, etc.
% require that the seismogram plot on the main window be redrawn. In addition
% to the seismograms, the plot contains a number of other objects, most of
% which are XORed. These must be made invisible before redrawing the seismograms
% or the seimograms will be XORed with the object where they overlap. After
% the seismograms are replotted, the object visibility is restored.

tmin=xlim(1);
tmax=xlim(2);

% Get handles and make everything invisible...

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


hNORTHTRACE=findobj(gcf,'Tag','Ntrce');
hEASTTRACE=findobj(gcf,'Tag','Etrce');
hVERTTRACE=findobj(gcf,'Tag','Vtrce');

%set(hNORTHTRACE,'visible','off')
%set(hEASTTRACE,'visible','off')
%set(hVERTTRACE,'visible','off')


% reset the seismogram info text position and make invisible   
   for j=1:6
      tag=num2str(j);
      h=findobj(gcf,'Tag',tag);
      pos=get(h,'position');
      pos(1)=tmin;
      set(h,'position',pos,'visible','off')
   end




   Tdata=get(hNORTHTRACE,'xdata');
   [Y,Imin]=min(abs(Tdata-tmin));
   [Y,Imax]=min(abs(Tdata-tmax));
   Ydata=get(hNORTHTRACE,'ydata');
   Ymax=max(Ydata(Imin:Imax));
   Ymin=min(Ydata(Imin:Imax));
   ScaleFactor=2/(Ymax-Ymin)*Mag;
   set(hNORTHTRACE,'ydata',(Ydata-mean(Ydata))*ScaleFactor+4);

   Ydata=get(hEASTTRACE,'ydata');
   Ymax=max(Ydata(Imin:Imax));
   Ymin=min(Ydata(Imin:Imax));
   ScaleFactor=2/(Ymax-Ymin)*Mag;
   set(hEASTTRACE,'ydata',(Ydata-mean(Ydata))*ScaleFactor+2);
 
   Ydata=get(hVERTTRACE,'ydata');
   Ymax=max(Ydata(Imin:Imax));
   Ymin=min(Ydata(Imin:Imax));
   ScaleFactor=2/(Ymax-Ymin)*Mag;
   set(hVERTTRACE,'ydata',(Ydata-mean(Ydata))*ScaleFactor);
   



% Make everything visible again.
%set(hNORTHTRACE,'visible','on')
%set(hEASTTRACE,'visible','on')
%set(hVERTTRACE,'visible','on')


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
