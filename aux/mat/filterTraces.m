function filterTraces
% This function is called in response to a click on the Filter Traces button
% on the main GUI screen. It uses the current corner and order values to
% construct a Butterworth filter and apply it forwards and backwards to the data.

global hNORTHTRACE		% handle to North trace
global hEASTTRACE		% handle to East trace
global hVERTTRACE		% handle to Vert trace
global hPPMPATCH		% handle to graphical representation of PPM window

global LCORNER                  % lower corner frequency for BP filter
global HCORNER                  % upper corner frequency for BP filter
global ORDER    	        % filter order
global hTRACES			% handle to the trace plot on main screen
global dtn			% delta T for North component
global ynback			% un-modified copy of the north trace
global yeback			% un-modified copy of the east trace
global yzback			% un-modified copy of the vert trace
global yn			% data for North component
global ye			% data for East component
global yz			% data for Vert component

global ORDER			% filter order
global LCORNER			% filter low corner
global HCORNER			% filter high corner
global PPM3ACTIVE		% true if ppm3 window is open
global hMAGNIFY			% handle to magnify slider
global DATAREAD			% if true, then seismograms have been read into work space.


if ~DATAREAD,return,end


dt=dtn;


[b,a]=butter(ORDER,[LCORNER*dt*2 HCORNER*dt*2]);

   value=get(hMAGNIFY,'value');
   Mag=exp(value); 
   set(hPPMPATCH,'visible','off')
   limits=get(hTRACES,'xlim');
   tmin=limits(1);
   tmax=limits(2);
   Tdata=get(hNORTHTRACE,'xdata');
   [Y,Imin]=min(abs(Tdata-tmin));
   [Y,Imax]=min(abs(Tdata-tmax));
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
 
   yz=filtfilt(b,a,yzback);
   Ymax=max(yz(Imin:Imax));
   Ymin=min(yz(Imin:Imax));
   ScaleFactor=2/(Ymax-Ymin);
   set(hVERTTRACE,'ydata',(yz-mean(yz))*ScaleFactor*Mag);
   set(hPPMPATCH,'visible','on')

   if PPM3ACTIVE
      UpdatePPMplot
   end
   Winpolariz
   drawnow