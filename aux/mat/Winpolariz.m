function Winpolariz
% get polarization of current window and update azimuth and dip arrows on main plot.

global hTRACES			% handle to the trace plot on main screen
global PPMWINLENGTH		% length of window for Particle motion plots
global PPMWINSTART		% start of window for Particle motion plots
global hCOMPASS			% handle to the compass plot
global hPOLARARROW		% handle to arrow showing polarization azimuth
global hDIP			% handle to the dip cartoon plot
global hPOLARDIPARROW		% handle to arrow showing dip
global hFIG			% array of handles to figures
global hINCIDTEXT		% handle to angle of incidence text
global hBAZTEXT			% handle to BAZ text.
global hELLIPTEXT		% handle to ellipticity text.
		% handle to ellipticity text.

[azimuth,incidence,Polariz]=getWinPolariz(PPMWINSTART,PPMWINLENGTH);
figure(hFIG(1))
subplot(hCOMPASS)
radius=0.8;
if ~isempty(hPOLARARROW)
   delete(hPOLARARROW)
end
hPOLARARROW=mkArrow([0,0],radius,azimuth,'r');
set(hBAZTEXT,'string',num2str(round(azimuth)))

subplot(hDIP)
if ~isempty(hPOLARDIPARROW)
   delete(hPOLARDIPARROW)
end
hPOLARDIPARROW=mkArrow([0,0],radius,incidence+180,'r');

subplot(hTRACES)
set(hINCIDTEXT,'string',num2str(round(incidence)))
set(hELLIPTEXT,'string',num2str(Polariz))



function handle=mkArrow(origin,Diameter,angle,color)
% This function creates the arrows that show the back azimuth and incidence
% angles in the meter controls in the top part of the main screen of the GUI. 
% The arrows are created with theirs tails at origin, of length Diameter,
% at an angle to the origin of angle, and with color color.


m1=Diameter/35;
m2=Diameter/7;
m3=Diameter/5;
m4=Diameter-m3;
x=[-m1 -m1 -m2 0 m2 m1 m1];
y=[0 m4 m4 Diameter m4 m4 0];

th=-angle*pi/180;
cs=cos(th);
sn=sin(th);

R=[cs -sn ;sn cs];
V=R*[x;y];
handle=patch(V(1,:), V(2,:),color,'erasemode','xor');
