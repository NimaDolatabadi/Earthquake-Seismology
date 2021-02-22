function [azimuth,incid,Polariz]=getWinPolariz(startTime,WinLength)
% This function is called after data is read into the workspace, or
% after changes to the analysis window, or after filtering. It
% determines the azimuth and incidence angle of the principal polarization
% direction of the signal within the analysis window by an eigenvalue
% analysis of the zero-lag covariance matrix of the data. 


global t			% time for all 3 components

global yn			% data for North component
global ye			% data for East component
global yz			% data for Vert component

global dtn			% delta T for North component
global RotationCorrection	% Subtract this angle from all computed azimuths


[Y,ist]=min(abs(t-startTime));

[Y,lst]=min(abs(t-startTime-WinLength));
x=ye(ist:lst);
y=yn(ist:lst);
z=yz(ist:lst);
A=cov([x,y,z]);
[V,D] = eig(A);

% get max eigenvalue...
[Y,I]=max(max((D)));
Vmax=V(:,I);

% and calculate azimuth and incidence angle...
th=atan2(Vmax(2),Vmax(1));
azimuth=90-th*180/pi;
horiz=sqrt(Vmax(2)^2+Vmax(1)^2);
th=atan2(-Vmax(3),horiz);
incid=th*180/pi;
if incid < -90
   incid=abs(incid)-90;
elseif incid < 0
   azimuth=azimuth+180;
   if azimuth > 360, azimuth=azimuth-360;end
   incid=90+incid;
elseif incid < 90
   incid=90-incid;
else
   azimuth=azimuth+180;
   if azimuth > 360, azimuth=azimuth-360;end
   incid=incid-90;
end

azimuth = azimuth - RotationCorrection;  
if azimuth < 0, azimuth=360+azimuth;end
if azimuth > 360, azimuth=azimuth-360;end

%Get the polarization
eigs=diag(D);
numerator=(eigs(1) - eigs(2))^2 + (eigs(1) - eigs(3))^2 + (eigs(2) - eigs(3))^2;
denom=2*(sum(eigs))^2;
Polariz=numerator/denom;
