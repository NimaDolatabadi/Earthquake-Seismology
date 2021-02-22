function cpoints = mkcircle(vec,rad,npts);
% create an array holding the coordinates for npts evenly spaced points
% on a circle of radius rad centered at vec(1),vec(2),vec(3), and lying
% in the x-y plane

cpoints=zeros(3,npts);
dt=2*pi/(npts-1);
for j=1:npts
   theta=j*dt;
   cpoints(1,j)=cos(theta)*rad+vec(1);
   cpoints(2,j)=sin(theta)*rad+vec(2);
   cpoints(3,j)=vec(3);
end
