function handle=mkAxisSymb(origin,Diameter,angle,color)
% This function creates the axis Symbol displayed on the compass plot.
% The symbol is created with its origin at the axis origin, and
% with axis arms of length Diameter, at an angle to the origin of angle, 
% and with color color.



x=[0 0 Diameter];
y=[Diameter 0 0];

th=-angle*pi/180;
cs=cos(th);
sn=sin(th);

R=[cs -sn ;sn cs];
V=R*[x;y];
handle=line(V(1,:), V(2,:),'color',color,'erasemode','xor','linewidth',3);
