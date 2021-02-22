function arrow3d(length,x0,y0,z0,orient)
% This function plots 3-d arrow heads on the particle-motion plot
% to indicate the direction of particle motion. The arrow heads consist
% of 4 triangular patches with one common vertex at x0,y0,z0. The
% direction of motion is in the orient vector.


b=length/5;
R=eye(3);
R(:,1)=orient;
R(1,:)=orient';

% patch1
x=[0,0,length];
y=[-b,-b,0];
z=[b,-b,0];
U=R*[x;y;z];
x=U(1,:)+x0;
y=U(2,:)+y0;
z=U(3,:)+z0;
X(:,1)=x';
Y(:,1)=y';
Z(:,1)=z';
%patch(x,y,z,'w')

%patch2
x=[0,0,length];
y=[b,-b,0];
z=[b,b,0];
U=R*[x;y;z];
x=U(1,:)+x0;
y=U(2,:)+y0;
z=U(3,:)+z0;
X(:,2)=x';
Y(:,2)=y';
Z(:,2)=z';
%patch(x,y,z,'w')

%patch3
x=[0,0,length];
y=[b,-b,0];
z=[-b,-b,0];
U=R*[x;y;z];
x=U(1,:)+x0;
y=U(2,:)+y0;
z=U(3,:)+z0;
X(:,3)=x';
Y(:,3)=y';
Z(:,3)=z';
%patch(x,y,z,'w')

%patch4
x=[0,0,length];
y=[b,b,0];
z=[b,-b,0];
U=R*[x;y;z];
x=U(1,:)+x0;
y=U(2,:)+y0;
z=U(3,:)+z0;
X(:,4)=x';
Y(:,4)=y';
Z(:,4)=z';
patch(X,Y,Z,'w')

