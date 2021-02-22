function handle=drawRect(corner1,corner2,color)
% This function is called during a zoom operation to draw a rectangle
% showing the current extent of the selected region. The rectangle
% is drawn with XOR attributes so that it can be erased without damaging
% the underlying screen.

x(1)=corner1(1,1);
x(2)=corner2(1,1);
x(3)=x(2);
x(4)=x(1);
x(5)=x(4);
y(1)=corner1(1,2);
y(2)=y(1);
y(3)=corner2(1,2);
y(4)=corner2(1,2);
y(5)=y(1);
handle=line(x,y,'color',color,'erasemode','xor','linestyle','--');
