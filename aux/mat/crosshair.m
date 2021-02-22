function handle=crosshair(pnt)
% during the picking process, as long as the mouse button is down, a cross hair
% is displayed on the screen. The crosshair moves with the mouse cursor. This
% is the function used to create the cross hair.


handle(1)=line([pnt(1), pnt(1)],[pnt(2)-.5, pnt(2)+.5],'color','k','erasemode','xor');
handle(2)=line([pnt(1)-.5, pnt(1)+.5],[pnt(2), pnt(2)],'color','k','erasemode','xor');
