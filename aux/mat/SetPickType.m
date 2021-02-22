function SetPickType
% During interactive picking, after the mouse button has been released a drop-down
% list of phase types appears at the mouse location. If the user chooses a pick
% from the list, this function is called. It determines the pick string from the
% value of the list control, sets the pick string to that value, and moves the drop-
% down list to its "parking" position on the main GUI screen. 

global TRUE
global FALSE
global hPICK			% handle to pick button frame
global hActivePick		% handle to currently active pick
global WAITTODISMISS		% if true the pick-type box is still open

if ~isempty(hActivePick)
  Index=get(hPICK,'value');
  string=get(hPICK,'string');
  set(hActivePick(2),'string',string(Index,:));
  WAITTODISMISS=FALSE;
  position=get(hPICK,'position');
  position(1)=205;
  position(2)=548;
  set(hPICK,'position',position)
  deleteDuplicatePick
end