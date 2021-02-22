function DeletePick
% This function deletes the currect pick (the pick whose handle
% is hActivePick). In addition to deleting the pick marker from
% the screen, the function updates the arrays holding pick info.

global hActivePick		% handle to currently active pick
global NumZpicks		% number of picks on Z channel
global NumEpicks		% number of picks on E channel
global NumNpicks		% number of picks on N channel
global hZpick			% array of handles to Z pick markers
global hEpick			% array of handles to E pick markers
global hNpick			% array of handles to N pick markers

if isempty(hActivePick),return,end
for j=1:NumZpicks
   if (hActivePick == hZpick(:,j))
      delete(hZpick(1,j));
      delete(hZpick(2,j));
      hZpick(:,j)=[];
      NumZpicks=NumZpicks-1;
      hActivePick=[];
      return
   end
end

for j=1:NumEpicks
   if (hActivePick == hEpick(:,j))
      delete(hEpick(1,j));
      delete(hEpick(2,j));
      hEpick(:,j)=[];
      NumEpicks=NumEpicks-1;
      hActivePick=[];
      return
   end
end

for j=1:NumNpicks
   if (hActivePick == hNpick(:,j))
      delete(hNpick(1,j));
      delete(hNpick(2,j));
      hNpick(:,j)=[];
      NumNpicks=NumNpicks-1;
      hActivePick=[];
      return
   end
end
