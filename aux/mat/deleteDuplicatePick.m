function deleteDuplicatePick
% During the picking process it is possible for the user to set multiple
% copies of the same pick on any trace. The sensible thing to do in that case
% is assume that the user really wants the latest choice, and delete any earlier
% picks of that type. This function is called after every pick is set to
% see if there are multiple copies, and if so to delete earlier picks of that type.


global hActivePick		% handle to currently active pick
global NumZpicks		% number of picks on Z channel
global NumEpicks		% number of picks on E channel
global NumNpicks		% number of picks on N channel
global hZpick			% array of handles to Z pick markers
global hEpick			% array of handles to E pick markers
global hNpick			% array of handles to N pick markers

if isempty(hActivePick),return,end

string=get(hActivePick(2),'string');
ydata=get(hActivePick(1),'ydata');
Index=mean(ydata)/2+1;
if Index == 1
   for j=1:NumZpicks
      oldstring=get(hZpick(2,j),'string');
      if strcmp(string,oldstring) & (hActivePick ~= hZpick(:,j))
         delete(hZpick(1,j));
         delete(hZpick(2,j));
         hZpick(:,j)=[];
         NumZpicks=NumZpicks-1;
         break
      end
   end
elseif Index == 2
   for j=1:NumEpicks
      oldstring=get(hEpick(2,j),'string');
      if strcmp(string,oldstring) & (hActivePick ~= hEpick(:,j))
         delete(hEpick(1,j))
         delete(hEpick(2,j))
         hEpick(:,j)=[];
         NumEpicks=NumEpicks-1;
         break
      end
   end
elseif Index == 3
   for j=1:NumNpicks
      oldstring=get(hNpick(2,j),'string');
      if strcmp(string,oldstring) & (hActivePick ~= hNpick(:,j))
         delete(hNpick(1,j))
         delete(hNpick(2,j))
         hNpick(:,j)=[];
         NumNpicks=NumNpicks-1;
         break
      end
   end
end
