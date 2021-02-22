function PrevGroup
% Make the previous 3-component set current, and load into workspace.

global CurrentSet		% Index of current 3-comp set being analyzed
global NumSets			% number of current 3-comp sets


if CurrentSet == 1, return, end
CurrentSet=CurrentSet-1;
hPrev1=findobj(gcf,'Tag','mPrevGroup');
hPrev2=findobj(gcf,'Tag','PrevGroup');
hNext1=findobj(gcf,'Tag','mNextGroup');
hNext2=findobj(gcf,'Tag','NextGroup');

if CurrentSet == 1
   set(hPrev1,'enable','off')
   set(hPrev2,'enable','off')
end
if CurrentSet < NumSets
   set(hNext1,'enable','on')
   set(hNext2,'enable','on')
end

GetFiles;

