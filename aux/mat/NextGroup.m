function NextGroup
% Make the next 3-component set current, and load into workspace.

global CurrentSet		% Index of current 3-comp set being analyzed
global NumSets			% number of current 3-comp sets


if CurrentSet == NumSets, return, end
CurrentSet=CurrentSet+1;
hPrev1=findobj(gcf,'Tag','mPrevGroup');
hPrev2=findobj(gcf,'Tag','PrevGroup');
hNext1=findobj(gcf,'Tag','mNextGroup');
hNext2=findobj(gcf,'Tag','NextGroup');

if CurrentSet > 1
   set(hPrev1,'enable','on')
   set(hPrev2,'enable','on')
end
if CurrentSet == NumSets
   set(hNext1,'enable','off')
   set(hNext2,'enable','off')
end
GetFiles;

