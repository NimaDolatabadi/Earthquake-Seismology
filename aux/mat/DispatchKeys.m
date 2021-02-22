function DispatchKeys
% If a "d" or "D" is pressed delete the current pick.

key=get(gcf,'CurrentCharacter');
if strcmp(key,'D') | strcmp(key,'d')
   DeletePick
end
