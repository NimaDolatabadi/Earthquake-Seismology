function [NumberSets] = SacCheck() 
  NumberSets = 0;
  if(exist('THeaderData') == 0) 
    warning('Required Variable THeaderData not found')
    return
  end
  
  [junk, NumberSets] = size(THeaderData);
  NumberSets = fix(NumberSets/3);
  return
  