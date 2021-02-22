function ScrollHelp
% This is the callback function for the scrollbar that appears on help screens
% with more text than can fit onto one page. The text in any help topic is made
% up of a a number of text objects (one per line of text) By controlling the
% y-values of the text objects (on a fixed axis system) the text can be made to 
% scroll up or down. Text objects outside the axis limits have their visibility
% set to 'off', and object inside the axis limits have visibility set to 'on'.
% The amount to scroll is determined by getting the current value of the slider,
%  scaling by the slider limits, and adjusting the y values of the text objects
% by the difference between the value and the axis limits.



hs = findobj(gcf,'Tag','HelpSlider');
value=get(hs,'value');
max=get(hs,'max');
min=get(hs,'min');
value=max-value;
ylim=get(gca,'ylim');
delta=value-ylim(2);
h=findobj(gcf,'Tag','hlpStr');
h=flipud(h);
if delta > 0
   for j=1:length(h)
      ypos=(j-1)*.14;
      pos=get(h(j),'position');
      pos(2)=ypos-delta;
      if pos(2) < 0.1 | pos(2) > 2.95,set(h(j),'visible','off'),end
      set(h(j),'position',pos)
      if pos(2) > .1 & pos(2) <= 2.95, set(h(j),'visible','on'),end
   end 
else
   for j=1:length(h)
      ypos=(j-1)*.14;
      pos=get(h(j),'position');
      pos(2)=ypos;
      if pos(2) < 0.1 | pos(2) >2.95 ,set(h(j),'visible','off'),end
      set(h(j),'position',pos)
      if pos(2) > .1 & pos(2) <= 2.95, set(h(j),'visible','on'),end
   end
end 
