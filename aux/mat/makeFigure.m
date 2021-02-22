function makeFigure(num,position,title)
% This function is called to create a figure with the specified title
% and position, and include the handle in the hFIG array.


global hFIG			% array of handles to figures

if hFIG(num)==0
   set(0,'defaultFigurePosition',position);
   set(0,'defaultFigureName',title);
   hFIG(num)=figure;
   set(gcf,'menubar','none')
else
   figure(hFIG(num))
   set(gcf,'Name',title)
   clf
end
