function magnifyTrces
% This function is the callback to the slider control on the main screen of the GUI.
% It allows scaling from 10X to 0.1X


% magnify (demagnify) traces between 1/10 to 10 times default value
hslider=findobj(gcf,'Tag','Magnify');
value=get(hslider,'value');
Mag=exp(value);

hAxis=findobj(gcf,'Tag','TrcePlot');
xlim=get(hAxis,'xlim');
RedrawSeis(xlim,Mag)