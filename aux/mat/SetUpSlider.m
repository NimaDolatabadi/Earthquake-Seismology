function SetUpSlider
% This function is called at the beginning of the ML calculation. Because
% that calculation can be quite lengthy it is necessary to display some kind
% of progress information. This control displays progress using a
% thermometer-like device. At the beginning of the calculation the slider is
% all blue. As calculations are completed the blue area is covered with red
% until at the conclusion of calculations, the slider is all red. The slider
% is implemented as a figure with a frame enclosing two colored frames; one
% blue and one red. This function sets up these controls.

 
global hFIG			% array of handles to figures
global hSLIDERWIN		% handle to progress slider control
global hSLIDERDONE		% handle to done fraction
global hSLIDERTOTAL		% handle to total fraction of slider control

Width=210;
Height=95;
ScreenSize = get(0,'ScreenSize');

makeFigure(3,[(ScreenSize(3)-Width)/2 (ScreenSize(4)-Height)/2 Width Height],'Polarization Analysis');
hSLIDERWIN=hFIG(3);
set(hSLIDERWIN,'Resize','off')



uicontrol('style','frame','position',[10 5 190 80]);
hSLIDERTEXT=uicontrol('style','text','position',[65,55,80,20],'string','Working...');
hSLIDERTOTAL=uicontrol('style','frame','position',[35 25 140 20],...
              'backgroundcolor','b');
hSLIDERDONE=uicontrol('style','frame','position',[35 25 40 20],...
              'backgroundcolor','r');
