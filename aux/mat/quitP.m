function quitP
% This function is called to quit the program. It closes all windows
% and restores the default figure properties that it modified.
global GUIRUNNING		% if true, then GUI is still running so stay in event loop.

GUIRUNNING=0;
return
close all
clear all
set(0,'defaultfigurecolor','k');
set(0,'defaultFigureName',' ');
set(0,'defaultFigureNumberTitle','on');
