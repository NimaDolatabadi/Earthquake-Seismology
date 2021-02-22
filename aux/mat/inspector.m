%
%INSPECTOR
%
%   Display a Window Interface to Interact with SAC Data and
%   Workspace Variables


global traceIndex
global SeisData
global SACdata
global BlackBoard
global Ntraces

% Set the index to the initial trace
traceIndex = 1;

% Create the Figure
[fig, ax] = inspFigure();

% Create the Toolbar
inspToolbar(fig);

% Create the Plot of Data
inspRealPlot(ax);	

% Clear unneeded variables
clear fig
clear ax

% Turn the Cursor to Zoom in and out
zoom on	
	
