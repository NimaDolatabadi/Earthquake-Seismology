function c = inspRealPlot(axis)
global traceIndex
global SeisData
global SACdata

% Clear the Current Axis
cla

% Set the current Trace index
index = traceIndex;

% Create the time axis
time  = linspace(SACdata(index).times.b, SACdata(index).times.e,SACdata(index).trcLen);

% Grab the Data
y     = SeisData(:,index); % - mean(SeisData(:,index));

% Determine the number of Data Points
NPTS  = SACdata(index).trcLen;

% Draw the Seismogram
c = line('Parent',     axis, ...
         'Color',      [0 0 1], ...
         'Tag',       'Seismogram', ...
         'XData',      time, ...
         'YData',      y(1:NPTS) );

% Setup the Labels on the Plot
xlabel('Time (seconds)')
ylabel('amplitude (units?)')

% Bound the Seismogram
range  = max(y)  - min(y);
bottom = mean(y) - range/8;

for i = 0:1:9
  t = getfield(SACdata(index),'times',[ 't', num2str(i) ]);
  if ~isempty(t)
    h=line([t,t], [bottom,bottom+range/4], 'color','r');
    string = getfield(SACdata(index), 'times', [ 'kt' num2str(i)]);
    if ~isempty(string)
      h = text(t,bottom,string); 
    end
  end
end


hTxtWin = findobj('Tag','FigTextWin');
if isempty(hTxtWin)
  hTxtWin = uipanel('Parent',           gcf, ...
                    'Units',           'Normalize', ...
                    'BackgroundColor',  [1 1 1], ...
                    'BorderWidth',      1, ...
                    'Position',         [0.75 0.85 0.2 0.12], ...
                    'Tag',             'FigTextWin');
  
end

% Create a Label with the Station Name, Component, 
%  Event Time/Date and the Event Name
label    = cell(4,1);
label{1} = [ 'Name: ' SACdata(index).station.kstnm ];
label{2} = [ 'Comp: ' SACdata(index).station.kcmpnm ];
timedate = [num2str(SACdata(index).event.nzyear) ' ' num2str(SACdata(index).event.nzjday)];
timedate = [timedate ' ' num2str(SACdata(index).event.nzhour) ' ' num2str(SACdata(index).event.nzmin)];
timedate = [timedate ' ' num2str(SACdata(index).event.nzsec) ];
label{3} = [ 'Time: ' timedate ];
label{4} = [ 'Event: ' SACdata(index).event.kevnm ];


hTxtString = findobj('Tag','plotLabel');
if ~isempty(hTxtString)
   delete(hTxtString)
end


hTxtString = uicontrol('Parent',               hTxtWin, ...
                       'Units',               'Normalize', ...
                       'BackgroundColor',      [1 1 1], ...
                       'Position',             [0.05 0.05 0.95 0.95], ...
                       'Style',               'text', ...
                       'string',               label, ...
                       'HorizontalAlignment', 'left', ...
                       'FontSize',            11, ...
                       'Tag',                 'plotLabel');

