function inspCallbacks(action)
  global traceIndex
  global SeisData
  global SACdata
  global Ntraces
  
  hNext = findobj('Tag','Next');
  hPrev = findobj('Tag','Previous');
  
  switch(action)
   case 'next'
    deleteInspectWin
    traceIndex = traceIndex + 1;
    set(hPrev,'enable','on')
    if traceIndex == Ntraces
      set(hNext,'enable','off')
    end
    seisHandle = inspRealPlot(gca);
    
   case 'prev'
    deleteInspectWin
    traceIndex = traceIndex - 1;
    set(hNext,'enable','on')
    if traceIndex == 1
      set(hPrev,'enable','off')
    end
    seisHandle = inspRealPlot(gca);
    
   case 'time'
    deleteInspectWin
    setUpFrame(action)
    
   case 'stat'
    deleteInspectWin
    setUpFrame(action)
    
   case 'event'
    deleteInspectWin
    setUpFrame(action)
    
   case 'user'
    deleteInspectWin
    setUpFrame(action)
    
   case 'descrip'
    deleteInspectWin
    setUpFrame(action)
    
   case 'evsta'
    deleteInspectWin
    setUpFrame(action)
    
   case 'llnl'
    deleteInspectWin
    setUpFrame(action)
    
   case 'response'
    deleteInspectWin
    setUpFrame(action)
    
   case 'bboard'
    deleteInspectWin
    setUpFrame(action)
    
   case 'Delete'
    deleteInspectWin
  end
  
  
function [n] = maxStringLengthCell(c) 
  n = 0;
  for i = 1:length(c)
    t = length(char(c{i}));
    n = max(t,n);
  end


function setUpFrame(type)
% build string array...
  [names, labels, values] = makeStringArray(type);
  
  % Get Handle to Current Callback Object
  h = gcbo;

  hFrame = findobj('Tag','PropWin');
  if isempty(hFrame)
    hFrame = uipanel('Parent',          gcf, ...
                     'Units',          'Normalized', ...
                     'BackgroundColor', [1 1 1], ...
                     'Position',        [0.00 0.00 0.02 0.02], ...
                     'Visible',        'off', ...
                     'BorderWidth',    1, ...
                     'Tag',            'PropWin');
     
  else
    set(hFrame,'visible','off')	    
  end

  
  frameWidth(1) = maxStringLengthCell(names) + 1;
  frameWidth(2) = maxStringLengthCell(labels) + 1;
  frameWidth(3) = maxStringLengthCell(values);
  frameWidth(4) = max(sum(frameWidth(1:3)), 25);
  frameHeight = length(values);

  p = get(h,'Parent');
  set(p, 'Units', 'characters');  frmPos = get(p,'position');  set(p, 'Units', 'Normalized');  
  set(h, 'Units', 'characters');  btnPos = get(h,'position');  set(h, 'Units', 'Normalized');
  frameLeft = frmPos(1) + btnPos(1);
  frameBot  = (frmPos(2) + btnPos(2)) - frameHeight - 1;

  dframe = [frameLeft, frameBot - 1, frameWidth(4), frameHeight + 2 ];
  set(hFrame, 'Units', 'characters', 'position',dframe);
  set(hFrame,'visible','on')

  sframe = [0.05, 0.05, 1/3.5, 0.95];
  if(frameWidth(1) > 0)
    hName = uicontrol('Parent',               hFrame, ...
                      'Units',               'Normalize', ...
                      'BackgroundColor',      [1 1 1], ...
                      'Position',             sframe, ...
                      'Style',               'text', ...
                      'String',               names, ...
                      'HorizontalAlignment', 'left', ...
                      'Tag',                 'PropNames');
  end
  sframe = [0.05 + 1/3.5, 0.05, 1/3.5, 0.95];
  if(frameWidth(2) > 0) 
    hLabels = uicontrol('Parent',               hFrame, ...
                        'Units',               'Normalize', ...
                        'BackgroundColor',      [1 1 1], ...
                        'Position',             sframe, ...
                        'Style',               'text', ...
                        'String',               labels, ...
                        'HorizontalAlignment', 'left', ...
                        'Tag',                 'PropLabels');
  end
  sframe = [0.05 + 2/3.5, 0.05, 1/3.5, 0.95];
  if(frameWidth(3) > 0) 
    hValues = uicontrol('Parent',               hFrame, ...
                        'Units',               'Normalize', ...
                        'BackgroundColor',      [1 1 1], ...
                        'Position',             sframe, ...
                        'Style',               'text', ...
                        'String',               values, ...
                        'HorizontalAlignment', 'left', ...
                        'Tag',                 'PropValues');
  end
  hclose = uicontrol('Parent',                  hFrame, ...
                     'Units',                  'Normalize', ...
                     'BackgroundColor',         [.5 .5 .5], ...
                     'Position',                [0.90, 0.0, 0.1, 0.1], ...
                     'String',                 'x', ...
                     'Callback',               'inspCallbacks(''Delete'')', ...
                     'Tag',                    'CloseFrame');
  set(hclose,'Units','points');
  pos = get(hclose,'Position');
  pos(3:4) = [ 15 15 ];
  set(hclose, 'Position',pos);
  set(hclose,'Units','Normalize');
  
  set(hFrame,  'Units', 'Normalized');
  
  
  
function [names, labels, values] = makeStringArray(type)
  global traceIndex
  global SACdata
  global BlackBoard
  
  switch(type)
   case 'time'
    time=SACdata(traceIndex).times;
    names  = cell(16,1);
    labels = cell(16,1);
    values = cell(16,1);
    names{1} = 'delta';
    names{2} = 'b';
    names{3} = 'e';
    names{4} = 'o';
    names{5} = 'ka - a';
    names{6} = 'kt0 - t0';
    names{7} = 'kt1 - t1';
    names{8} = 'kt2 - t2';
    names{9} = 'kt3 - t3';
    names{10} = 'kt4 - t4';
    names{11} = 'kt5 - t5';
    names{12} = 'kt6 - t6';
    names{13} = 'kt7 - t7';
    names{14} = 'kt8 - t8';
    names{15} = 'kt9 - t9';
    names{16} = 'kf - f';
    values{1} = num2str(time.delta);
    values{2} = num2str(time.b);
    values{3} = num2str(time.e);
    values{4} = num2str(time.o);
    values{5} = num2str(time.a);
    values{6} = num2str(time.t0);
    values{7} = num2str(time.t1);
    values{8} = num2str(time.t2);
    values{9} = num2str(time.t3);
    values{10} = num2str(time.t4);
    values{11} = num2str(time.t5);
    values{12} = num2str(time.t6);
    values{13} = num2str(time.t7);
    values{14} = num2str(time.t8);
    values{15} = num2str(time.t9);
    values{16} = num2str(time.f);
    
    labels{6} = time.kt0;
    labels{7} = time.kt1;
    labels{8} = time.kt2;
    labels{9} = time.kt3;
    labels{10} = time.kt4;
    labels{11} = time.kt5;
    labels{12} = time.kt6;
    labels{13} = time.kt7;
    labels{14} = time.kt8;
    labels{15} = time.kt9;
    labels{16} = time.kf; 
    
    
   case 'stat'
    stat=SACdata(traceIndex).station;
    names  = cell(9,1);
    labels = cell(9,1);
    values = cell(9,1);
    names{1} = 'stla';
    names{2} = 'stlo';
    names{3} = 'stel';
    names{4} = 'stdp';
    names{5} = 'cmpaz';
    names{6} = 'cmpinc';
    names{7} = 'kstnm';
    names{8} = 'kcmpnm';
    names{9} = 'knetwk';
    values = struct2cell(stat);
    
   case 'event'
    event  = SACdata(traceIndex).event;
    names  = cell(14,1);
    labels = cell(14,1);
    values = cell(14,1);
    names{1} = 'evla';
    names{2} = 'evlo';
    names{3} = 'evel';
    names{4} = 'evdp';
    names{5} = 'nzyear';
    names{6} = 'nzjday';
    names{7} = 'nzhour';
    names{8} = 'nzmin';
    names{9} = 'nzsec';
    names{10}= 'nzmsec';
    names{11}= 'kevnm';
    names{12}= 'mag';
    names{13}= 'imagtyp';
    names{14}= 'imagsrc';
    values = struct2cell(event);
    
   case 'user'
    user  = SACdata(traceIndex).user;
    names  = cell(10,1);
    labels = cell(10,1);
    values = cell(10,1);
    for j=1:10
      names{j} = ['user' num2str(j-1)];
      if ~isempty(user(j).label)
        labels{j} = user(j).label;
      end
      values{j} = num2str(user(j).data);
    end
    
   case 'descrip'
    desc  = SACdata(traceIndex).descrip;
    names  = cell(9,1);
    labels = cell(9,1);
    values = cell(9,1);
    names{1} = 'iftype';
    names{2} = 'idep';
    names{3} = 'iztype';
    names{4} = 'iinst';
    names{5} = 'istreg';
    names{6} = 'ievreg';
    names{7} = 'ievtyp';
    names{8} = 'iqual';
    names{9} = 'isynth';
    values = struct2cell(desc);
    
    
   case 'evsta'
    evsta  = SACdata(traceIndex).evsta;
    names  = cell(4,1);
    labels = cell(4,1);
    values = cell(4,1);
    names{1} = 'dist';
    names{2} = 'az';
    names{3} = 'baz';
    names{4} = 'gcarc';
    values = struct2cell(evsta);
    
   case 'llnl'
    llnl  = SACdata(traceIndex).descrip;
    names  = cell(9,1);
    labels = cell(9,1);
    values = cell(9,1);
    names{1} = 'xminimum';
    names{2} = 'xmaximum';
    names{3} = 'yminimum';
    names{4} = 'ymaximum';
    names{5} = 'norid';
    names{6} = 'nevid';
    names{7} = 'nwfid';
    names{8} = 'nxsize';
    names{9} = 'nysize';
    values = struct2cell(llnl);
    
   case 'response'
    resp  = SACdata(traceIndex).response;
    names  = cell(10,1);
    labels = cell(10,1);
    values = cell(10,1);
    for j=1:10
      names{j} = ['resp' num2str(j-1)];
      if resp(j) ~= -12345
        values{j} = num2str(resp(j));
      end
    end
    
   case 'bboard'
    [M,N] = size(BlackBoard);
    names  = cell(M,1);
    labels = cell(M,1);
    values = cell(M,1);
    for j=1:M
      names{j} = BlackBoard(j).name;
      if isstr(BlackBoard(j).value)
        values{j} = BlackBoard(j).value;
      else
        values{j} = num2str(BlackBoard(j).value);
      end
    end
  end

function deleteInspectWin

  h=findobj('Tag','PropValues');
  if ~isempty(h)
    delete(h)
  end
  h=findobj('Tag','PropLabels');
  if ~isempty(h)
    delete(h)
  end
  h=findobj('Tag','PropNames');
  if ~isempty(h)
    delete(h)
  end
  h=findobj('Tag','CloseFrame');
  if ~isempty(h)
    delete(h)
  end
  hFrame = findobj('Tag','PropWin');
  if ~isempty(hFrame)
    set(hFrame,'visible','off')
  end
  