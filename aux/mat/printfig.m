function printfig(handle)
% Set up a dialog to control printing the figure pointed to by handle.
% The dialog allows selection of a physical printer, printing to postscript,
% or printing to Illustrator 88  format. Selection is implemented using
% "radio-button" style controls arranged to allow only an exclusive selection.
% That is, selection of one option cancels any other selected option.

global hFIG			% array of handles to figures
global PRINTFIGURE		% handle of figure to print

Width=290;
Height=200;
ScreenSize = get(0,'ScreenSize');
if ~ isstr(handle)		% Calling to set up window...
   PRINTFIGURE=handle;
   makeFigure(6,[(ScreenSize(3)-Width)/2 (ScreenSize(4)-Height)/2 Width Height],'Print Figure');
   h6=hFIG(6);
   set(h6,'Resize','off')
   frame=uicontrol('style','frame','position',[0 0 Width+1 Height+1]);
   rBtn=uicontrol('style','radiobutton','position',[10 Height-30 120 20],'string','Printer','value',1);
   set(rBtn,'Tag','Printer','callback','printfig(''ChkPrinter'')')

   hEdit=uicontrol('style','edit','position',[160 Height-30 100 20],'string','Default');
   set(hEdit,'BackgroundColor',[1 1 1],'Tag','PrinterName')

   rBtn=uicontrol('style','radiobutton','position',[10 Height-70 120 20],'string','Illustrator File','value',0);
   set(rBtn,'Tag','Illustrator','callback','printfig(''ChkIllust'')')

   rBtn=uicontrol('style','radiobutton','position',[10 Height-110 120 20],'string','Postscript File','value',0);
   set(rBtn,'Tag','Postscript','callback','printfig(''ChkPostScr'')')

   Btn=uicontrol('style','pushbutton','position',[30 Height-170 90 20],'string','Print');
   set(Btn,'Tag','PrintBtn','callback','printfig(''DoPrint'')')

   Btn=uicontrol('style','pushbutton','position',[150 Height-170 90 20],'string','Close');
   set(Btn,'Tag','ClosePrintBtn','callback','printfig(''CancelPrint'')')

elseif strcmp('ChkPrinter',handle)
   h=findobj(hFIG(6),'Tag','Printer');
   set(h,'value',1)
   h=findobj(hFIG(6),'Tag','PrinterName');
   set(h,'enable','on')
   
   h=findobj(hFIG(6),'Tag','Illustrator');
   set(h,'value',0)
   h=findobj(hFIG(6),'Tag','Postscript');
   set(h,'value',0)
   
elseif strcmp('ChkIllust',handle)
   h=findobj(hFIG(6),'Tag','Printer');
   set(h,'value',0)
   h=findobj(hFIG(6),'Tag','PrinterName');
   set(h,'enable','off')
   
   h=findobj(hFIG(6),'Tag','Illustrator');
   set(h,'value',1)
   h=findobj(hFIG(6),'Tag','Postscript');
   set(h,'value',0)
   
   
elseif strcmp('ChkPostScr',handle)
   h=findobj(hFIG(6),'Tag','Printer');
   set(h,'value',0)
   h=findobj(hFIG(6),'Tag','PrinterName');
   set(h,'enable','off')
   
   h=findobj(hFIG(6),'Tag','Illustrator');
   set(h,'value',0)
   h=findobj(hFIG(6),'Tag','Postscript');
   set(h,'value',1)
   
elseif strcmp('DoPrint',handle)

   % handle case where printing to installed printer...
   h=findobj(hFIG(6),'Tag','Printer');
   if get(h,'value')
      h=findobj(hFIG(6),'Tag','PrinterName');
      if strcmp(get(h,'string'),'Default')
         PrintCmd='print -dpsc';
      else  
         PrintCmd=['print -dpsc -P' get(h,'string')];
      end
   end

   % handle case where printing Illustrator file...
   h=findobj(hFIG(6),'Tag','Illustrator');
   if get(h,'value')
      [Illfile, Path] = uiputfile('*.ai', 'Save As');
      if isempty(Illfile)
         hclose(6)
         return
      elseif ~isstr(Illfile)
         hclose(6)
         return
      else
         PrintCmd=['print -dpsc ' Path Illfile];
      end
   end


   % handle case where printing Postscript file...
   h=findobj(hFIG(6),'Tag','Postscript');
   if get(h,'value')
      [PSfile, Path] = uiputfile('*.ps', 'Save As');
      if isempty(PSfile)
         hclose(6)
         return
      elseif ~isstr(PSfile)
         hclose(6)
         return
      else
         PrintCmd=['print -dpsc ' Path PSfile];
      end
   end

   % make sure the figure is still open...
   h=findobj(0);
   ii=find(h==PRINTFIGURE);
   if isempty(ii)
      hclose(6)
      return
   end
   
   figure(PRINTFIGURE)
   set(gcf,'inverthardcopy','on')
   eval(PrintCmd)
   hclose(6)
   return
   
elseif strcmp('CancelPrint',handle)
   hclose(6)
end   
