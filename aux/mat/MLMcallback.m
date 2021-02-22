function MLMcallback(action)
% This function processes callbacks from GUI controls on the MLM window.

global hFIG			% array of handles to figures
h=findobj(gcf,'Tag','BAZplot');
if isempty(h),return,end
if strcmp(action,'colormap')
   h=findobj(hFIG(4),'Tag','ColorMap');
   value=get(h,'value');
   string=get(h,'string');
   map=string(value,:);
   eval(['colormap ' map])
   
elseif strcmp(action,'shading')
   h=findobj(hFIG(4),'Tag','Shading');
   value=get(h,'value');
   string=get(h,'string');
   type=string(value,:);
   ii=findstr(type,' ');
   type(ii)=[];
   h=findobj(gcf,'Tag','BAZplot');
   setshade(type,h,gcf)


elseif strcmp(action,'print')
   printfig(hFIG(4))
end



function setshade(type,ax,fig)
% This is a slightly hacked version of the shading.m file distributed
% by Matlab. It explicitly imports the axis handle and figure handle
% rather than attempting to use the current. This allows an arbitrary
% figure and axis to be operated on using a control in any figure.



kids = get(ax,'Children');
fc = get(ax,'color');
if strcmp(lower(fc),'none')
	fc = get(fig,'color');
end
imesh = [];
isurf = [];
itext = [];
for i = 1:max(size(kids))
    t = lower(get(kids(i),'type'));
	if(strcmp(t, 'surface') | strcmp(t, 'patch'))
		face = get(kids(i),'facecolor');
		if isstr(face), face = lower(face); end
		if strcmp(face,'none')
			imesh = [imesh ; kids(i)];
		elseif strcmp(face,'texturemap')
			itext = [itext; kids(i)];
		elseif ~isstr(face)
			if (all(face == fc)) 
				imesh = [imesh ; kids(i)];
			else
				isurf = [isurf; kids(i)];
			end
		else
			isurf = [isurf; kids(i)];
		end
	end
end

if(nargin == 0)
	ret_type = get(ax,'DefaultSurfaceFaceColor');
	if(strcmp(ret_type, 'flat'))
		if(strcmp(get(ax,'DefaultSurfaceEdgeColor'),'black'))
			ret_type = 'faceted';
		end
	end
else
	if(strcmp(type, 'flat'))
		if ~isempty(isurf),set(isurf,'facecolor','flat','edgecolor','none');end
		if ~isempty(imesh),set(imesh,'facecolor',fc,'edgecolor','flat');end
		if ~isempty(itext),set(itext,'edgecolor','none');end
	elseif(strcmp(type, 'interp'))
		if ~isempty(isurf),set(isurf,'facecolor','interp','edgecolor','none');end
		if ~isempty(imesh),set(imesh,'facecolor',fc,'edgecolor','interp');end
		if ~isempty(itext),set(itext,'edgecolor','interp');end
	elseif(strcmp(type,'faceted'))
		if ~isempty(isurf),set(isurf,'facecolor','flat','edgecolor','black');end
		if ~isempty(imesh),set(imesh,'facecolor',fc,'edgecolor','flat');end
		if ~isempty(itext),set(itext,'edgecolor','black');end
	else
		error('Shading methods are flat, faceted, and interp.');
	end
end



