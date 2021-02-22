function CalcMLM
% compute the degree of polarization of the signal contained in vectors
% yn,ye,yz using a sliding window of length PPMWINLENGTH
% This function is called when the "Calculate" button in the "Polarization"
% GUI window is clicked.

global hFIG			% array of handles to figures
global hSLIDERWIN		% handle to progress slider control
global hSLIDERDONE		% handle to done fraction
global hSLIDERTOTAL		% handle to total fraction of slider control
global X_MLM			% array of x values for MLM analysis
global Y_MLM			% array of y values for MLM analysis
global Z_MLM			% array of z values for MLM analysis
global T_MLM			% array of t values for MLM analysis
global WAVETYPE			% index to wavetype to model

global MLazimuth		% azimuth of P-wave from ML estimator
global MLprob 			% associated probability of P-wave
global MLwidth 			% width of zone with <= 10% probability drop

dt=T_MLM(2)-T_MLM(1);
N=length(X_MLM);

% initialize a progress indicator tool
SetUpSlider
done=get(hSLIDERDONE,'position');
done(3)=done(1);
set(hSLIDERDONE,'position',done);
set(hSLIDERWIN,'visible','on');
total=get(hSLIDERTOTAL,'position');
totalLength=total(3)-total(1);


% Get the values of the parameters controlling the estimation from
% their respective controls, validating the parameters as they are 
% obtained. Any bad parameters are reset to default values and
% the control updated.

% First WinLen...
h=findobj(hFIG(4),'Tag','WinLen');
value=str2num(get(h,'string'));
if value/dt < 15
   value=15*dt;
   set(h,'string',num2str(value))
elseif value/dt > N
   value=N*dt;
   set(h,'string',num2str(value))
end
winlen=fix(value/dt);   

% Now time step...
h=findobj(hFIG(4),'Tag','TimeStep');
value=str2num(get(h,'string'));
if fix(value/dt) < 1
   value=dt;
   set(h,'string',num2str(value))
elseif value/dt > N
   value=N*dt;
   set(h,'string',num2str(value))
end
step=fix(value/dt);   


% Now the azimuth step...
h=findobj(hFIG(4),'Tag','AzimStep');
value=str2num(get(h,'string'));
if fix(value) < 1
   value=1;
   set(h,'string',num2str(value))
end
azimstep=fix(value);

% Now incidence start ...
h=findobj(hFIG(4),'Tag','IncidStrt');
value=str2num(get(h,'string'));
if fix(value) < 5
   value=5;
   set(h,'string',num2str(value))
end
incidstart=fix(value);   

% Now incidence step ...
h=findobj(hFIG(4),'Tag','IncidStep');
value=str2num(get(h,'string'));
if fix(value) < 1
   value=1;
   set(h,'string',num2str(value))
elseif value > 10
   value=10;
   set(h,'string',num2str(value))
end
incidstep=fix(value);   

% Now incidence stop ...
h=findobj(hFIG(4),'Tag','IncidStop');
value=str2num(get(h,'string'));
if fix(value) < incidstart
   value=incidstart;
   set(h,'string',num2str(value))
elseif value > 90
   value=90;
   set(h,'string',num2str(value))
end
incidstop=fix(value);   

start=1;
endd=winlen;
azim=0:azimstep:360;
incid=incidstart:incidstep:incidstop;
Result=[];
T=[];


% Get the number of steps for slider updating...
j=0;
while endd < length(X_MLM)
   j=j+1;
   start=start+step;
   endd=endd+step;
end
NumSteps=j;

% precalculate the model matrix for all incidence angles,
% and the rotation matrices for all azimuths
model=getModelMatrix(incid);	% signal covariance model
Rotmatrix=getRotationMatrix(azim);

% noise model and related terms 
M=length(azim);
Noise=randn(100,3);
SIGn=cov(Noise);
Fn=ones(M,1)*getMisFit(incid,SIGn,model);


% and do the calculation...
start=1;
endd=winlen;
j=0;
window = hanning(winlen);
while endd < length(X_MLM)
   j=j+1;
   xx=X_MLM(start:endd).*window;
   yy=Y_MLM(start:endd).*window;
   zz=Z_MLM(start:endd).*window;
   tt=T_MLM(start:endd);
   T(j)=mean(tt);
   Pp=MLest(xx,yy,zz,azim,incid,model,Fn,Rotmatrix);
   Result(:,j)=Pp;
   start=start+step;
   endd=endd+step;
   fraction=j/NumSteps;
   done(3)=fraction*totalLength+done(1);
   set(hSLIDERDONE,'position',done);
   drawnow
end
hclose(3);
NumTimeSteps = j;


% Now do the plot...
figure(hFIG(4))
h=findobj(gcf,'Tag','BAZplot');
if ~ isempty(h),delete(h),end
h=findobj(gcf,'Tag','BAZSplot');
if ~ isempty(h),delete(h),end
pcolor(T,azim,Result)
pos=get(gca,'position');
pos(4)=.6;
set(gca,'position',pos,'xcolor','k','ycolor','k','Tag','BAZplot')
MLMcallback('shading')
MLMcallback('colormap')

if WAVETYPE == 1
   title('P-wave probability')
elseif WAVETYPE == 2
   title('SH-wave probability')
end

ylabel('Azimuth')
xlabel('time (s)')
h=get(gca,'xlabel');
set(h,'color','k')
h=get(gca,'ylabel');
set(h,'color','k')
h=get(gca,'title');
set(h,'color','k')



% Now plot seismogram after scaling so its P-P amplitude is 100
% with a mean of 50
range=max(Z_MLM)-min(Z_MLM);
seis=Z_MLM/range*100+50;
line(T_MLM,seis,'color','b')
if WAVETYPE > 1, return,end



% Now get the parameters of a 1-D marginal distribution along a line
% of constant T where T is the time of the first global min or max for
% the time segment being analyzed.
[Ymax,Imax] = max(Z_MLM);
[Ymin,Imin] = min(Z_MLM);
cutIndex = min([Imax,Imin]);
tmax = T_MLM(cutIndex);
[Tmax, Imax] = min(abs( T - tmax) );

PixelRange = 1;
minCol = Imax - PixelRange;
maxCol = Imax + PixelRange;
if minCol < 1,minCol = 1;end
if maxCol > NumTimeSteps, maxCol = NumTimeSteps;end
temp = Result(:,minCol:maxCol);

[colMaxVal, rowNum] = max(temp);
[Y,I] = max(colMaxVal);
Imax = minCol + I -1;
marginal = Result(:,Imax);
tmax = T(Imax);
hCutLine = line([tmax tmax], [0 360],'color','g');
set(hCutLine,'linestyle','--')



azimInterp = linspace(0,360,2000);
marginalInterp = interp1(azim,marginal,azimInterp,'spline');
 
MaxMarginalVal = max( marginalInterp );
ii = find(marginalInterp >= 0.9 * MaxMarginalVal);
if ii >= 2
   minAzim = azimInterp(ii(1));
   maxAzim = azimInterp( ii( length(ii) ) );
   [margMax, jj] = max(marginalInterp);
   MostLikelyAzim = azimInterp(jj);
else
   disp('Could not find a range of azimuths with P-wave probability > 0.9')
end
MLazimuth = MostLikelyAzim;
MLprob =  margMax;
MLwidth = maxAzim - minAzim;
         
hMaxLine = line( [min(T_MLM), max(T_MLM)], [ MostLikelyAzim MostLikelyAzim ],...
           'color','g','linestyle','--');
maxProb = num2str(margMax);
maxProbAzim = num2str(MostLikelyAzim);           
txtString = ['Peak Probability of ',maxProb,' at azimuth ',maxProbAzim]; 
TxtStart =  tmax + 0.2 *(max(T) - min(T)) ;        
htxt1 = text(TxtStart, MostLikelyAzim + 5,txtString,'color','g','fontsize',8);
           
hLBndLine = line( [min(T_MLM), max(T_MLM)], [ minAzim, minAzim ],...
           'color','r','linestyle','-.');
           
Prob = num2str(0.9 * margMax);
ProbAzim = num2str(minAzim);           
txtString = ['Probability of ',Prob,' at azimuth ',ProbAzim]; 
TxtStart =  tmax + 0.2 *(max(T) - min(T)) ;        
htxt1 = text(TxtStart, minAzim - 5,txtString,'color','r','fontsize',8);

           
hUBndLine = line( [min(T_MLM), max(T_MLM)], [ maxAzim maxAzim ],...
           'color','r','linestyle','-.');
Prob = num2str(0.9 * margMax);
ProbAzim = num2str(maxAzim);           
txtString = ['Probability of ',Prob,' at azimuth ',ProbAzim]; 
TxtStart =  tmax + 0.2 *(max(T) - min(T)) ;        
htxt1 = text(TxtStart, maxAzim + 5,txtString,'color','r','fontsize',8);


function matrix=getRotationMatrix(azim)
% This function constructs a master rotation matrix which is a concatenation
% of the rotation matrices for the entire rande of azimuths to be tested.
% By pre-computing the rotation matrices in this manner, considerable computation
% time is saved.
global RotationCorrection       % Subtract this angle from all computed azimuths


M=length(azim);
matrix=zeros(3,M);
% Loop through azimuths
for k=1:M
   index=(k-1)*3 +1;
   th=(azim(k) + RotationCorrection -90)*pi/180;        %th is the rotation angle relative to x-axis
   cs=cos(th);
   sn=sin(th);
   matrix(:,index:index+2)=[cs -sn 0 ; sn cs 0; 0 0 1];
end



function matrix=getModelMatrix(incid)
% This function constructs a set of model matrices for the current
% wavetype and current range of incidence angles.


global WAVETYPE                 % index to wavetype to model

incid=incid*pi/180;
M=length(incid);
matrix=zeros(3,3*M);
for j=1:M
   cs=cos(incid(j));
   sn=sin(incid(j));
   index=(j-1)*3+1;
   if WAVETYPE ==1
      sig=[sn*sn 0 -cs*sn;0 0 0;-cs*sn 0 cs*cs];
   elseif WAVETYPE == 2
      sig=[0 0 0;0 1 0;0 0 0];
   end
   matrix(:,index:index+2)=sig;
end




function SetUpSlider
% This function is called at the beginning of the ML calculation. Because
% that calculation can be quite lengthy it is necessary to display some kind
% of progress information. This control displays progress using a
% thermometer-like device. At the beginning of the calculation the slider is
% all blue. As calculations are completed the blue area is covered with red
% until at the conclusion of calculations, the slider is all red. The slider
% is implemented as a figure with a frame enclosing two colored frames; one
% blue and one red. This function sets up these controls.

 
global hFIG                     % array of handles to figures
global hSLIDERWIN               % handle to progress slider control
global hSLIDERDONE              % handle to done fraction
global hSLIDERTOTAL             % handle to total fraction of slider control

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
