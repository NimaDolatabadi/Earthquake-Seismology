function GetFiles
% Loads the current 3-component set of SAC files for analysis.
% The working data arrays are filled and the data is plotted.

global CurrentSet		% Index of current 3-comp set being analyzed
global NumSets			% number of current 3-comp sets
global TSeisData		% Array of seismograms passed in from SAC
global THeaderData		% Array of header data passed in from SAC
global TMatlabString		% Array of header strings passed in from SAC

global t			% time for all 3 components

global yn			% data for North component
global dtn			% delta T for North component
global datan			% header variables for North component
global stringdatan		% string data for North component

global ye			% data for East component
global dte			% delta T for East component
global datae			% header variables for East component
global stringdatae		% string data for East component

global yz			% data for Vert component
global dtz			% delta T for Vert component
global dataz			% header variables for Vert component
global stringdataz		% string data for Vert component

global ynback			% un-modified copy of the north trace
global yeback			% un-modified copy of the east trace
global yzback			% un-modified copy of the vert trace
global HCORNER			% filter high corner
global EVDATA			% string data describing seismograms
global hActivePick		% handle to currently active pick
global NumZpicks		% number of picks on Z channel
global NumEpicks		% number of picks on E channel
global NumNpicks		% number of picks on N channel
global hZpick			% array of handles to Z pick markers
global hEpick			% array of handles to E pick markers
global hNpick			% array of handles to N pick markers
global RotationCorrection	% Subtract this angle from all computed azimuths
global Axis1Angle		% Angle of first horizontal axis
global Axis2Angle		% Angle of second horizontal axis

global MLazimuth		% azimuth of P-wave from ML estimator
global MLprob 			% associated probability of P-wave
global MLwidth 			% width of zone with <= 10% probability drop

STRGLEN=136;			% length of string passed from SAC for each channel

EVDATA=setstr(zeros(6,25));
% First get East component

index=(CurrentSet-1)*3+1;
NPTS=THeaderData(2,index);
dte=THeaderData(1,index);
B=THeaderData(3,index);
E=(NPTS-1)*dte+B;
te=linspace(B,E,NPTS);
te=te';
ye=TSeisData(1:NPTS,index);
datae=zeros(110,1);
datae(1)=dte;
datae(80)=NPTS;
datae(6)=B;
datae(9)=THeaderData(4,index);
datae(41)=THeaderData(5,index);
datae(42)=THeaderData(6,index);
for j=1:10
   datae(10+j)=THeaderData(6+j,index);
end
for j=1:6
   datae(70+j)=THeaderData(16+j,index);
end
stringdatae=setstr(ones(1,192)*real(' '));

strgStart=(CurrentSet-1)*3*STRGLEN + 1;
stringdatae(1:8)=TMatlabString(strgStart:strgStart+7);
stringdatae(41:48)=TMatlabString(strgStart+24:strgStart+31);
stringdatae(49:128)=TMatlabString(strgStart+48:strgStart+127);
stringdatae(137:152)=TMatlabString(strgStart+32:strgStart+47);
stringdatae(161:176)=TMatlabString(strgStart+8:strgStart+23);
Axis1Angle = THeaderData(23,1);
MLazimuth=THeaderData(26,index);
MLprob=THeaderData(27,index);
MLwidth=THeaderData(28,index);

IDstring=[stringdatae(1:8) ' comp az =: ' num2str(round(Axis1Angle))];
EVDATA(4,1:length(IDstring))=IDstring;
EVDATA(3,1:20)=sprintf('%4i %3i %2i:%2i:%5.2f',datae(71),datae(72),datae(73)...
           ,datae(74),datae(75)+datae(76)/1000);
ii=findstr(EVDATA(3,10:20),' ');
for j=1:length(ii)
   EVDATA(3,9+ii(j))='0';
end



% Then get North component
index=CurrentSet*3 - 1;
NPTS=THeaderData(2,index);
dtn=THeaderData(1,index);
B=THeaderData(3,index);
E=(NPTS-1)*dtn+B;
tn=linspace(B,E,NPTS);
tn=tn';
yn=TSeisData(1:NPTS,index);
datan=zeros(110,1);
datan(1)=dtn;
datan(80)=NPTS;
datan(6)=B;
datan(9)=THeaderData(4,index);			%A
datan(41)=THeaderData(5,index);			%USER0
datan(42)=THeaderData(6,index);			%USER1
for j=1:10					% T0 - T9
   datan(10+j)=THeaderData(6+j,index);
end
for j=1:6					% year,jday,hour,min,sec,msec
   datan(70+j)=THeaderData(16+j,index);
end
stringdatan=setstr(ones(1,192)*real(' '));
strgStart=CurrentSet*3*STRGLEN -2*STRGLEN + 1;

stringdatan(1:8)=TMatlabString(strgStart:strgStart+7);
stringdatan(41:48)=TMatlabString(strgStart+24:strgStart+31);
stringdatan(49:128)=TMatlabString(strgStart+48:strgStart+127);
stringdatan(137:152)=TMatlabString(strgStart+32:strgStart+47);
stringdatan(161:176)=TMatlabString(strgStart+8:strgStart+23);

Axis2Angle = THeaderData(23,2);
IDstring=[stringdatae(1:8) ' comp az =: ' num2str(round(Axis2Angle))];
EVDATA(6,1:length(IDstring))=IDstring;
EVDATA(5,1:20)=sprintf('%4i %3i %2i:%2i:%5.2f',datan(71),datan(72),datan(73)...
           ,datan(74),datan(75)+datan(76)/1000);
ii=findstr(EVDATA(5,10:20),' ');
for j=1:length(ii)
   EVDATA(5,9+ii(j))='0';
end

% Finally get Vert component
index=CurrentSet*3 ;
NPTS=THeaderData(2,index);
dtz=THeaderData(1,index);
B=THeaderData(3,index);
E=(NPTS-1)*dtz+B;
tz=linspace(B,E,NPTS);
tz=tz';
yz=TSeisData(1:NPTS,index);
dataz=zeros(110,1);
dataz(1)=dtz;
dataz(80)=NPTS;
dataz(6)=B;
dataz(9)=THeaderData(4,index);
dataz(41)=THeaderData(5,index);
dataz(42)=THeaderData(6,index);
for j=1:10
   dataz(10+j)=THeaderData(6+j,index);
end
for j=1:6
   dataz(70+j)=THeaderData(16+j,index);
end
stringdataz=setstr(ones(1,192)*real(' '));
strgStart=CurrentSet*3*STRGLEN -STRGLEN + 1;

stringdataz(1:8)=TMatlabString(strgStart:strgStart+7);
stringdataz(41:48)=TMatlabString(strgStart+24:strgStart+31);
stringdataz(49:128)=TMatlabString(strgStart+48:strgStart+127);
stringdataz(137:152)=TMatlabString(strgStart+32:strgStart+47);
stringdataz(161:176)=TMatlabString(strgStart+8:strgStart+23);

EVDATA(2,1:22)=[stringdataz(1:8)  ' Vertical Comp'];
EVDATA(1,1:20)=sprintf('%4i %3i %2i:%2i:%5.2f',dataz(71),dataz(72),dataz(73)...
           ,dataz(74),dataz(75)+dataz(76)/1000);
ii=findstr(EVDATA(1,10:20),' ');
for j=1:length(ii)
   EVDATA(1,9+ii(j))='0';
end
if HCORNER > 0.8/2/dtn
   HCORNER=0.8/2/dtn;
end   
yz=yz-mean(yz);
yn=yn-mean(yn);
ye=ye-mean(ye);



% Trim the seismograms to be the same length and with common begin time
t1=max([min(te) min(tn) min(tz)]);
t2=min([max(te) max(tn) max(tz)]);
Nsamp=round((t2-t1)/dte+1);
t2=t1+(Nsamp-1)*dte;
t=linspace(t1,t2,Nsamp);
t=t';
[Y,Iz]=min(abs(tz-t1));
yz=yz(Iz:Iz+Nsamp-1);
yzback=yz;

[Y,In]=min(abs(tn-t1));
yn=yn(In:In+Nsamp-1);
ynback=yn;

[Y,Ie]=min(abs(te-t1));
ye=ye(Ie:Ie+Nsamp-1);
yeback=ye;

hActivePick=[];	
NumZpicks=0;	
NumEpicks=0;	
NumNpicks=0;	
hZpick=[];	
hEpick=[];	
hNpick=[];	

hStrg=findobj(gcf,'Tag','GroupInfo');
set(hStrg,'string',['Group ' num2str(CurrentSet) ' of ' num2str(NumSets) ' sets'])
RotationCorrection = THeaderData(24,1);
plotmain;
