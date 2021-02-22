function SaveResults
% This function is called to write pick information, BAZ information,
% and incidence angle information into the 3 current SAC file headers.
% Picks are written into the T and KT variables. Azimuth and incidence
% are written into USER0,USER1 and KUSER0, KUSER1 variables.


global CurrentSet		% Index of current 3-comp set being analyzed
global NumSets			% number of current 3-comp sets
global TSeisData		% Array of seismograms passed in from SAC
global THeaderData		% Array of header data passed in from SAC
global TMatlabString		% Array of header strings passed in from SAC

global datan			% header variables for North component
global stringdatan		% string data for North component

global datae			% header variables for East component
global stringdatae		% string data for East component

global dataz			% header variables for Vert component
global stringdataz		% string data for Vert component

global NumZpicks                % number of picks on Z channel
global NumEpicks                % number of picks on E channel
global NumNpicks                % number of picks on N channel
global hZpick                   % array of handles to Z pick markers
global hEpick                   % array of handles to E pick markers
global hNpick                   % array of handles to N pick markers
global Axis1Angle		% Angle of first horizontal axis
global Axis2Angle		% Angle of second horizontal axis
global ynback                   % un-modified copy of the north trace
global yeback                   % un-modified copy of the east trace


global MLazimuth		% azimuth of P-wave from ML estimator
global MLprob 			% associated probability of P-wave
global MLwidth 			% width of zone with <= 10% probability drop

STRGLEN=136;			% length of string passed from SAC for each channel

UndefinedNum=-12345;
UndefinedStrg='-12345  ';

% Update the polarization information...
hELLIP=findobj(gcf,'Tag','hELLIP');
ellip=str2num(get(hELLIP,'string'));



% First do the Z-picks
% Start by clearing out the T and Kt variables
dataz(11:20)=ones(size(dataz(11:20)))*UndefinedNum;
for j=1:10
   index=(j-1)*8+49;
   stringdataz(index:index+7)=UndefinedStrg;
end

k=0;
for j=1:NumZpicks
   line=hZpick(1,j);
   txt=hZpick(2,j);
   xdata=get(line,'xdata');
   string=get(txt,'string');
   index=k*8+49;
   stringdataz(index:index+7)='        ';
   stringdataz(index:index+length(string)-1)=string;
   dataz(11+k)=xdata(1);
   k=k+1;
end



% Now add the back azimuth and incidence angles to user0 and user1
hBAZIM=findobj(gcf,'Tag','hBAZIM');
hINCID=findobj(gcf,'Tag','hINCID');
dataz(41)=str2num(get(hBAZIM,'string'));
dataz(42)=str2num(get(hINCID,'string'));
stringdataz(137:144)='BackAzim';
stringdataz(145:152)='IncidAng';

index=CurrentSet*3 ;
THeaderData(4,index)=dataz(9);
THeaderData(5,index)=dataz(41);
THeaderData(6,index)=dataz(42);
for j=1:10
   index=CurrentSet*3 ;
   THeaderData(6+j,index)=dataz(10+j);
end
for j=1:6
   THeaderData(16+j,index)=dataz(70+j);
end
THeaderData(25,index)=ellip;
THeaderData(26,index)=MLazimuth;
THeaderData(27,index)=MLprob;
THeaderData(28,index)=MLwidth;

strgStart=CurrentSet*3*STRGLEN -STRGLEN + 1;
TMatlabString(strgStart+24:strgStart+31)=stringdataz(41:48);
TMatlabString(strgStart+48:strgStart+127)=stringdataz(49:128);
TMatlabString(strgStart+32:strgStart+47)=stringdataz(137:152);
TMatlabString(strgStart+128:strgStart+134)='POLARIZ';




% Now do the E-picks
% Start by clearing out the T and Kt variables
datae(11:20)=ones(size(datae(11:20)))*UndefinedNum;
for j=1:10
   index=(j-1)*8+49;
   stringdatae(index:index+7)=UndefinedStrg;
end

k=0;
for j=1:NumEpicks
   line=hEpick(1,j);
   txt=hEpick(2,j);
   xdata=get(line,'xdata');
   string=get(txt,'string');
   index=k*8+49;
   stringdatae(index:index+7)='        ';
   stringdatae(index:index+length(string)-1)=string;
   datae(11+k)=xdata(1);
   k=k+1;
end
% Now add the back azimuth and incidence angles to user0 and user1
hBAZIM=findobj(gcf,'Tag','hBAZIM');
hINCID=findobj(gcf,'Tag','hINCID');
datae(41)=str2num(get(hBAZIM,'string'));
datae(42)=str2num(get(hINCID,'string'));
stringdatae(137:144)='BackAzim';
stringdatae(145:152)='IncidAng';
index=(CurrentSet-1)*3+1;
NPTS=THeaderData(2,index);
TSeisData(1:NPTS,index) = yeback;
THeaderData(4,index)=datae(9);
THeaderData(5,index)=datae(41);
THeaderData(6,index)=datae(42);
for j=1:10
   THeaderData(6+j,index)=datae(10+j);
end
for j=1:6
   THeaderData(16+j,index)=datae(70+j);
end
THeaderData(23,1) = Axis1Angle;
THeaderData(25,index)=ellip;
THeaderData(26,index)=MLazimuth;
THeaderData(27,index)=MLprob;
THeaderData(28,index)=MLwidth;

strgStart=(CurrentSet-1)*3*STRGLEN + 1;

TMatlabString(strgStart:strgStart+7)=stringdatae(1:8);
TMatlabString(strgStart+24:strgStart+31)=stringdatae(41:48);
TMatlabString(strgStart+48:strgStart+127)=stringdatae(49:128);
TMatlabString(strgStart+32:strgStart+47)=stringdatae(137:152);
TMatlabString(strgStart+8:strgStart+23)=stringdatae(161:176);
TMatlabString(strgStart+128:strgStart+134)='POLARIZ';





% Now do the N-picks
% Start by clearing out the T and Kt variables
datan(11:20)=ones(size(datan(11:20)))*UndefinedNum;
for j=1:10
   index=(j-1)*8+49;
   stringdatan(index:index+7)=UndefinedStrg;
end

k=0;
for j=1:NumNpicks
   line=hNpick(1,j);
   txt=hNpick(2,j);
   xdata=get(line,'xdata');
   string=get(txt,'string');
   index=k*8+49;
   stringdatan(index:index+7)='        ';
   stringdatan(index:index+length(string)-1)=string;
   datan(11+k)=xdata(1);
   k=k+1;
end
% Now add the back azimuth and incidence angles to user0 and user1
hBAZIM=findobj(gcf,'Tag','hBAZIM');
hINCID=findobj(gcf,'Tag','hINCID');
datan(41)=str2num(get(hBAZIM,'string'));
datan(42)=str2num(get(hINCID,'string'));
stringdatan(137:144)='BackAzim';
stringdatan(145:152)='IncidAng';
index=CurrentSet*3 - 1;
NPTS=THeaderData(2,index);
TSeisData(1:NPTS,index) = ynback;

THeaderData(4,index)=datan(9);
THeaderData(5,index)=datan(41);
THeaderData(6,index)=datan(42);
for j=1:10
   THeaderData(6+j,index)=datan(10+j);
end
for j=1:6
   THeaderData(16+j,index)=datan(70+j);
end
THeaderData(23,2) = Axis2Angle;
THeaderData(25,index)=ellip;
THeaderData(26,index)=MLazimuth;
THeaderData(27,index)=MLprob;
THeaderData(28,index)=MLwidth;

strgStart=CurrentSet*3*STRGLEN -2*STRGLEN + 1;

TMatlabString(strgStart:strgStart+7)=stringdatan(1:8);
TMatlabString(strgStart+24:strgStart+31)=stringdatan(41:48);
TMatlabString(strgStart+48:strgStart+127)=stringdatan(49:128);
TMatlabString(strgStart+32:strgStart+47)=stringdatan(137:152);
TMatlabString(strgStart+8:strgStart+23)=stringdatan(161:176);



TMatlabString(strgStart+128:strgStart+134)='POLARIZ';
