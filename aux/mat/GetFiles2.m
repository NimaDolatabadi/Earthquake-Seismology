function GetFiles2
% Loads the current 3-component set of SAC files for analysis.

global CurrentSet		% Index of current 3-comp set being analyzed
global TSeisData		% Array of seismograms passed in from SAC
global THeaderData		% Array of header data passed in from SAC

global t			% time for all 3 components

global yn			% data for North component
global dtn			% delta T for North component
global datan			% header variables for North component

global ye			% data for East component
global dte			% delta T for East component
global datae			% header variables for East component

global yz			% data for Vert component
global dtz			% delta T for Vert component
global dataz			% header variables for Vert component

global RotationCorrection	% Subtract this angle from all computed azimuths


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

[Y,In]=min(abs(tn-t1));
yn=yn(In:In+Nsamp-1);

[Y,Ie]=min(abs(te-t1));
ye=ye(Ie:Ie+Nsamp-1);


RotationCorrection = THeaderData(24,1);

