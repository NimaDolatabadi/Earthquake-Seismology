function bh=readbhrc(data)
%the unite is secound nd cm/sec*sec
clear rem N A c e
fid=fopen(data);
n=0;
i=0;
while n==0
    i=i+1;
    f{i}=fgets(fid);
    f1{i}=str2num(f{i});
    
    if feof(fid)==1
        break;
    end
end
if (str2num(f{2}(17)))==1
    
    N=max(f1{15});
else
A=size(f1{15});
A=A(2)-2;
N=f1{15}(A);
end

remm=rem(N,10);
kh=(N-remm)/10;
if remm~=0
    gh=kh+1;
else
    gh=kh;
end
% L comp
accL=f1{28};
for L=29:(27+gh)
    a=f1{L};
    accL=[accL a];  %acceleration value for L comp
end
% V comp
accV=f1{56+gh};
for V=(57+gh):(55+2*gh)
    a2=f1{V};
    accV=[accV a2]; %acceleration value for V comp
end
% T comp
accT=f1{2*gh+84};
for T=(2*gh+85):(i-1)
    a3=f1{T};
    accT=[accT a3];  %acceleration value for T comp
end
%**************************************************************************
Fs=200;
zarib=98;
accL=accL*zarib;
accT=accT*zarib;
accV=accV*zarib;
% convert to cm/sec*sec 
%**************************************************************************
if length(f{9})>11
    late=str2num(f{9}(11:16));
    lone=str2num(f{9}(20:25));
else
    late=-12345;
    lone=-12345;
end
%--------------------------------------------
tm=findstr(f{8},' N');
lats=str2num(f{8}(tm-6:tm));
tm=findstr(f{8},' E');
lons=str2num(f{8}(tm-6:tm));
%--------------------------------------------
if length(f{9})>58
    Mag=str2num(f{9}(58:60));
else
    Mag=-12345;
end
if length(f{9})>34
    dep=str2num(f{9}(34:35));
else
    dep=-12345;
end

t=0:.005:((N-1)*0.005);
[delta,azeqst,azsteq]=delaz(late,lone,lats,lons,0);
if length(f{3})>25
bh.ye=f{3}(15:18);
bh.mon=f{3}(20:21);
bh.day=f{3}(23:24);
bh.hou=f{3}(28:29);
bh.min=(f{3}(31:32));
bh.sec=f{3}(34:35);
bh.ot=[str2num(f{3}(15:18)),str2num(f{3}(20:21)),str2num(f{3}(23:24)),...
    str2num(f{3}(28:29)),str2num(f{3}(31:32)),str2num(f{3}(34:35))];
end



bh.accV=accV;
bh.accL=accL;
bh.accT=accT; 
% 
% 
bh.time=t;
bh.elat=late;
bh.elon=lone;
bh.Mag=Mag;
bh.dep=dep;

bh.slat=lats;
bh.slon=lons;
tm=findstr(f{8},'Station');

bh.stname=f{8}(1:tm-1);
% bh.comp=f{7}(6:7);

bh.Fs=Fs;


bh.dist=delta;
bh.distkm=delta*111.1;
bh.az=azeqst;
bh.baz=azsteq;
%--------------------------------------
tm=findstr(f{8},'Azimuth');
bh.AzL=str2num(f{8}(tm+10:tm+13));
bh.AzT=str2num(f{8}(tm+17:end));
% --------------------------------------

