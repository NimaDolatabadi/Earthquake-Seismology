clc
clear all
close all
addpath('/home/habib/Desktop/Inst_seismology_97_98/BHRC_data/bhrc');
cd('/home/habib/Desktop/Inst_seismology_97_98/BHRC_data/bhrc/Bushehr.NPPBhrc/');
%----------------------------------------plot kardane f    ile sac
station=dir('2*');
for i=1:length(station)
    cd(station(i).name);
    v1_data=dir('*.V1');
    for ii=1:length(v1_data)
        rec=readbhrc(v1_data(ii).name);
        %-------------------------
%            ts=1/rec.Fs;
%             wave=rec.accV;
%            N=length(wave);
%             t=0:ts:((N-1)*ts);
%          
%            figure(i)
%         
%         
%           plot(t,wave)
% %            title(['station=',station(ii).name,'record=',v1_data(i).name]);
%           xlabel('time sec');ylabel('amp cm/sec')
%         Fs=1/ts;
    
%         end
%         cd..
%         end
        %-------------------------------------------------------------------
        year=rec.ot(1);
        mon=rec.ot(2);
        day=rec.ot(3);
         jday=juliandate(year,mon,day)-juliandate(year,1,1)+1;
        rec_sac=sacempty;
        rec_sac.an=year;
         rec_sac.jr=jday;
        rec_sac.hr=rec.ot(4);
        rec_sac.mn=rec.ot(5);
        rec_sac.sec=fix(rec.ot(6));
        rec_sac.msec=0;
        rec_sac.evt0=0;
        rec_sac.pointe=length(rec.accV);
        rec_sac.npts=length(rec.accV);
        rec_sac.evname=rec.stname;
        rec_sac.trace=rec.accV;
        rec_sac.tau=1/rec.Fs;
        name_out=[v1_data(ii).name,'.V.sac'];
        rec_sac.kcomp='BHZ'; 
        Fs=200;
        %+++++++++++++++++++++++++++++++++++++++++++
%         sptool;
%         pause;
%         tp=r.x1;
%         ts=r.x2;
%         rec_sac.t0=tp;
%         rec_sac.t1=ts;
        %+++++++++++++++++++++++++++++++++++++++++++
         writesac(rec_sac,name_out,'pc');
        
        rec_sac.trace=rec.accT;
        rec_sac.kcomp='BHE';
        name_out=[v1_data(ii).name,'.T.sac'];
        writesac(rec_sac,name_out,'pc');
        
        rec_sac.trace=rec.accL;
        name_out=[v1_data(ii).name,'.L.sac'];
        rec_sac.kcomp='BHN';
        writesac(rec_sac,name_out,'pc');
        disp(name_out);
        %-------------------------
    end
    
    cd ..
end