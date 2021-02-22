function Fp=getMisFit(incid,C,model)
% This function is called during the ML calculations to determine the misfit between
% the current model and the actual data covariance matrix. The misfit is calculated
% as a sum of squares of the difference between the two matrices.

Fp=1.0e+34;

for j=1:length(incid)
   index=(j-1)*3 +1;
   sig=model(:,index:index+2);
   DIFF=sig-C;
   DIFF=DIFF.*DIFF;
   F=sum(sum(DIFF));
   if F < Fp
      Fp=F;
   end
end
