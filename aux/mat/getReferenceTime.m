function refTime = getReferenceTime(set,reference)

global THeaderData		% Array of header data passed in from SAC

index=(set-1)*3+1;

if reference == 1
   refTime = THeaderData(4,index);
else
   refTime = THeaderData(5 + reference,index);
end 