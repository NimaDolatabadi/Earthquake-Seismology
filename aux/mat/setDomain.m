function angle = setDomain(angle)
% Force angle to be between 0 and 360 degrees.
while angle < 0 
   angle = angle +360;
end

while angle > 360
   angle = angle - 360;
end   