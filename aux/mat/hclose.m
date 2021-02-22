function hclose(num)
% This function is called to close the figure with handle hFIG(num)


global hFIG			% array of handles to figures
if hFIG(num) > 0
   close(hFIG(num))
   hFIG(num)=0;
end
