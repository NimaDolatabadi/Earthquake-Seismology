function RotateToBAZ

global DATAREAD                 % if true, then seismograms have been read into work space.

global RotationCorrection       % Subtract this angle from all computed azimuths
global Axis1Angle		% Angle of first horizontal axis
global Axis2Angle		% Angle of second horizontal axis
global hAXIS_SYMB               % handle to seismometer axes symbol
global hCOMPASS                 % handle to the compass plot



if ~DATAREAD,return,end


hBAZIM = findobj(gcf,'Tag','hBAZIM');
BAZ = str2num(get(hBAZIM,'string'));



NewAxis2Angle = BAZ;
if NewAxis2Angle > 360
   NewAxis2Angle = NewAxis2Angle -360;
end
rotationAngle = Axis2Angle - NewAxis2Angle;
RotationCorrection = RotationCorrection + rotationAngle;
if RotationCorrection > 360
   RotationCorrection = RotationCorrection -360;
end   
DoRotations(rotationAngle);
delete(hAXIS_SYMB)
subplot(hCOMPASS)   
hAXIS_SYMB=mkAxisSymb([0,0],0.7,-RotationCorrection,'b');
   
