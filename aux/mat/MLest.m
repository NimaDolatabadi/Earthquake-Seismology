function P=MLest(x,y,z,azim,incid,model,Fn,Rotmatrix)
% This function calculates the misfit between the rotated data covariance
% matrix and the model matrix for the vectors x,y,z.
% S is the covariance matrix of the signal
S=cov([x,y,z]);
PHI=norm(S);
S=S/PHI;
N=length(x);
M=length(azim);



Fp=zeros(M,1);

% Loop through azimuths
for k=1:M
   index=(k-1)*3 +1;
   R=Rotmatrix(:,index:index+2);
   C=R*S*R';
   Fp(k)=getMisFit(incid,C,model);
end
Pp=1-chi2cdf(Fp*(N-1),3);
Pd=chi2cdf((N-1)*(Fn-Fp),2);
P=Pp.*Pd;




function p = chi2cdf(x,v)
%CHI2CDF Chi-square cumulative distribution function.
%	P = CHI2CDF(X,V) returns the chi-square cumulative distribution
%	function with V degrees of freedom at the values in X.
%	The chi-square density function with V degrees of freedom,
%	is the same as a gamma density function with parameters V/2 and 2.
%
%	The size of P is the common size of X and V. A scalar input   
%	functions as a constant matrix of the same size as the other input.	 

%	References:
%	   [1]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%	   Functions", Government Printing Office, 1964, 26.4.

%	Copyright (c) 1993 by The MathWorks, Inc.
%	$Revision: 1.1.1.1 $  $Date: 2005/07/27 18:55:03 $

if   nargin < 2, 
    error('Requires two input arguments.');
end

%[errorcode x v] = distchck(2,x,v);
if length(v) ~= length(x)
   v=ones(size(x))*v(1);
end
% Call the gamma distribution function.

p = gamcdf(x,v/2,2);

% Return NaN if the degrees of freedom is not a positive integer.
k = find(v < 0  |  round(v) ~= v);
if any(k)
    p(k) = NaN * ones(size(k));
end




function p = gamcdf(x,a,b);
%GAMCDF Gamma cumulative distribution function.
%       P = GAMCDF(X,A,B) returns the gamma cumulative distribution
%       function with parameters A and B at the values in X.
%
%       The size of P is the common size of the input arguments. A scalar input  
%       functions as a constant matrix of the same size as the other inputs.     
%
%       Some references refer to the gamma distribution with a single
%       parameter. This corresponds to the default of B = 1. 
%
%       GAMMAINC does computational work.

%       References:
%          [1]  L. Devroye, "Non-Uniform Random Variate Generation", 
%          Springer-Verlag, 1986. p. 401.
%          [2]  M. Abramowitz and I. A. Stegun, "Handbook of Mathematical
%          Functions", Government Printing Office, 1964, 26.1.32.

%       Copyright (c) 1993 by The MathWorks, Inc.
%       $Revision: 1.1.1.1 $  $Date: 2005/07/27 18:55:03 $

if nargin < 3, 
    b = 1; 
end

if nargin < 2, 
    error('Requires at least two input arguments.'); 
end

%[errorcode x a b] = distchck(3,x,a,b);
if length(b) ~= length(x)
   b=ones(size(x))*b(1);
end
%if errorcode > 0
%    error('The arguments must be the same size or be scalars.');
%end

%   Return NaN if the arguments are outside their respective limits.
k1 = find(a <= 0 | b <= 0);     
if any(k1)
    p(k1) = NaN * ones(size(k1));
end

% Initialize P to zero.
p = zeros(size(x));

k = find(x > 0 & ~(a <= 0 | b <= 0));
if any(k), 
    p(k) = gammainc(x(k) ./ b(k),a(k));
end

% Make sure that round-off errors never make P greater than 1.
k = find(p > 1);
if any(k)
    p(k) = ones(size(k));
end
