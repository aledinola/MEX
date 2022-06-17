%% Test Fortran MEX file for linear interpolation
% Author: Alessandro Di Nola. 12/2021.
% Do linear interpolation 
% Yi = myinterp1q(X,Y,Xi)
% similar to Matlab function interp1q
% First, compile the source code using the mex command:
% mex -O -v -R2018a myinterp1q.f90 -output myinterp1q
% This will create the file myinterp1q.mexw64 (on a Windows machine)
% USAGE in Matlab: Yi = myinterp1q(X,Y,Xi)
% INPUTS: X monotonically increasing vector
%         Y matrix with dimension [length(X),ncol]
% OUTPUT: Yi matrix with dimension [length(Xi),ncol]

clear;clc;close all

nx  = 100;
nxi = 1000;
X   = linspace(0.01,5,nx)';
Xi  = linspace(0.01,5,nxi)';
Y   = [exp(X),exp(1.1*X)];

Yi1 = interp1q(X,Y,Xi);

Yi2 = myinterp1q(X,Y,Xi);

if isequal(Yi1,Yi2)
    disp('Test passed!')
else
    disp('Wrong result!')
    err = max(max(abs(Yi1-Yi2)));
    if err<1e-10
        fprintf('But Discrepancy is negligible \n')
    end
    fprintf('Discrepancy = %f \n',err)
end

figure
plot(X,Y,'o',Xi,Yi1)
title('interp1q')

figure
plot(X,Y,'o',Xi,Yi2)
title('myinterp1q')

