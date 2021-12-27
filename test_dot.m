%% Test Fortran MEX file 
% Compute dot product of two vectors
% First, compile the source code using the mex command:
% mex -O -v -R2018a mydot_mex.f90 -output mydot_mex
% This will create the file mydot_mex.mexw64 (on a Windows machine)
% USAGE in Matlab: z = mydot_mex(x,y)
% INPUTS: x,y are vectors
% OUTPUT: z is scalar

clear;clc;close all

xvec = [1,2,3]';
yvec = [4,5,6,7]';

% Result must be 32, confirm it with Matlab function
%res = dot(xvec,yvec);

% Call the MEX function
res1 = mydot_mex(xvec,yvec);

if isequal(res,res1)
    disp('Test passed!')
else
    disp('Wrong result!')
end
