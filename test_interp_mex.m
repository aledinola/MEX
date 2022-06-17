% Author: Alessandro Di Nola. 12/2021.
clear
clc
close all

% Compile with mex -O -v -R2018a myinterp1q.f90 -output myinterp1q
% or, if you want to modify optimization falgs, with 
% mex -v OPTIMFLAGS="/O3 /DNDEBUG" -R2018a myinterp1q.f90 -output myinterp1q
% or even more aggressive
% mex -v OPTIMFLAGS="/O3 /Qprec-div- /QxHost /DNDEBUG" -R2018a myinterp1q.f90 -output myinterp1q
% USAGE in Matlab: yi = myinterp1q(x,y,xi)

% Prepare test data
nx  = 100;
nxi = nx*100;

%% Test with equally spaced points
x_grid = linspace(1e-3,5,nx)';
y_grid = exp(x_grid);
xi     = linspace(1e-3,5,nxi)';

% Run Matlab interp1q
tic
yi1     = interp1q(x_grid,y_grid,xi);
time=toc;
fprintf('Matlab interp1q runs in %f seconds \n',time)

% Run my MEX routine
tic
yi2     = myinterp1q(x_grid,y_grid,xi);
time=toc;
fprintf('My interp1q (MEX) runs in %f seconds \n',time)

% Check if results are identical
dist = max(abs(yi1-yi2));
if dist>1e-12
    disp(dist)
    warning('interp1q and myinterp1q give different results')
end

figure
plot(x_grid,y_grid,'-o','linewidth',2)
hold on
plot(xi,yi2,'linewidth',2)
grid on
title('Testing myinterp1q, equally spaced grid')

%% Redo same test with *unequally* spaced points

x_grid = nonlinspace(1e-3,5,nx,1.2);
y_grid = exp(x_grid);
xi     = nonlinspace(1e-3,5,nxi,1.2);

% Run Matlab interp1q
tic
yi1     = interp1q(x_grid,y_grid,xi);
time=toc;
fprintf('Matlab interp1q runs in %f seconds \n',time)

% Run my MEX routine
tic
yi2     = myinterp1q(x_grid,y_grid,xi);
time=toc;
fprintf('My interp1q (MEX) runs in %f seconds \n',time)

% Check if results are identical
dist = max(abs(yi1-yi2));
if dist>1e-12
    disp(dist)
    warning('interp1q and myinterp1q give different results')
end

figure
plot(x_grid,y_grid,'-o','linewidth',2)
hold on
plot(xi,yi2,'linewidth',2)
grid on
title('Testing myinterp1q, unequally spaced grid')

function x = nonlinspace(lo,hi,n,phi)
% Author: Alessandro Di Nola. 05/2021.
    % Build an unequally spaced grid.
    % phi > 1 -> more mass at the lower end of the grid.
    
    x      = zeros(n,1);
    x(1) = lo;
    for i = 2:n
        x(i) = x(i-1) + (hi-x(i-1))/((n-i+1)^phi);
    end
    
end
