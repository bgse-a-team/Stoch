% ----------------------------------------------------------------------
% Information
% ----------------------------------------------------------------------
%
% 14D006 Stochastic Models and Optimization
%
% (iv) 
% Compare the behavior of the system for two input-cost matrices, 
% one "much larger" than the other, under optimal control
%
% (Authors) Daniel Bestard Delgado, Michael Cameron, 
%           Hans-Peter Höllwirth, Akhil Lohia
% (Date)    03.2017

% ----------------------------------------------------------------------
% Common setup
% ----------------------------------------------------------------------
N = 100;

A = [1 2; 3 2];
B = [4 1; 2 3];
C = [1 3];

% ----------------------------------------------------------------------
% Case specific setup
% ----------------------------------------------------------------------
D = [1 0; 0 1];
x0 = [1,1];

R1 = [2 0; 0 4];
R2 = [18 0; 0 22];

% ----------------------------------------------------------------------
% Compute discrete-time Riccati equation
% ----------------------------------------------------------------------
K = dtRiccati(A, B, C, R, N);

% ----------------------------------------------------------------------
% Compute state transitions
% ----------------------------------------------------------------------

% draw disturbances with mean 0 and covariance matrix D
rng(1000);
w = transpose(mvnrnd([0,0],D,N));

% compute state transitions with input-cost matrix R1
x1 = transitions(A, B, C, R1, K, N, w, x0);

% compute state transitions with input-cost matrix R2
x2 = transitions(A, B, C, R2, K, N, w, x0);

% ----------------------------------------------------------------------
% Plot state transitions
% ----------------------------------------------------------------------
figure
subplot(2,1,1)       % add first plot in 2 x 1 grid
plot(0:N, x1(1,:), 0:N, x2(1,:));
title('State component 1')

subplot(2,1,2)       % add second plot in 2 x 1 grid
plot(0:N, x1(2,:), 0:N, x2(2,:));
title('State component 2')



