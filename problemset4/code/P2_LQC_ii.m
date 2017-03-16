% ----------------------------------------------------------------------
% Information
% ----------------------------------------------------------------------
%
% 14D006 Stochastic Models and Optimization
%
% (ii) 
% Compare the behavior of the system for two covariance matrices 
% for the disturbances, one ?much larger? than the other, 
% under optimal control (given by discrete-time Riccati equation)
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
R = [2 0; 0 4];
x0 = [1,1];

D1 = [1 0; 0 1];
D2 = [5 0; 0 6];

% ----------------------------------------------------------------------
% Compute discrete-time Riccati equation
% ----------------------------------------------------------------------
K = dtRiccati(A, B, C, R, N);

% ----------------------------------------------------------------------
% Compute state transitions
% ----------------------------------------------------------------------
rng(1000);

% draw disturbances and compute state transitions with covariance matrix D1
w1 = transpose(mvnrnd([0,0],D1,N));
x1 = transitions(A, B, C, R, K, N, w1, x0);

% draw disturbances and compute state transitions with covariance matrix D2
w2 = transpose(mvnrnd([0,0],D2,N));
x2 = transitions(A, B, C, R, K, N, w2, x0);

% ----------------------------------------------------------------------
% Plot state transitions
% ----------------------------------------------------------------------
figure
subplot(2,1,1)       % add first plot in 2 x 1 grid
plot(0:100, x1(1,:), 0:N, x2(1,:));
title('State component 1')

subplot(2,1,2)       % add second plot in 2 x 1 grid
plot(0:100, x1(2,:), 0:N, x2(2,:));
title('State component 2')



