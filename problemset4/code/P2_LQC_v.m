% ----------------------------------------------------------------------
% Information
% ----------------------------------------------------------------------
%
% 14D006 Stochastic Models and Optimization
%
% (v) 
% compare the behavior of the system under optimal control 
% vs. steady-state control (given by the algebraic Riccati equation)
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
D = [1 0; 0 1];
x0 = [1,1];

% ----------------------------------------------------------------------
% Compute discrete-time Riccati equation
% ----------------------------------------------------------------------
K1 = dtRiccati(A, B, C, R, N);

% ----------------------------------------------------------------------
% Compute algebraic Riccati equation
% ----------------------------------------------------------------------
[K,L,G] = dare(A,B,transpose(C)*C,R)
K2 = repmat(0, [2 2 N]);
for k = 1:N
    K2(:,:,k) = K;
end

% ----------------------------------------------------------------------
% Compute state transitions
% ----------------------------------------------------------------------

% draw disturbances with mean 0 and covariance matrix D
rng(1000);
w = transpose(mvnrnd([0,0],D,N));

% compute state transitions under optimal control (K1)
x1 = transitions(A, B, C, R, K1, N, w, x0);

% compute state transitions under steady-state control (K2)
x2 = transitions(A, B, C, R, K2, N, w, x0);

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



