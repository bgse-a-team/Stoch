% ----------------------------------------------------------------------
% Information
% ----------------------------------------------------------------------
%
% 14D006 Stochastic Models and Optimization
%
% (Authors) Daniel Bestard Delgado, Michael Cameron, 
%           Hans-Peter Höllwirth, Akhil Lohia
% (Date)    03.2017

function K = dtRiccati(A, B, C, R, N)
% discrete-time Riccati equation
    K = repmat(0, [2 2 N]);

    K(:,:,N) = transpose(C)*C;
    for k = (N-1):-1:1
        K(:,:,k) = transpose(A) * (K(:,:,(k+1)) - K(:,:,(k+1)) * B * inv(transpose(B) * K(:,:,(k+1)) * B + R) * transpose(B) * K(:,:,(k+1))) * A + transpose(C)*C;
    end
end

