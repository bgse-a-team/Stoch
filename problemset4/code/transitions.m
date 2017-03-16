function x = transitions( A, B, C, R, K, N, w, x0 )
%TRANSITIONS computes state transitions for linear-quadratic control
    x = repmat(0, [2 (N+1)]);
    u = repmat(0, [2 N]);

    % initial state
    x(:,1) = x0;

    % compute state transitions
    for k = 1:N
       L = -inv(transpose(B) * K(:,:,k) * B + R) * transpose(B) * K(:,:,k) * A;
       u(:,k) = L * x(:,k);
       x(:,k+1) = A * x(:,k) + B * u(:,k) + w(:,k);
    end
end

