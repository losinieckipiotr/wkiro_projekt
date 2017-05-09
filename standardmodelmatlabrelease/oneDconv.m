X = [-9 0; -3 4; -1 -4];
Y = [-4 -1; -2 1];
%X = [1 2 3; 4 5 6; 7 8 9]
%Y = [-1 -2 -1; 0 0 0; 1 2 1]

[m, n] = size(X);
[k, l] = size(Y);
z_y = m+k-1;
z_x = n + l - 1;
v_x = zeros(z_y, z_x);
v_y = zeros(z_y, z_x);
l_x = (n - 1)*(m + k - 1) + m;
l_y = (l - 1)*(m + k - 1) + k;
l_z = l_x + l_y - 1;

v_x(1:m,1:n) = X(1:end,1:end)
v_y(1:k,1:l) = Y(1:end,1:end)

v_x = v_x(:)'
v_y = v_y(:)'

v_x = v_x(1:l_x)
v_y = v_y(1:l_y)

v_z = conv(v_x, v_y)
Z = reshape(v_z, z_y, z_x);

