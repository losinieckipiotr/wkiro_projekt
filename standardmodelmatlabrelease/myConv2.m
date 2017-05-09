function [ C ] = myConv2( A, B, shape)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here


%   C = CONV2(A, B) performs the 2-D convolution of matrices A and B.
%   If [ma,na] = size(A), [mb,nb] = size(B), and [mc,nc] = size(C), then
%   mc = max([ma+mb-1,ma,mb]) and nc = max([na+nb-1,na,nb]).

[ma,na] = size(A);
[mb,nb] = size(B);
mc = max([ma+mb-1,ma,mb]);
nc = max([na+nb-1,na,nb]);
 
j_i = 1:mc;
k_i = 1:nc;

if ((nargin == 3) && strcmp(shape, 'same'))
    %x index
    s_x = 1;
    e_x = mc;%out x size
    l_x = e_x - s_x + 1;%calc x length
    o_x = size(A,1);
    while true
        if (l_x == o_x)
            break
        end
        s_x = s_x + 1; %cut top edge
        l_x = e_x - s_x + 1;
        if (l_x == o_x)
            break
        end
        e_x = e_x - 1;%cut bottom edge
        l_x = e_x - s_x + 1;
    end
    %y index
    s_y = 1;
    e_y = nc;%out y size
    l_y = e_y - s_y + 1;%calc y length
    o_y = size(A,2);
    while true
        if (l_y == o_y)
            break
        end
        s_y = s_y + 1; %cut left edge
        l_y = e_y - s_y + 1;
        if (l_y == o_y)
            break
        end
        e_y = e_y - 1;%cut right edge
        l_y = e_y - s_y + 1;
    end
    j_i = s_x:e_x;
    k_i = s_y:e_y;
end

s = [mc nc];
a_x = size(A,1);
a_y = size(A,2);
b_x = size(B,1);
b_y = size(B,2);
C = zeros(s(1),s(2));

for j = 1:s(1)
    for k = 1:s(2)
        for p = 1:a_x
            for q = 1:a_y
                i_x = j-p+1;
                i_y = k-q+1;
                if (i_x > 0) && (i_y > 0) &&(i_x <= b_x) && (i_y <= b_y)
                    C(j,k) = C(j,k) + A(p,q)*B(i_x,i_y);
                end
            end
        end
    end
end
C=C(j_i, k_i);

end

