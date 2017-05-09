function [ C ] = myConv3( H1, H2, A )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

C = myConv2((H1*H2')', A)';

end

