img = imread('test.png');
img = double(img) ./ 255;
fhi = ones(3,3);
fhi(1,2) = -8;
%i = abs(imfilter(img, fhi,'symmetric','same','corr'));
i = abs(conv2(img, fhi,'same'));
imshow(i);