function [  ] = test_image_rotate ( imageFile, rad )
   file = imread(imageFile);
   rotated = imrotate(file, radtodeg(rad) * -1, 'bilinear');
   imwrite(rotated, 'output/test_image_rotate.pgm');
end

