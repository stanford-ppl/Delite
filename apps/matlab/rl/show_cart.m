% This function displays the "animation"
function [] = show_cart(x, x_dot, theta, theta_dot, pause_time)

set(gcf,'DoubleBuffer','on');

length = 3;

plotx(1) = x;
ploty(1) = 0;

%plotx(2) = x + length * cos(theta + pi/2.0);
%ploty(2) = length * sin(theta + pi/2.0);

plotx(2) = x + length * sin(theta);
ploty(2) = length * cos(theta);

plot(plotx, ploty); 
rectangle('Position', [x-0.4, -0.25, 0.8, 0.25], 'FaceColor', 'cyan');
rectangle('Position', [x-0.01, -0.5, 0.02, 0.25], 'FaceColor', 'r');
axis([-3 3 -0.5 3.5]);

drawnow;
pause(pause_time);

