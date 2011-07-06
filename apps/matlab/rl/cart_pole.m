function [new_x, new_x_dot, new_theta, new_theta_dot] = cart_pole(action, x, x_dot, theta, theta_dot)

% Parameters for simulation dynamics
GRAVITY = 9.8;
MASSCART = 1.0;
MASSPOLE = 0.3;
TOTAL_MASS = (MASSPOLE + MASSCART);
LENGTH = 0.7;    %  actually half the pole's length
POLEMASS_LENGTH = (MASSPOLE * LENGTH);
FORCE_MAG = 10.0; % WAS 10.0
TAU = 0.02;    %  seconds between state updates 
FOURTHIRDS = 1.3333333333333;

% Noise parameters
action_flip_prob = 0.00;
force_noise_factor = 0.0; % multiplied by between 1-.. and 1+.. 
no_control_prob = 0.00; % Force is 0 with this probability

action = action - 1;

% Flip action with action_flip_prob
if (rand(1)<action_flip_prob)
  action = 1 - action;
end
  
if (action>0)
  force=FORCE_MAG;
else
  force=-FORCE_MAG;
end

force = force * (1 - force_noise_factor + rand(1) * 2*force_noise_factor);

if (rand(1)<no_control_prob)
  force=0;
end

costheta = cos(theta);
sintheta = sin(theta);

temp = (force + POLEMASS_LENGTH * theta_dot * theta_dot * sintheta) / TOTAL_MASS;

thetaacc = (GRAVITY * sintheta - costheta * temp) / (LENGTH * (FOURTHIRDS ...
						  - MASSPOLE * costheta * costheta / TOTAL_MASS));

xacc  = temp - POLEMASS_LENGTH * thetaacc* costheta / TOTAL_MASS;

% Return the new state variables (using Euler's method)

new_x  = x + TAU * x_dot;
new_x_dot = x_dot + TAU * xacc;
new_theta = theta + TAU * theta_dot;
new_theta_dot = theta_dot + TAU * thetaacc;
