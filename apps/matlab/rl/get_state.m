% This function returns a discretized value (a number) for a continuous
% state vector. Currently x is divided into 3 "boxes", x_dot into 3,
% theta into 6 and theta_dot into 3. A finer discretization produces a
% larger state space, but allows a better policy.
function [state] = get_state(x, x_dot, theta, theta_dot)

% Parameters for state discretization in get_state
one_degree = 0.0174532;	% 2pi/360
six_degrees = 0.1047192;
twelve_degrees = 0.2094384;
fifty_degrees = 0.87266;

total_states=163;

state=0;

if (x < -2.4 || x > 2.4  || theta < -twelve_degrees || theta > twelve_degrees)
  state=total_states-1; % to signal failure
else
  if (x < -1.5)
    state = 0;
  elseif (x < 1.5)
    state = 1;
  else		  
    state = 2;
  end
  
  if (x_dot < -0.5) 		       
    ;
  elseif (x_dot < 0.5)
    state = state + 3;
  else
    state = state + 6;
  end
     
  if (theta < -six_degrees) 	       
    ;
  elseif (theta < -one_degree)
    state = state + 9;
  elseif (theta < 0)
    state = state + 18;
  elseif (theta < one_degree)
    state = state + 27;
  elseif (theta < six_degrees)
    state = state + 36;
  else
    state = state + 45;
  end
  
  if (theta_dot < -fifty_degrees)
    ;
  elseif (theta_dot < fifty_degrees)  
    state = state + 54;
  else
    state = state + 108;
  end
  
end

state=state+1;
