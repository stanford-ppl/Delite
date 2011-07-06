% CS 229 Problem Set 1
% Arvind Sujeeth
% developed and tested on octave 3.0.0 in ubuntu 8.04

% problem 1 (b)
% Newton-Ralphson algorithm for optimizing l(theta)
% where l(theta) =
%    sum from i to m{ y_i * log(h(x_i)) + (1-y_i)*log(1-h(x_i))}
% and h(z) = 1/(1 + exp(-z))

clear;

% the input vector x and output vector y are stored in these files
load 'q1x.dat'
load 'q1y.dat'

% to maximize the log likelihood l(theta), we can use newton's method
% with f(theta) = l'(theta), generalized to vectors, yielding the update rule:
% theta = theta - H^-1(gradient(l(theta)))

% from the hand solutions to problem 1,
%  l'(theta)_l   = sum from i to m{x_il*(y_i-h(x_i))}
%  l''(theta)_kl = sum from i to m{-x_il*x_ik*h(x_i)*(1-h(x_i))}

% initial guess
dims = size(q1x);
% add the intercept term
x = [ones(dims(1),1),q1x];
old_theta = zeros(dims(2)+1,1);
theta = old_theta;
dtheta=zeros(dims(2)+1,1);
ddtheta=zeros(dims(2)+1);

% stop iterating when each theta estimate is within .001 of the previous
%done = sum(theta - old_theta < .001);
done = 0; ll=0; oldll = 1000;
iter=0;
while (!done)
	%old_theta = theta;
    
	% hyp is the hypothesis function with the vector input x (of features)
	% given by hyp(x) = 1 / (1 + exp^(transpose(theta)*x));
	% here, we pre-calculate each n-dimensional sample (from 1 to m) 
	hyp_v = zeros(dims(1),1);
	for i = 1:dims(1)
		hyp_v(i) = 1 / (1 + exp(-1*theta.'*x(i,:).'));
    end
 	
	% calculate the log likelihood to test for convergence
    ll = sum(q1y.*log2(hyp_v) + (1-q1y).*log2(1-hyp_v));

    for l=1:3
    	% calculate l'(theta)
    	dtheta(l) = sum( x(:,l).*(q1y(:)-hyp_v) );  
    	for k=1:3
    		% calculate l''(theta)
    		ddtheta(k,l) = sum( -1*x(:,l).*x(:,k).*hyp_v.*(1-hyp_v) );
    	end
    end

	% update theta according to the update rule
    theta = theta - inv(ddtheta)*dtheta;
	%[theta]
    
	done = (abs(ll - oldll) < .000001);
	oldll = ll;
	% done = sum(abs(theta - old_theta) < .000000001);
	iter += 1;
end

[iter]
[ll, oldll]
[theta]

%scatter(q2x,q2y);
% label the data points as either o's (if the label had a 0) or x's (1)
scatter(q1x(~q1y,1),q1x(~q1y,2),'o');
hold on
scatter(q1x((q1y==1),1),q1x((q1y==1),2),'+');
% the line is given by the equation h(x) = theta_0 + theta_1*x_1 + theta_2*x_2
% where h(x) = 0.5
xref=[0,10];
y = (-1.*log2(1)-theta(1)-theta(2).*xref)./theta(3);
plot(xref,y);
axis([0,10,-5,5]);
hold off
title('P.1.c Training data and decision boundary');
