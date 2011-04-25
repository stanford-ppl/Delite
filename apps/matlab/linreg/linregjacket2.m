function exectime = linreg(xfile, yfile)
addpath /usr/local/jacket/engine

% CS 229 Problem Set 1
% Arvind Sujeeth
% developed and tested on octave 3.0.0 in ubuntu 8.04

% problem 2 

% (d.i)
% unweighted linear regression using the normal equations to minimize
% J(theta). This value of theta = inv(X.'X)*X.'*y
% the resulting fitted line is given by the equation
%   h(x) = theta_0 + theta_1*x_1

% the input vector x and output vector y are stored in these files
q2x = load(xfile);
q2y = load(yfile);

tic

dims = size(q2x);
m = dims(1);
n = dims(2)+1;
% initialize x and theta vector with the intercept term.
% (by convention, x_0 = 1.)
x = [ones(m,1),q2x];

xstep = 25/dims(1);
xref = -10:xstep:14.99; % reasonable sampling range for this data with
                        % same number of points as the input vector q2x
xref = [ones(dims(1),1),xref.'];

% (d.ii)
% locally weighted linear regression using the normal equations to minimize
% J(theta). This value of theta = inv(x.'*W*x)*x.'*W*q2y;

% initialize the W matrix based on the hand calculated results
% and the given formula, w_i = exp(-(x-x_i)^2/2*tau^2)

tau = 10;

weights = zeros(dims(1),1);

x=gdouble(x);

% for every query point, ALL of the weights must be re-calculated
dims_ref=size(xref);
guess = gzeros(dims_ref(1), 1);
xref = gdouble(xref);
i=[];
for i=1:dims_ref(1)
	for j=1:dims(1)    
        temp = (xref(i,2)-q2x(j));
		weights(j) = exp(-1*temp*temp/(2*tau^2));
	end
	% we can vectorize this only in the special case that
	% dims_ref = dims (num query pts = num sample pts)
	%weights = exp(-1*(xref(i,2)-q2x(:)).^2./(2*tau^2));
	W = diag(1/2.*weights);
	% directly calculate theta using the weighted normal equations
	t1 = x.'*W;
	theta = inv(t1*x)*t1*q2y;
	guess(i) = theta.'*xref(i,:).';
end

guess(1)
guess(end)

exectime = toc;
