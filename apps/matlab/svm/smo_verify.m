function [w, b, alphas] = smo_verify(X, Y, C, tol, max_passes)

% runs smo_train, + plots Xs, decision boundary in first 2 dimensions

% Use a default training matrix if one isn't specified.
if(~exist('X'))
     X = [1 1; 1 0 ; 0 1; 2 2; 2 3];
     Y = [1; 1; 1; -1; -1];
     C = 1;
     tol = .01;
     max_passes = 10;
end


% Run SMO to find the hyperplane
[b, alphas] = smo_train(X, Y, C, tol, max_passes);
b
alphas

% compute the weights (assuming a linear kernel)
w = zeros(2,1);
for i=1:size(X,1),
  w = w + alphas(i)*Y(i)*X(i,1:2)';
end


     
N=size(X,1);
     
figure
     
hold on;

% Plot each of the points.  Support vectors are crosses, non support vectors
% are circles.  Points in the positive class are green, points in the negative
% class are red.

for i=1:N
     
  if(Y(i)>0 & alphas(i)<=tol) % non support vector, positive sample

    plot([X(i,1) X(i,1)] ,[X(i,2) X(i,2)],'go'); % bug in octave

  elseif(Y(i)>0 & alphas(i)>tol) % support vector, pos sample

    plot([X(i,1) X(i,1)] ,[X(i,2) X(i,2)],'gx'); % bug in octave

  elseif(alphas(i)<=tol) % non sv, neg sample

    plot([X(i,1) X(i,1)] ,[X(i,2) X(i,2)],'ro'); % bug in octave

  else % sv, neg sample

    plot([X(i,1) X(i,1)] ,[X(i,2) X(i,2)],'rx'); % bug in octave

  end

end


% Plot separating hyperplane.  Note: we assume a linear kernel.

% Find the range for the training set.
maxX = max(max(abs(X(:,1:2))));
x1 = -maxX:tol:maxX;

% Plot the hyperplane through the positive support vectors.

x2 = (-w(1)*x1-b+1)/w(2);

plot(x1,x2,'g--');

% Plot the hyperplane through the negative support vectors.

x2 = (-w(1)*x1-b-1)/w(2);

plot(x1,x2,'r--');

% Plot the separating hyperplane.

x2 = (-w(1)*x1-b)/w(2);

plot(x1,x2,'b--');

axis([-maxX, maxX, -maxX, maxX]);
