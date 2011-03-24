load mix.dat	% load mixed sources

anneal = [0.1 0.1 0.1 0.05 0.05 0.05 0.02 0.02 0.01 0.01 ...
      0.005 0.005 0.002 0.002 0.001 0.001];

% Initialize parameters
n = size(mix, 2);
m = size(mix, 1);
W = eye(n);

% Iterate through the annealing schedule
tic
for iter=1:length(anneal)
    iter
   % Randomly interate through the samples running stochastic gradient descent
   rowIndices = randperm(m);
   for i = 1:length(rowIndices)
      rowIndex = rowIndices(i);
      % Perform the ICA stochastic gradient descent update
      W = W + anneal(iter) * ((ones(n,1)-2*ones(n,1)./(ones(n,1)+exp(-W*mix(rowIndex,:)')))*mix(rowIndex,:) + (W')^-1);
   end
end;
toc