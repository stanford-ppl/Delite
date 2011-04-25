function exectime = kmeansgpu(inputfile, mufile)
addpath /usr/local/jacket/engine

tol = 0.001;

x = load(inputfile);
x = reshape(x, size(x, 1), 1, 3);
m = size(x,1);

% Initialize mu values
%indices = randi(m, k, 1);
%mu = shiftdim(x(indices, :), -1);
mu = load(mufile);
k = size(mu, 1);
mu = shiftdim(mu, -1);

oldmu = zeros(size(mu));

tic

xExt = gdouble(repmat(x, 1, k));

iters = 0;

% Run K-means until convergence
while (sum(abs(oldmu - mu)) > tol)
	% Update c's to reflect closest mu to each point
	muExt = gdouble(repmat(mu, m, 1));
	distances = sum(double((xExt - muExt).^2), 3);
	[junk, c] = min(distances, [], 2);

	% Update mu's to average of their respective points
	oldmu = mu;
	for j = 1:k
		mu(1,j,:) = sum(x(find(c==j),1,:))/sum(c==j);
    end
    iters = iters + 1;
end

iters
exectime = toc;
