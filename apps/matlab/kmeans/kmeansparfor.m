function exectime = kmeans(inputfile, mufile)

tol = 0.001;

x = load(inputfile);
x = reshape(x, size(x, 1), 3);
m = size(x,1);

% Initialize mu values
%indices = randi(m, k, 1);
%mu = shiftdim(x(indices, :), -1);
mu = load(mufile);
k = size(mu, 1);

oldmu = zeros(size(mu));

tic
iters = 0;

% Run K-means until convergence
distances = zeros(m, k);
while (sum(abs(oldmu - mu)) > tol)
% 	% Update c's to reflect closest mu to each point
%     parfor i=1:m
%         xrep = repmat(x(i,:), k, 1);
%         distances(i,:) = sum((xrep-mu).^2,2);
%     end
    
    parfor j=1:k
        muJrep = repmat(mu(j,:), m, 1);
        distances(:,j) = sum((x-muJrep).^2, 2);
    end
    
%     for j=1:k
%         for i=1:m
%             distances(i, j) = (x(i,:)-mu(j,:))*(x(i,:)-mu(j,:))';
%         end
%     end

%     muExt = repmat(mu, m, 1);
% 	  distances = sum((xExt - muExt).^2, 3);
    [junk, c] = min(distances, [], 2);
   

    
    % non-vectorized version of step 1
	%for i=1:m
	%	mindist = 1000000000; minj = -1;
	%	for j=1:k
	%		dist = (x(i,:)-u(j,:))*(x(i,:)-u(j,:)).';
	%		if (dist < mindist)
	%			minj = j;
	%			mindist = dist;
	%		end
    %    end
	%	c(i) = minj;
	%end;


	% Update mu's to average of their respective points
	oldmu = mu;
	for j = 1:k
		mu(j,:) = sum(x(find(c==j),:))/sum(c==j);
    end
    iters = iters + 1
end

iters
mu
exectime = toc;
