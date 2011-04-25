function [b, alphas] = smo_train(X, Y, C, tol, max_passes)

% Runs the SMO algorithm.  X is the matrix of training examples.  Each row is
% a training example, and the jth column holds the jth feature.  Y is a column
% matrix containing 1 for positive examples and -1 for negative examples.  C
% is the standard SVM regularization parameter.  tol is a tolerance value used
% for determining equality of floating point numbers. 

% initialization
dims = size(Y);
numSamples = dims(1);
alphas = zeros(numSamples,1);
b = 0; % initial b
passes = 0;

while(passes<max_passes)
	num_changed_alphas = 0;
	for i = 1:numSamples
		f_i = sum(gather(alphas.*gpuArray(Y).*(X*X(i,:)')))+b;
		E_i = f_i - Y(i);
		if (((Y(i)*E_i < -1*tol) && (alphas(i) < C)) || ((Y(i)*E_i > tol) && (alphas(i) > 0))) 
			% select a candidate j from the remaining numSamples-i samples at random
			j = floor(rand*(numSamples-1))+1;
			while (j == i)
				j = floor(rand*(numSamples-1))+1;
			end
		 
			f_j = sum(gather(alphas.*gpuArray(Y).*(X*X(j,:)')))+b;
			E_j = f_j - Y(j);
			old_aj = alphas(j);
			old_ai = alphas(i);

			% calculate bounds L and H that must hold in order for a_i, alphas(j) to
			% satisfy constraints and check
			if (Y(i) ~= Y(j))
				L = max(0, alphas(j) - alphas(i));
				H = min(C, C + alphas(j) - alphas(i));
			else
				L = max(0, alphas(i) + alphas(j) - C);
				H = min(C, alphas(i) + alphas(j));
			end;
			if (L == H) continue; end;

			% calculate eta
			eta = 2.*X(i,:)*X(j,:)' - X(i,:)*X(i,:)' - X(j,:)*X(j,:)';
			% check eta
			if (eta >= 0) continue; end;

			% compute new alphas(j)

			alphas(j) = alphas(j) - Y(j)*(E_i-E_j)/eta;
			% clip alphas(j) if necessary
			if (alphas(j) > H) alphas(j) = H;
			elseif (alphas(j) < L) alphas(j) = L;
			end;
			% check alphas(j) convergence
			if (abs(alphas(j) - old_aj) < tol) continue; end;

			% find a_i to maximize objective function
			old_ai = alphas(i);
			alphas(i) = alphas(i) + Y(i)*Y(j)*(old_aj-alphas(j));

			% compute the new b such that KKT conditions are satisfied
			old_b = b;
			b1 = b - E_i - Y(i)*(alphas(i)-old_ai).*X(i,:)*X(i,:)' - Y(j)*(alphas(j)-old_aj).*X(i,:)*X(j,:)';
			b2 = b - E_j - Y(i)*(alphas(i)-old_ai).*X(i,:)*X(j,:)' - Y(j)*(alphas(j)-old_aj).*X(j,:)*X(j,:)';
			if ((alphas(i) > 0) && (alphas(i) < C))
				b = b1;
			end;
			if ((alphas(j) > 0) && (alphas(j) < C))
				b = b2;
			end;
			if (old_b == b) % neither threshold valid
				b = (b1+b2)/2;
			end;

			num_changed_alphas = num_changed_alphas+1;
		end;
	end;
	if (num_changed_alphas == 0)
		passes=passes+1;
	else
		passes=0;
	end;
end;
