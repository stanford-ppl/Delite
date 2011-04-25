function exectime = nb_train(trainfile)

[spmatrix, tokenlist, trainCategory] = readMatrix(trainfile);

trainMatrix = full(spmatrix);
numTrainDocs = size(trainMatrix, 1);
numTokens = size(trainMatrix, 2);

% trainMatrix is now a (numTrainDocs x numTokens) matrix.
% Each row represents a unique document (email).
% The j-th column of the row $i$ represents the number of times the j-th
% token appeared in email $i$. 

% tokenlist is a long string containing the list of all tokens (words).
% These tokens are easily known by position in the file TOKENS_LIST

% trainCategory is a (numTrainDocs x 1) vector containing the true 
% classifications for the documents just read in. The i-th entry gives the 
% correct class for the i-th email (which corresponds to the i-th row in 
% the document word matrix).

% Spam documents are indicated as class 1, and non-spam as class 0.
% Note that for the SVM, you would want to convert these to +1 and -1.

% the multinomial event model Naive Bayes algorithm with Laplace
% smoothing is parameterized by the equations given on the class
% section 2 notes, pg. 14, where:
%
%   x_i_j = the jth word in the ith training example
%   n_i = the number of words in the ith training example
%   y = spam (1) or not spam (0)
%   phi_k_y1 = p(x_j = k | y = 1) (for any j)
%   phi_k_y0 = p(x_j = k | y = 0) (for any j)
%   phy_y = p(y)

tic

dims = size(trainMatrix);
numSamples = dims(1);
numTokens = dims(2);
phi_y1 = zeros(numTokens,1); % size of number of tokens
phi_y0 = zeros(numTokens,1);
num_y1 = zeros(numTokens,1); 
num_y0 = zeros(numTokens,1);
den_y1 = zeros(numTokens,1);
den_y0 = zeros(numTokens,1);
phi_y = 0; % scalar prior probability ( p(y=1) )

% to train the model, we simply need to derive the above parameters
% from the training set

% phi_y
phi_y = sum(trainCategory) / numSamples;

% phi_y1, phi_y0
% calculate phi for each token
% for each token, loop thru all of the training samples
parfor i = 1:numSamples
	x = trainMatrix(i,:); % for convenience
	% get the non-zero entries in x
	[j,windx,numWords] = find(x);
	%n_i = size(windx)(2);
	% for each training sample, count the number of times the
	% kth token appears, and divide by the total number of words
	if (trainCategory(i) == 1)
		num_y1 = num_y1 + x(:);
    den_y1 = den_y1 + sum(numWords);
	else
		num_y0 = num_y0 + x(:);
		den_y0 = den_y0 + sum(numWords);
	end
end

% normalize to laplace smoothing
phi_y1 = (num_y1 + 1) ./ (den_y1 + numTokens);
phi_y0 = (num_y0 + 1) ./ (den_y0 + numTokens);

%end

exectime = toc;
