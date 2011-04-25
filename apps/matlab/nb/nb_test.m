
[spmatrix, tokenlist, category] = readMatrix('MATRIX.TEST');

testMatrix = full(spmatrix);
numTestDocs = size(testMatrix, 1);
numTokens = size(testMatrix, 2);

% Assume classify.m has just been executed, and all the parameters computed/needed
% by your classifier are in memory through that execution. You can also assume 
% that the columns in the test set are arranged in exactly the same way as for the
% training set (i.e., the j-th column represents the same token in the test data 
% matrix as in the original training data matrix).

% Write code below to classify each document in the test set (ie, each row
% in the current document word matrix) as 1 for SPAM and 0 for NON-SPAM.

% Construct the (numTestDocs x 1) vector 'output' such that the i-th entry 
% of this vector is the predicted class (1/0) for the i-th  email (i-th row 
% in testMatrix) in the test set.
output = zeros(numTestDocs, 1);

%---------------
% from bayes formula:
% p(y=1|x) = prod(i=1->n){p(x_i|y=1)}*p(y=1)
%            --------------------------------------------------------
%       prod(i=1->n){p(x_i|y=1)}*p(y=1) + prod(i=1->n){p(x_i|y=0)}*p(y=0)
%

% the parameters of the model that we have already computed are:
%   phi_y1(k) = p(x_j = k | y = 1) (for any j)
%   phi_y0(k) = p(x_j = k | y = 0) (for any j)
%   phi_y = p(y)

% rewriting p(y=1|x) in terms of our model parameters, we have:
%   p(y=1|x) = prod(k=1->n){phi_k_y1(k)}*phy_y
%        ------------------------------------------------------------
%       prod(k=1->n){phi_k_y1}*phy_y + prod(k=1->n){phi_k_y0}*(1-phy_y)

for i = 1:numTestDocs
	X = testMatrix(i,:); 
	test = find(X);
	n_i = size(test)(2);
	% calculate the (log) probability that x(i) is spam
	% note that these are not real probabilities, because we realize
	% that both have the denominator p(x) in bayes formula, and
	% so we can ignore it. instead, we just compare the log of
	% the numerators.
	p_spam = 0; p_norm = 0;
	for j = 1:n_i
		%(test(j) = token in email, by index
		%X(test(j)) = # of times word test(j) appears in this email
		p_spam = p_spam + X(test(j))*log(phi_y1(test(j))) + log(phi_y);
		p_norm = p_norm + X(test(j))*log(phi_y0(test(j))) + log(1-phi_y);
	end;
    
	if ( p_spam > p_norm )
		output(i) = 1;
	end;
end

% track the five most indicative spam words
% first col is their log probability indicator (not actual probability)
% second col is the index of this word
spam_words = zeros(5,2);
spam_words(:,1) = log(spam_words(:,1)); % initialize to -inf
 
% search for the five most indicative tokens
for k=1:numTokens 
	% similarity measure given in (a) of the question:
	sim = log(phi_y1(k)) - log(phi_y0(k));
	[sm, i_sm] = min(spam_words(:,1));
	if (sim > sm) 
		spam_words(i_sm,1) = sim;
		spam_words(i_sm,2) = k;
	end
end

% print out the five most indicative tokens
for i = 1:5
	j = spam_words(i,2);
	j
end

%---------------


% Compute the error on the test set
error=0;
for i=1:numTestDocs
  if (category(i) ~= output(i))
    error=error+1;
  end
end

%Print out the classification error on the test set
error/numTestDocs


