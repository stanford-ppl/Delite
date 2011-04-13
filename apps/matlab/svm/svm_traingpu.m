function exectime = svm_traingpu(trainfile, tol)

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

% convert labels
Y = (trainCategory(:) == 1)*(1) + (trainCategory(:) == 0)*(-1);
Y=Y';
% format trainMatrix for SMO algorithm?

% SMO algorithm
tic
[b, alphas] = smo_traingpu(trainMatrix, Y', 1, tol, 10);
exectime = toc;

% compute the weights (assuming a linear kernel)
dim = size(trainMatrix,2);
w = gpuArray(zeros(dim, 1));
for i=1:size(trainMatrix,1)
  w = w + alphas(i)*Y(i)*trainMatrix(i,:)';
end
wc = gather(w);
