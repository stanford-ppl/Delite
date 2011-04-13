function [matrix, tokenlist, category] = readMatrix(filename)

fid = fopen(filename);

%Read the header line
headerline = fgetl(fid);

%Read number of documents and tokens
rowscols = fscanf(fid, '%d %d\n', 2);

%Read the list of tokens - just a long string!
blah = fscanf(fid, '%s', 1); % required for octave
tokenlist = fgetl(fid);

% Document word matrix
% Each row represents a document (mail)
% Each column represents a distinct token
% The (i,j)-th element represents the number of times token j appeared in
% document i
matrix = sparse(1, 1, 0, rowscols(2), rowscols(1)); % the transpose!

% Vector containing the categories corresponding to each row in the
% document word matrix
% The i-th component is 1 if the i-th document (row) in the document word
% matrix is SPAM, and 0 otherwise.
category = matrix(rowscols(1));

%Read in the matrix and the categories
for m = 1:rowscols(1) % as many rows as number of documents
  line = fgetl(fid);
  nums = sscanf(line, '%d');
  category(m) = nums(1);
  matrix(1 + cumsum(nums(2:2:end - 1)), m) = nums(3:2:end - 1);
end

matrix = matrix'; % flip it back

fclose(fid);

