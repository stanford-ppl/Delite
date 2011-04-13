function exectime = matmult(n)
addpath /usr/local/jacket/engine
	
a = gsingle(rand(n, n, 'single'));
b = gsingle(rand(n, n, 'single'));

tic
for i=1:5
z = a * b;
end
exectime = toc;
