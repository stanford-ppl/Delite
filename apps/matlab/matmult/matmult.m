function exectime = matmult(n)

a = rand(n, n, 'single');
b = rand(n, n, 'single');

tic
for i=1:5
z = a * b;
end
exectime = toc;
