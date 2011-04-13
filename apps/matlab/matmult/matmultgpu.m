function exectime = matmultgpu(n)

a = gpuArray(rand(n, n, 'single'));
b = gpuArray(rand(n, n, 'single'));

tic
for i=1:5
z = a * b;
end
exectime = toc;
