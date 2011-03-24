function exectime = runmatmult(gpu)
addpath matmult
if (gpu==1)
disp 'GPU'
    exectime = matmultgpu(5000)
elseif (gpu==2)
disp 'Jacket'
    exectime = matmultjacket(5000)
else
disp 'CPU'
    exectime = matmult(5000)
end
