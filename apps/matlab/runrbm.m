function exectime = runrbm(gpu)
addpath rbm
if (gpu==1)
    exectime = rbmgpu('/kunle/ppl/delite/data/ml/rbm/mnist2000x10.dat', 2000, 2000);
elseif (gpu==2)
    exectime = rbmjacket('/kunle/ppl/delite/data/ml/rbm/mnist2000x10.dat', 2000, 2000);
else
    exectime = rbm('/kunle/ppl/delite/data/ml/rbm/mnist2000x10.dat', 2000, 2000);
end
