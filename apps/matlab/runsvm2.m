function exectime = runsvm(gpu)
addpath svm
trainfile = '/kunle/ppl/delite/data/ml/svm/MATRIX.TRAIN.400'
tol = 0.0001
rand('state', 0);
if (gpu == 1)
    exectime = svm_traingpu(trainfile, tol);
elseif (gpu == 2)
    exectime = svm_trainjacket(trainfile, tol);
else
    exectime = svm_train(trainfile, tol);
end