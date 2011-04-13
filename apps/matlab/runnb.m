function exectime = runnb(gpu)
addpath nb
trainfile = '/kunle/ppl/delite/data/ml/nb/MATRIX.TRAIN.RANDOM.250K';
if (gpu == 1)
    exectime = nb_traingpu(trainfile);
elseif(gpu == 2)
    exectime = nb_trainjacket(trainfile);
else
    exectime = nb_train(trainfile);
end
