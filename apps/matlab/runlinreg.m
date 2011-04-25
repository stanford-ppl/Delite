function exectime = runlinreg(gpu)
addpath linreg
xfile = '/kunle/ppl/delite/data/ml/linreg/x-2048.dat';
yfile = '/kunle/ppl/delite/data/ml/linreg/y-2048.dat';
if (gpu == 1)
    exectime = linreggpu(xfile, yfile);
elseif (gpu == 2)
    exectime = linregjacket(xfile, yfile);
else
    exectime = linreg(xfile, yfile);
end
