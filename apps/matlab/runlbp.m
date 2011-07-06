function exectime = runlbp(gpu)
addpath lbp
f1 = '/kunle/ppl/delite/data/ml/lbp/onlyedges1';
f2 = '/kunle/ppl/delite/data/ml/lbp/graphprint1';
rand('state', 0);
if (gpu == 1)
    exectime = lbpgpu(f1, f2);
elseif (gpu == 2)
    exectime = lbpjacket(f1, f2);
else
    exectime = lbp(f1, f2);
end

