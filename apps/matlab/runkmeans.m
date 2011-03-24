function exectime = runkmeans(gpu)
addpath kmeans
if (gpu == 1)
    exectime = kmeansgpu('/kunle/ppl/delite/data/ml/kmeans/mandrill-large.dat', '/kunle/ppl/delite/data/ml/kmeans/initmu.dat');
elseif (gpu == 2)
    exectime = kmeansjacket('/kunle/ppl/delite/data/ml/kmeans/mandrill-large.dat', '/kunle/ppl/delite/data/ml/kmeans/initmu.dat');
else
    exectime = kmeans('/kunle/ppl/delite/data/ml/kmeans/mandrill-large.dat', '/kunle/ppl/delite/data/ml/kmeans/initmu.dat');
end