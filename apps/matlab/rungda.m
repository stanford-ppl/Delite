function exectime = rungda(gpu)
addpath gda
if (gpu==1)
    exectime = gdagpu('/kunle/ppl/delite/data/ml/gda/4096-1200x.dat','/kunle/ppl/delite/data/ml/gda/q1y.dat');
elseif (gpu==2)
    exectime = gdajacket('/kunle/ppl/delite/data/ml/gda/4096-1200x.dat','/kunle/ppl/delite/data/ml/gda/q1y.dat');
else
    exectime = gda('/kunle/ppl/delite/data/ml/gda/4096-1200x.dat','/kunle/ppl/delite/data/ml/gda/q1y.dat');
end
