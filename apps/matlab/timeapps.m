function timeapps(app, gpu)

if (gpu==2)
    proclist = -2;
elseif (gpu==1)
    proclist = -1;
else
    proclist = [1, 2, 4, 8]; % -1 denotes gpu, -2 denotes jacket
end

if (strcmpi(app, 'all'))
%	apps = {'gda', 'nb', 'linreg', 'kmeans', 'svm', 'lbp', 'matmult' 'rbm'};
       apps = {'linreg', 'kmeans', 'svm', 'lbp', 'matmult'};
else
	apps = {app};
end
	
outputTimes = -1 * ones(length(proclist), length(apps));
for j=1:length(apps)
    for i=1:length(proclist)
        outputTimes(i, j) = timeapp(char(apps(j)), proclist(i));
    end
end

outputTimes
