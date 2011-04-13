function exectime = timeapp(appname, procs)
if (procs == -2)
    exectime = eval(['run' appname '(2)']);
elseif (procs == -1)
    exectime = eval(['run' appname '(1)']);
else
    oldprocs = maxNumCompThreads(procs);
    if (procs ~= 1)
        matlabpool('local',procs);
    end
    disp(appname)
    exectime = eval(['run' appname '(0)']);
    maxNumCompThreads(oldprocs);
    if (procs ~= 1)
        matlabpool close;
    end
end
end
