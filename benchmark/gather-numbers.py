from optparse import OptionParser
import multiprocessing
import os, sys
import math
from socket import gethostname
from string import *

delite_apps = ['gda', 'nb', 'linreg', 'kmeans', 'rbm']#, 'lbp','svm']
numRuns = 10
delite_threads = [ 1, 2 , 4, 8, 16]
delite_gpus = [ 1, 2 ]
matlab_apps = []
c_apps = []

DATA_DIR = os.getenv("DATA_DIR")
DELITE_HOME = os.getenv("DELITE_HOME")
JAVA_HOME = os.getenv("JAVA_HOME")



params = {}
classes = {}
    

def main():
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose")
    (options, args) = parser.parse_args()
    if len(args) != 0:
        parser.error("incorrect number of arguments")
    
    # check all the required environment variables
    if DATA_DIR is None:
        exit("DATA_DIR not defined, needs to be set to point to directory that contains the data folder with datasets")
    if DELITE_HOME is None:
        exit("DELITE_HOME not defined, needs to be set to point to Delite home directory")
    loadParams()
    loadClasses()
    
    # run the delite applications
    for app in delite_apps:
        print "Running app: " + app + "\n===========================\nGenerating DEG file" 
        os.putenv("GEN_OPTS", "-Dblas.home=/usr/local/intel -Ddelite.home.dir=" + DELITE_HOME + " -Ddelite.build.dir=" + DELITE_HOME +  "/generated/ -Ddelite.deg.filename=" + app + ".deg")
        #MKL ENV
        os.putenv("LD_PRELOAD", JAVA_HOME + "/jre/lib/amd64/libjsig.so")
        os.system(DELITE_HOME + "/bin/gen " + classes[app])
        #do it for each config of delite
        #do it for each thread configuration
        for numThreads in delite_threads:
            opts = "-Ddelite.threads=" + str(numThreads) + " -Ddelite.runs=" + str(numRuns) + " -Dstats.dump -Dstats.dump.component=app -Dstats.dump.overwrite -Dstats.output.dir=" + DELITE_HOME  + "/benchmark/times -Dstats.output.filename=" + app + "-smp-" +str(numThreads) + ".times"         
            os.putenv("JAVA_OPTS", os.getenv("JAVA_OPTS", "") + " " + opts)
            os.putenv("MKL_NUM_THREADS", str(numThreads))
            print "running: " + app + " " + params[app],
            print "with config: " + opts + "\n"
            os.system(DELITE_HOME + "/bin/exec " + app + ".deg " + params[app])

#        if app.find("gpu") < 0:
#            for procnum in procnumbers:
#                launchApp(app, int(procnum), options)
#        else:
#            print launchApp(app, 1, options)

#def setApplicationVariables(procnum, nativeOps):
    #if procnum == -1:
     #   optsString = "-DenableDebugOptions=true -DbypassExecutor=true"
        #os.putenv("DELITE_NUM_THREADS", `1`)
    #else:
     #   optsString = ""
        #os.putenv("DELITE_NUM_THREADS", `procnum`)

    #if nativeOps:
    #    print "Enabling native ops."
    #    optsString = optsString + " -DuseNativeLibs=true"
    #else:
    #    print "Disabling native ops."
    #    optsString = optsString + " -DuseNativeLibs=false"
    #os.putenv("JAVA_OPTS", os.getenv("JAVA_OPTS", "") + " " + optsString)
		
def launchApp(app, procnum, options):
    setApplicationVariables(procnum, options.native)
    #os.putenv("DELITE_BASE", CONFIG_DIR + "/../../")
    #os.system(CONFIG_DIR + "/" + app + params[app])

def isTflop():
    hostname = gethostname()
    if (hostname.startswith('tflop')):
        return True
    else:
        return False

def loadClasses():
    f = open(DELITE_HOME + "/benchmark/config/classes", 'r')
    for line in f:
        tokens = line.split('|')
        app = tokens.pop(0)
        clazz = tokens.pop(0)
        classes[app] = clazz
    f.close()

def loadParams():
    if (isTflop()):
        hostname = 'tflop'
    else:
        hostname = 'default'
		
    f = open(DELITE_HOME + '/benchmark/config/datasets.' + hostname, 'r')
    for line in f:
        settings = line.split('|')
        app = settings.pop(0)
        app_params = ''
        for param in settings:
            param = expand(param)
            app_params = app_params + ' ' + param
        params[app] = app_params
    f.close()
 
def expand(param):
    if (param[0] == '$'):
        return DATA_DIR + "/" +  param[1:len(param)]
    else:
        return param   
    

if __name__ == "__main__":
    main()
