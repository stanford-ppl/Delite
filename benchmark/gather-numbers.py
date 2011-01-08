from optparse import OptionParser
import multiprocessing
import os, sys
import math
from socket import gethostname
from string import *

delite_apps = ['gda']
delite_threads = [ 1, 2, 4, 8, 16]
delite_gpus = [ 1, 2 ]
matlab_apps = []
c_apps = []

statsFolder = "c:\\dev\stats\\"

STATS_DIR = os.getenv("STATS_DIR")
DATA_DIR = os.getenv("DATA_DIR")
CONFIG_DIR = os.getenv("CONFIG_DIR")
DELITE_HOME = os.getenv("DELITE_HOME")

params = {}

    

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
    if STATS_DIR is None:
        exit("STATS_DIR not defined, needs to be set to point to directory with times file.\nThis should match the directory supplied to the Delite runtime to dump its stats")
    if CONFIG_DIR is None:
        exit("CONFIG_DIR not defined, need to be set to point to directory with configurations for each machine")
    if DELITE_HOME is None:
        exit("DELITE_HOME not defined, needs to be set to point to Delite home directory")
    loadParams()
    
    # run the delite applications
    for app in delite_apps:
        #do it for each config of delite
        #do it for each thread configuration
        for numThreads in delite_threads:
            opts = "-DnumThreads=" + str(numThreads) + " -Ddump-stats -Ddump-stats-overwrite -DstatsOutputDirectory=" + str(STATS_DIR) + "/times -D=statsOutputFilename=" + app + "-smp-" +str(numThreads) + ".times"         
            os.putenv("JAVA_OPTS", os.getenv("JAVA_OPTS", "") + " " + opts)
            print "running: " + app + " " + params[app],
            print "with config: " + opts + "\n"
            os.system(DELITE_HOME + "/bin" + app + params[app])
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
    os.putenv("DELITE_BASE", CONFIG_DIR + "/../../")
    #os.system(CONFIG_DIR + "/" + app + params[app])

def isTflop():
    hostname = gethostname()
    if (hostname.startswith('tflop')):
        return True
    else:
        return False

def loadParams():
    if (isTflop()):
        hostname = 'tflop'
    else:
        hostname = 'default'
		
    f = open(CONFIG_DIR + 'datasets.' + hostname, 'r')
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
        return DATA_DIR + param[1:len(param)]
    else:
        return param   
    

if __name__ == "__main__":
    main()
