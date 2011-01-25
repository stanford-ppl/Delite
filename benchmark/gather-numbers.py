#!/usr/local/bin/python

from optparse import OptionParser
import multiprocessing
import os, sys
import math
from socket import gethostname
from string import *
import ConfigParser

DELITE_HOME = os.getenv("DELITE_HOME")

apps_default = ['gda', 'nb', 'linreg', 'kmeans', 'rbm', 'svm']#, 'lbp']
delite_threads_default = [ 1, 2 , 4, 8]


#delite_gpus = [ 1, 2 ]
#matlab_apps = []
#c_apps = []




params = {}
classes = {}
props = {}
options = {}


def main():
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose")
    parser.add_option("-a", "--apps", dest="apps", default="_all", help="a list of comma separated applications to run (e.g. -a gda,nb)")
    parser.add_option("-r", "--runs", dest="runs", default="10", type="string", help="the number of times the runtime will execute the applications")
    parser.add_option("-t", "--threads", dest="threads", default="_all", help="a list of comma separated thread counts (e.g. -t 1,2,4)")
    parser.add_option("-s", "--skip", dest="skip", default="_none", help="skips smp and/or gpu portion of gathering numbers (e.g. -s gpu)")
    parser.add_option("--nv", dest="no_variants", action="store_true" , help="disables variant support in the framework")

    (opts, args) = parser.parse_args()
    if len(args) != 0:
        parser.error("incorrect number of arguments")
    
    loadOptions(opts)
    loadProps(options)
    loadParams(options)
    loadClasses(options)
    launchApps(options)
    


def loadOptions(opts):
    options['verbose'] = opts.verbose
    #process apps
    if(opts.apps == "_all"):
        options['apps'] = apps_default
    else:
        options['apps'] = opts.apps.split(',')
    #process numRuns
    options['runs'] = opts.runs
    #process numThreads
    if(opts.threads == "_all"):
        options['delite.threads'] = delite_threads_default
    else:
        options['delite.threads'] = opts.threads.split(',')
    options['run']={}
    options['run']['smp'] = True
    options['run']['gpu'] = True
    
    if(opts.skip != "_none"):
        for s in opts.skip.split(','):
            options['run'][s] = False
    options['variants'] = not opts.no_variants

def loadProps(options):
    #load and check all the required environment variables
    if DELITE_HOME is None:
        exit("DELITE_HOME not defined, needs to be set to point to Delite home directory")
    config = ConfigParser.ConfigParser()
    config.readfp(open(DELITE_HOME + '/delite.properties'))
    items = config.items('delite')
    for item in items:
        k, v = item
        props[k] = v
    if(options['verbose']):
        print "loaded the following properties from delite.properties"
    for k in props.keys():
        print k + ":" + props[k]

def launchApps(options):
    # run the delite applications
    for app in options['apps']:
        print "==================================================="
        print "==         " + app 
        print "===================================================" 
        opts = "-Dblas.home=" + props['intel.mkl'] + " -Ddelite.home.dir=" + DELITE_HOME + " -Ddelite.build.dir=" + DELITE_HOME +  "/generated/ -Ddelite.deg.filename=" + app + ".deg"
        if options['variants'] == False:
            opts = opts + " -Dnested.variants.level=0"
        os.putenv("GEN_OPTS", opts)
        #MKL ENV
        os.putenv("LD_PRELOAD", props['java.home'] + "/jre/lib/amd64/libjsig.so")
        #emit other envs
        os.putenv("JAVA_HOME", props['java.home'])
        os.putenv('LMS_HOME', props['libs.lms.home'])
        os.putenv('SCALA_VIRT_HOME', props['scala.virtualized.home'])
        os.putenv('PATH', props['intel.icc'] + ":" + os.getenv('PATH'))
        print "==  Generating DEG file with options: " + opts
        ecode = os.system(DELITE_HOME + "/bin/gen " + classes[app])
        if ecode != 0:
            print "Detected abnormal exit code, exiting"
            exit(-1)
        #do it for each config of delite
        #do it for each thread configuration
        if options['run']['smp']: 
            for numThreads in options['delite.threads']:
                opts = "-Ddelite.home=" + DELITE_HOME + " -Ddelite.threads=" + str(numThreads) + " -Ddelite.runs=" + options['runs'] + " -Dstats.dump -Dstats.dump.component=app -Dstats.dump.overwrite -Dstats.output.dir=" + DELITE_HOME  + "/benchmark/times -Dstats.output.filename=" + app + "-smp-" +str(numThreads) + ".times"         
                os.putenv("JAVA_OPTS", os.getenv("JAVA_OPTS", "") + " " + opts)
                os.putenv("MKL_NUM_THREADS", str(numThreads))
                print "running: " + app + " " + params[app],
                print "with config: " + opts + "\n"
                os.putenv("SCALA_HOME", props['scala.vanilla.home'])
                ecode = os.system(DELITE_HOME + "/bin/exec " + app + ".deg " + params[app])
                if ecode != 0:
                    print "Detected abnormal exit code, exiting"
                    exit(-1)

        #check if gpu option is enabled
        if options['run']['gpu']:
            opts = "-Ddelite.home=" + DELITE_HOME +" -Ddelite.threads=1 -Ddelite.gpus=1 -Ddelite.runs=" + options['runs'] + " -Dstats.dump -Dstats.dump.component=app -Dstats.dump.overwrite -Dstats.output.dir=" + DELITE_HOME  + "/benchmark/times -Dstats.output.filename=" + app + "-gpu.times"         
            os.putenv("JAVA_OPTS", os.getenv("JAVA_OPTS", "") + " " + opts)
            os.putenv("MKL_NUM_THREADS", "1")
            #need nvcc in your path
            os.putenv('PATH', props['nvidia.cuda'] + "/bin:" + os.getenv('PATH'))
            print "== executing application: " + app + " " + params[app],
            print "== with options: " + opts + "\n"
            os.putenv("SCALA_HOME", props['scala.vanilla.home'])
            ecode = os.system(DELITE_HOME + "/bin/exec " + app + ".deg " + params[app])
            if ecode != 0:
                print "Detected abnormal exit code, exiting"
                exit(-1)
 		



def isTflop():
    hostname = gethostname()
    if (hostname.startswith('tflop')):
        return True
    else:
        return False

def loadClasses(options):
    f = open(DELITE_HOME + "/benchmark/config/classes", 'r')
    for line in f:
        tokens = line.split('|')
        app = tokens.pop(0)
        clazz = tokens.pop(0)
        classes[app] = clazz
    f.close()

def loadParams(options):
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
        return props['apps.data'] + "/" +  param[1:len(param)]
    else:
        return param   
    

if __name__ == "__main__":
    main()
