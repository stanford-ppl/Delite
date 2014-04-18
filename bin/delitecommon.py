# functionality common to delite and delitec

import os, sys
import ConfigParser

USER_HOME = os.getenv("HOME")
DELITE_HOME = os.getenv("DELITE_HOME")
JAVA_HOME = os.getenv("JAVA_HOME")
HYPER_HOME = os.getenv("HYPER_HOME")
MESOS_NATIVE_LIBRARY = os.getenv("MESOS_NATIVE_LIBRARY")
DELITE_MEM = os.getenv("DELITE_MEM")

scala_major_id = "scala-2.10"
lms_version = "lms_2.10"

props = {}

def err(s):
    exit("error: " + s)

def warn(s):
    print("warn: " + s)
 
def initialize():
    checkDeliteEnv()
    loadProps()
    checkCommonEnv()

def checkDeliteEnv():
    global DELITE_HOME
    if DELITE_HOME is None:
        #try to check if it is in the usual place
        script_path = os.path.dirname(__file__)
        cand_home = os.path.split(script_path)[0]
        if os.path.isfile(cand_home + "/delite.properties.sample"):
          DELITE_HOME = cand_home
        else:
          err("The DELITE_HOME environment variable must be defined")


def loadProps():
    propf = DELITE_HOME + "/delite.properties"
    if not os.path.isfile(propf):
        return

    config = ConfigParser.ConfigParser()
    config.readfp(open(propf))
    items = config.items('delite')
    for item in items:
        k, v = item
        props[k] = v


def checkCommonEnv():
    global USER_HOME
    global JAVA_HOME
    global HYPER_HOME
    global MESOS_NATIVE_LIBRARY
    global DELITE_MEM

    if JAVA_HOME is None:
        if "java.home" in props:
            JAVA_HOME = props["java.home"]
        else:
            err("The JAVA_HOME environment variable must be defined or the java.home entry in delite.properties must be set.")

    if HYPER_HOME is None:
        if "hyper.home" in props:
            HYPER_HOME = props["hyper.home"]
        else:
            HYPER_HOME = DELITE_HOME

    if MESOS_NATIVE_LIBRARY is None:
        if "mesos.lib" in props:
            MESOS_NATIVE_LIBRARY = props["mesos.lib"]

    if DELITE_MEM is None:
        if "delite.mem" in props:
            DELITE_MEM = props["delite.mem"]


def printEnv():
  print("======== REQUIRED DELITE ENVIRONMENT VARIABLES =========")
  print("USER_HOME = " + USER_HOME)
  print("DELITE_HOME = " + DELITE_HOME)
  print("JAVA_HOME = " + JAVA_HOME)
