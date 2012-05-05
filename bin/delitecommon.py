# functionality common to delite and delitec

import os, sys
import ConfigParser

USER_HOME = os.getenv("HOME")
DELITE_HOME = os.getenv("DELITE_HOME")
JAVA_HOME = os.getenv("JAVA_HOME")
SCALA_VIRT_HOME = os.getenv("SCALA_VIRT_HOME")

scala_virt_version = "scala-2.10"
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
    global SCALA_VIRT_HOME

    if JAVA_HOME is None:
        if "java.home" in props:
            JAVA_HOME = props["java.home"]
        else:
            err("The JAVA_HOME environment variable must be defined or the java.home entry in delite.properties must be set.")

    if SCALA_VIRT_HOME is None:
        if "scala.virtualized.home" in props:
            scala_virt_home = props["scala.virtualized.home"]
            if not os.path.isdir(scala_virt_home):
                warn("couldn't find scala virtualized at: " + scala_virt_home)
            else:
                SCALA_VIRT_HOME = scala_virt_home
    
    if SCALA_VIRT_HOME is None:
        scala_virt_home = USER_HOME + "/.sbt/boot/" + scala_virt_version + "/lib/"
        if not os.path.isdir(scala_virt_home):
            err("couldn't find scala virtualized at: " + scala_virt_home + ". Please set the SCALA_VIRT_HOME environment variable or scala.virtualized.home entry in delite.properties manually.")
        SCALA_VIRT_HOME = scala_virt_home

def printEnv():
  print("======== REQUIRED DELITE ENVIRONMENT VARIABLES =========")
  print("USER_HOME = " + USER_HOME)
  print("DELITE_HOME = " + DELITE_HOME)
  print("JAVA_HOME = " + JAVA_HOME)
  print("SCALA_VIRT_HOME = " + SCALA_VIRT_HOME)


