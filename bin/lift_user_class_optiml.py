#!/usr/local/bin/python

from optparse import OptionParser
import os, errno


options = {}
classes = []

def main():
    usage = "usage: %prog [options] ops_dir data_dir impls_dir(s)"
    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose")

    (opts, args) = parser.parse_args()
    if len(args) < 2:
        parser.error("incorrect number of arguments")

    ops_dir = args.pop(0)
    data_dir = args.pop(0)

    initDir(ops_dir)
    initDir(data_dir)

    loadOptions(opts)

    #get the list of files
    for impls_dir in args:
      files = os.listdir(impls_dir)
      for fname in files:
          if fname.endswith(".scala") and os.path.isdir(impls_dir + "/" + fname) == False:
              print "processing file:[" + fname + "]"
              liftClass(impls_dir, fname, ops_dir)
              copyClass(impls_dir, fname, data_dir)
    #now emit the application ops directory
    emitApplicationOps(ops_dir)

def initDir(path):
    try:
        os.makedirs(path)
    except OSError as exc:
        if exc.errno == errno.EEXIST:
          pass
        else: raise

def emitApplicationOps(ops_dir):
    out = []
    #package
    l =     "package ppl.dsl.optiml.application\n\n"
    l = l + "trait ApplicationOps extends " + mixify(classes, "", "Ops") + "\n"
    l = l + "trait ApplicationOpsExp extends " + mixify(classes, "", "OpsExp") + "\n"
    l = l + "trait ScalaGenApplicationOps extends " + mixify(classes, "ScalaGen", "Ops") + " \n\n" 
    
    l = l + "// abstract types for internal application data structures\n"
    for c in classes:
      l = l + "trait " + c + "\n"

    out.append(l)
    fileOut = open (ops_dir + "/ApplicationOps.scala", 'w')
    fileOut.writelines(out)
    fileOut.close()

def mixify(classes, pre, post):
    l = ""
    for c in classes:
        l = l + pre + c + post
        if(classes.index(c) != len(classes) - 1):
            l = l + " with " 
    return l

def copyClass(impls_dir, fname, data_dir):
    pckNimp = "package ppl.dsl.optiml.datastruct.scala"

    file = open(impls_dir + '/' + fname, 'r')
    #file.readline() # skip first line

    out = open(data_dir + '/' + fname, 'w')
    out.write(pckNimp + "\n")
    
    for line in file:
        out.write(line)

    file.close()
    out.close()

def liftClass(impls_dir, fname, ops_dir):
    
    #dictionary to keep track of fields and their types
    fields = []
    types = {}
    clazz = ""
    out = []

    #take care of package and imports
    pckNimp = "package ppl.dsl.optiml.application\n\n\
import ppl.dsl.optiml._\n\
import java.io.PrintWriter\n\
import ppl.delite.framework.{DSLType}\n\
import scala.virtualization.lms.common.ScalaGenBase\n\
import scala.virtualization.lms.util.OverloadHack\n\
import scala.virtualization.lms.common.{EffectExp, Variables}\n\n"
    out.append(pckNimp)

    file = open(impls_dir + '/' + fname, 'r')
    for line in file:
        #take care of package and imports
        line = line.lstrip().rstrip()

        #take care of class definition
        if line.startswith("class "):
            if clazz != "":
                print "Found what I think are two class definitions [" + clazz + "] and also this line:\n" + line
            clazz = line[6:].lstrip().rstrip('(').rstrip()
            classes.append(clazz)
            lclazz = clazz.lower()
            if options['verbose']:
                print "found class[" + clazz + "]"
            #add space after imports
            out.append("\n")
            continue
        #take care of vals
        if line.startswith("val "):
            #strip the val
            line = line [4:].lstrip().rstrip(",")
            tokens = line.split(":")
            name = tokens[0].rstrip()
            type = tokens[1].lstrip()
            if options['verbose']:
                print "found val:[" + name + ":" + type + "]"
            fields.append(name)
            types[name]= type
            continue
                
        #anything else just ignore
        if len(line) != 0:
            print "ignoring following line:[" + line + "]"
    file.close()

    
    
    #First handle the Ops part
    l =     "trait " + clazz + "Ops extends DSLType with Variables with OverloadHack {\n\n"
    l = l + "  object " + clazz + " {\n"
    l = l + "    def apply(" + repify(fields, types) + ") = " + lclazz + "_obj_new(" + listify(fields) + ")\n"
    l = l + "  }\n\n"

    l = l + "  implicit def rep" + clazz + "To" + clazz + "Ops(x: Rep[" + clazz +"]) = new " + lclazz + "OpsCls(x)\n"
    l = l + "  implicit def " + lclazz + "To" + clazz + "Ops(x: Var[" + clazz + "]) = new " + lclazz + "OpsCls(readVar(x))\n\n"

    l = l + "  class " + lclazz + "OpsCls(__x: Rep[" + clazz + "]) {\n"
    for f in fields:
        l = l + "    def " + f + " = " + lclazz + "_" + f + "(__x)\n"
    l = l + "  }\n\n"
    
    l = l + "  //object defs\n"
    l = l + "  def " + lclazz + "_obj_new(" + repify(fields, types) + "): Rep[" + clazz + "]\n\n"
    
    l = l + "  //class defs\n"
    for f in fields:
        l = l + "  def " + lclazz + "_" + f + "(__x: Rep[" + clazz + "]): Rep[" + types[f] + "]\n"
    l = l + "}\n\n"

    #OpsExp
    l = l + "trait " + clazz + "OpsExp extends " + clazz + "Ops with EffectExp {\n"
    l = l + "  case class " + clazz + "ObjectNew(" + expify(fields, types) + ") extends Def[" + clazz + "]\n"
    for f in fields:
        l = l + "  case class " + clazz + f.capitalize() + "(__x: Exp[" + clazz + "]) extends Def[" + types[f] + "]\n"
    l = l + "\n"
    l = l + "  def " + lclazz + "_obj_new(" + expify(fields, types) + ") = reflectEffect(" + clazz + "ObjectNew(" + listify(fields) + "))\n"
    for f in fields:
        l = l + "  def " + lclazz + "_" + f + "(__x: Rep[" + clazz + "]) = " + clazz + f.capitalize() +"(__x)\n"
    l = l + "}\n\n"


    l = l + "trait ScalaGen" + clazz + "Ops extends ScalaGenBase {\n"
    l = l + "  val IR: ApplicationOpsExp\n"
    l = l + "  import IR._\n\n"
    
    l = l + "  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {\n"
    l = l + "  // these are the ops that call through to the underlying real data structure\n"
    l = l + "    case " + clazz + "ObjectNew(" + listify(fields) + ") => emitValDef(sym, \"new \" + remap(manifest[" + clazz + "]) + \"(" + quotify(fields) + ")\")\n"
    for f in fields:
        l = l + "    case " + clazz + f.capitalize() + "(x) =>  emitValDef(sym, quote(x) + \"." + f +"\")\n" 
    l = l + "    case _ => super.emitNode(sym, rhs)\n"
    l = l + "  }\n"
    l = l + "}\n"


    out.append(l)
    if options['verbose']:
        print "resulting file:"
        for x in out:
            print x,

    
    outName = fname.replace(".scala", "Ops.scala")
    fileOut = open (ops_dir + '/' + outName, 'w')
    fileOut.writelines(out)
    fileOut.close()


def repify(fields, types):
    l = ""
    for f in fields:
        l = l + f + ": Rep[" + types[f] + "]"
        if(fields.index(f) != len(fields) - 1):
            l = l + ", "          
    return l

def expify(fields, types):
    l = ""
    for f in fields:
        l = l + f + ": Exp[" + types[f] + "]"
        if(fields.index(f) != len(fields) - 1):
            l = l + ", "          
    return l

def quotify(fields):
    l = "\" + "
    for f in fields:
        l = l + "quote(" + f + ") "
        if(fields.index(f) != len(fields) - 1):
            l = l + " + \",\" + "
    l = l + " + \""
    return l


def listify(fields):
    l = ""
    for f in fields:
        l = l + f 
        if(fields.index(f) != len(fields) - 1):
            l = l + ", "          
    return l



def loadOptions(opts):
    options['verbose'] = opts.verbose





if __name__ == "__main__":
    main()
