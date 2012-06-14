package ppl.delite.runtime.codegen

import java.util.ArrayDeque
import ppl.delite.runtime.graph.ops._
import collection.mutable.ArrayBuffer
import java.lang.annotation.Target
import ppl.delite.runtime.graph.targets.{OS, OPData, Targets}

/**
 * Author: Kevin J. Brown
 * Date: Dec 1, 2010
 * Time: 8:18:07 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * Generates optimized DeliteExecutable for a CUDA host thread for a given schedule
 * This generator creates a single executable function for the GPU host
 * The generated code is C++ (and a JNI call) in order to work with CUDA efficiently
 * WARNING: The implementation used here is not efficient for hosting multiple CUDA kernel streams simultaneously
 *
 * This generator makes the following synchronization optimizations:
 * 1) It utilizes 3 CUDA streams (1 for kernels, 1 for h2d transfers, and 1 for d2h transfers), for the maximum possible communication/computation overlap
 * 2) It generates a synchronized getter (h2d transfers) for dependencies from other resources for the first use only
 *    transferred data remains in the GPU device memory for local reuse
 * 3) It generates synchronized result publications (d2h transfers) only for outputs that other resources will need to consume
 *    outputs that the scheduler has restricted to this GPU resource exist only in device memory
 * 4) All kernel launches and device memory transfers are asynchronous with ordering maintained through CUDA events
 *    This allows the host thread to run-ahead as much as possible (keep multiple streams occupied)
 *    The host thread only blocks when data must be transferred to/from other CPU threads
 */

trait GPUExecutableGenerator {

  protected def addKernelCalls(schedule: ArrayDeque[DeliteOP], location: Int, available: ArrayBuffer[(DeliteOP,String)], awaited: ArrayBuffer[DeliteOP], syncList: ArrayBuffer[DeliteOP], out: StringBuilder)(implicit aliases:AliasTable[(DeliteOP,String)])

  protected def executableName: String

  protected def getSymCPU(name: String): String = {
    "xC"+name
  }

  protected def getSymGPU(name: String): String = {
    "xG"+name
  }

  protected def getScalaSym(op: DeliteOP, name: String): String = {
    "x"+name
  }

  protected def getJNIType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "void"
      case "Int" => "jint"
      case "Long" => "jlong"
      case "Float" => "jfloat"
      case "Double" => "jdouble"
      case "Boolean" => "jboolean"
      case "Short" => "jshort"
      case "Char" => "jchar"
      case "Byte" => "jbyte"
      //case r if r.startsWith("generated.scala.Ref[") => getJNIType(r.slice(20,r.length-1))
      case _ => "jobject"//all other types are objects
    }
  }

  protected def getJNIArgType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "Lscala/runtime/BoxedUnit;"
      case "Int" => "I"
      case "Long" => "J"
      case "Float" => "F"
      case "Double" => "D"
      case "Boolean" => "Z"
      case "Short" => "S"
      case "Char" => "C"
      case "Byte" => "B"
      //case r if r.startsWith("generated.scala.Ref[") => getJNIArgType(r.slice(20,r.length-1))
      case array if array.startsWith("Array[") => "[" + getJNIArgType(array.slice(6,array.length-1))
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    }
  }

  protected def getJNIOutputType(scalaType: String): String = {
    scalaType match {
      case "Unit" => "V"
      case "Int" => "I"
      case "Long" => "J"
      case "Float" => "F"
      case "Double" => "D"
      case "Boolean" => "Z"
      case "Short" => "S"
      case "Char" => "C"
      case "Byte" => "B"
      //case r if r.startsWith("generated.scala.Ref[") => getJNIOutputType(r.slice(20,r.length-1))
      case array if array.startsWith("Array[") => "[" + getJNIOutputType(array.slice(6,array.length-1))
      case _ => { //all other types are objects
        var objectType = scalaType.replace('.','/')
        if (objectType.indexOf('[') != -1) objectType = objectType.substring(0, objectType.indexOf('[')) //erasure
        "L"+objectType+";" //'L' + fully qualified type + ';'
      }
    }
  }

  protected def getJNIFuncType(scalaType: String): String = scalaType match {
    case "Unit" => "Void"
    case "Int" => "Int"
    case "Long" => "Long"
    case "Float" => "Float"
    case "Double" => "Double"
    case "Boolean" => "Boolean"
    case "Short" => "Short"
    case "Char" => "Char"
    case "Byte" => "Byte"
    //case r if r.startsWith("generated.scala.Ref[") => getJNIFuncType(r.slice(20,r.length-1))
    case _ => "Object"//all other types are objects
  }

  protected def getCPrimitiveType(scalaType: String): String = scalaType match {
    case "Unit" => "void"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Short" => "short"
    case "Char" => "char"
    case "Byte" => "char"
    case r if r.startsWith("generated.scala.Ref[") => getCPrimitiveType(r.slice(20,r.length-1))
    case other => error(other + " is not a primitive type")
  }

  protected def isPrimitiveType(scalaType: String): Boolean = scalaType match {
    case "Unit" => true
    case "Int" => true
    case "Long" => true
    case "Float" => true
    case "Double" => true
    case "Boolean" => true
    case "Short" => true
    case "Char" => true
    case "Byte" => true
    case r if r.startsWith("generated.scala.Ref[") => isPrimitiveType(r.slice(20,r.length-1))
    case _ => false
  }

  protected def writeJNIInitializer(locations: Set[Int], out: StringBuilder) {
    for (i <- locations) {
      out.append("jclass cls")
      out.append(i)
      out.append(" = env->FindClass(\"")
      out.append(executableName)
      out.append(i)
      out.append("\");\n")
    }
    //add a reference to the singleton of scala.runtime.BoxedUnit for use everywhere required
    out.append("jclass clsBU = env->FindClass(\"scala/runtime/BoxedUnit\");\n")
    out.append("jobject boxedUnit = env->GetStaticObjectField(clsBU, env->GetStaticFieldID(clsBU, \"UNIT\", \"Lscala/runtime/BoxedUnit;\"));\n")
  }

  protected def writeJNIFinalizer(locations: Set[Int], out: StringBuilder) {
    for (i <- locations) {
      out.append("env->DeleteLocalRef(cls")
      out.append(i)
      out.append(");\n")
    }
    out.append("env->DeleteLocalRef(clsBU);\n")
    out.append("env->DeleteLocalRef(boxedUnit);\n")
  }

  protected def emitCppHeader: String
  protected def emitCppBody(schedule: ArrayDeque[DeliteOP], location: Int, syncList: ArrayBuffer[DeliteOP]): String

}


class GPUScalaExecutableGenerator(target: Targets.Value) extends ExecutableGenerator {

  protected def executableName = "Executable"

  def emitScala(location: Int, syncList: ArrayBuffer[DeliteOP], kernelPath: String): String = {
    val out = new StringBuilder

    //the header
    writeHeader(out, location, "")

    //the run method
    out.append("def run() {\n")
    out.append("hostGPU\n")
    out.append('}')
    out.append('\n')

    //the native method
    out.append("@native def hostGPU : Unit\n")

    //link the native code upon object creation
    if(target == Targets.Cuda) {
      out.append("System.load(\"\"\"")
      out.append(CudaCompile.binCacheHome)
      out.append("cudaHost.")
      out.append(OS.libExt)
      out.append("\"\"\")\n")
    }
    else if (target == Targets.OpenCL) {
      out.append("System.load(\"\"\"")
      out.append(OpenCLCompile.binCacheHome)
      out.append("openclHost.")
      out.append(OS.libExt)
      out.append("\"\"\")\n")
    }

    //the sync methods/objects
    addSync(syncList, out)
    writeOuterSet(syncList, out) //helper set methods for JNI calls to access

    //an accessor method for the object
    addAccessor(out)

    //the footer
    out.append('}')
    out.append('\n')

    out.toString
  }

  protected def writeOuterSet(list: ArrayBuffer[DeliteOP], out: StringBuilder) {
    for (op <- list) {
      for (sym <- op.getOutputs) {
        out.append("def set")
        out.append(getSym(op, sym))
        out.append("(result : ")
        out.append(op.outputType(sym))
        out.append(") = ")
        out.append(getSync(op, sym))
        out.append(".set(result)\n")
      }
    }
  }
}
