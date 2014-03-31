package ppl.delite.runtime.codegen

import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.ops._
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.{OS, Targets}
import collection.mutable.ArrayBuffer
import sync._
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.scheduler.{OpHelper, OpList, PartialSchedule}

trait CppExecutableGenerator extends ExecutableGenerator {

  protected def addSource(source: String) {
    // Add extern header files for generated kernels at walk-time
    val externs = CppCompile.headers.map(s => "#include \"" + s + "\"\n").mkString("")
    CppCompile.addSource(externs+source, executableName)
  }

  protected def writeHeader() {
    out.append("#include <stdio.h>\n")
    out.append("#include <stdlib.h>\n")
    out.append("#include <jni.h>\n")
    out.append("#include \"cppSyncObjects.h\"\n")
    out.append("#include \"" + Targets.Cpp + "helperFuncs.h\"\n")
    out.append("#include \""+CppMultiLoopHeaderGenerator.headerFile+".h\"\n")
    out.append("extern JNIEnv* env" + location + ";\n")
  }

  protected def writeMethodHeader() {
    out.append("JNIEnv* env" + location + ";\n")
    val function = "JNIEXPORT void JNICALL Java_" + executableName + "_00024_host" + executableName + "(JNIEnv* jnienv, jobject object)"
    out.append("extern \"C\" ") //necessary because of JNI
    out.append(function)
    out.append(";\n")
    out.append(function)
    out.append(" {\n")
    out.append("env" + location + " = jnienv;\n")

    val locations = Range(0,Config.numThreads+Config.numCpp+Config.numCuda+Config.numOpenCL).toSet
    writeJNIInitializer(locations)
  }

  protected def writeJNIInitializer(locations: Set[Int]) {
    for (i <- locations) {
      out.append("jclass cls")
      out.append(i)
      out.append(" = env")
      out.append(location)
      out.append("->FindClass(\"")
      out.append("Sync_" + executableName(i))
      out.append("\");\n")
    }
    //add a reference to the singleton of scala.runtime.BoxedUnit for use everywhere required
    out.append("jclass clsBU = env" + location + "->FindClass(\"scala/runtime/BoxedUnit\");\n")
    out.append("jobject boxedUnit = env" + location + "->GetStaticObjectField(clsBU, env" + location + "->GetStaticFieldID(clsBU, \"UNIT\", \"Lscala/runtime/BoxedUnit;\"));\n")
  }

  protected def writeMethodFooter() {
    out.append("}\n")
  }

  protected def writeFooter() { }

  //TODO: can/should this be factored out? need some kind of factory for each target
  protected def makeNestedFunction(op: DeliteOP) = op match {
    case c: OP_Condition => {
      val codegen = new CppConditionGenerator(c, location, kernelPath)
      codegen.makeExecutable()
      CppCompile.addHeader(codegen.generateMethodSignature + ";\nextern bool " + c.id.split('_').head + "_cond;\n", codegen.executableName(location))
    }
   case w: OP_While => {
      val codegen = new CppWhileGenerator(w, location, kernelPath)
      codegen.makeExecutable()
      CppCompile.addHeader(codegen.generateMethodSignature + ";\n", codegen.executableName(location))
    }    
    case err => println("Cannot generate op" + op.id) //sys.error("Unrecognized OP type: " + err.getClass.getSimpleName)
  }

  protected def writeFunctionCall(op: DeliteOP) {
    def returnsResult = op.outputType(op.getOutputs.head) == op.outputType
    def resultName = if (returnsResult) getSymHost(op, op.getOutputs.head) else getOpSym(op)

    if (op.task == null) return //dummy op
    if (op.outputType(Targets.Cpp) != "void") {
      out.append(op.outputType(Targets.Cpp))
      out.append(' ')
      if (!isPrimitiveType(op.outputType)) out.append('*')
      out.append(resultName)
      out.append(" = ")
    }

    out.append(op.task) //kernel name
    op match {
      case args: Arguments => 
        if(Arguments.args.length > 0)
          out.append("(" + Arguments.args.length + Arguments.args.map("\""+_+"\"").mkString(",",",",");\n"))
        else
          out.append("(" + Arguments.args.length + ");\n") 
      case _ => out.append(op.getInputs.map(i=>getSymHost(i._1,i._2)).mkString("(",",",");\n"))
    }
   
    if (!returnsResult) {
      for (name <- op.getOutputs if(op.outputType(Targets.Cpp,name)!="void")) {
        out.append(op.outputType(Targets.Cpp, name))
        if (!isPrimitiveType(op.outputType(name))) out.append('*')
        out.append(" " + getSymHost(op,name) + " = " + resultName + "->" + name + ";\n")
      }
    }
  }

  protected def getSymCPU(name: String): String = {
    "xC"+name
  }

  protected def getSymHost(op: DeliteOP, name: String): String = {
    "xH"+name
  }

  protected def writeSyncObject() {  }

  protected def isPrimitiveType(scalaType: String) = scalaType match {
    case "java.lang.String" => true
    case _ => Targets.isPrimitiveType(scalaType)
  }

}

class CppMainExecutableGenerator(val location: Int, val kernelPath: String)
  extends CppExecutableGenerator with CppSyncGenerator {

  def executableName(location: Int) = "Executable" + location

  protected def syncObjectGenerator(syncs: ArrayBuffer[Send], target: Targets.Value) = {
    target match {
      case Targets.Scala => new ScalaMainExecutableGenerator(location, kernelPath) with ScalaSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case Targets.Cpp => new CppMainExecutableGenerator(location, kernelPath) with CppSyncObjectGenerator {
        protected val sync = syncs
        override def executableName(location: Int) = executableNamePrefix + super.executableName(location)
      }
      case _ => throw new RuntimeException("Unknown Host type " + target.toString)
    }
  }
}

object CppExecutableGenerator {

  val syncObjects = ArrayBuffer[String]()
  syncObjects += "#include <pthread.h>\n"
  syncObjects += "#include \"" + Targets.Cpp + "helperFuncs.h\"\n"
  syncObjects += "#include \""+CppMultiLoopHeaderGenerator.headerFile+".h\"\n"


  var typesMap = Map[Targets.Value, Map[String,String]]()

  //TODO: Remove this not to use global structure for type information
  def collectInputTypesMap(graph: DeliteTaskGraph) {
    for (resource <- graph.schedule; op <- resource) {
      if (op.getInputTypesMap != null)
        typesMap = DeliteTaskGraph.combineTypesMap(List(op.getInputTypesMap,typesMap))
      if (op.getOutputTypesMap != null)
        typesMap = DeliteTaskGraph.combineTypesMap(List(op.getOutputTypesMap,typesMap))

      if (op.isInstanceOf[OP_Nested]) {
        for (subgraph <- op.asInstanceOf[OP_Nested].nestedGraphs) {
          collectInputTypesMap(subgraph)
        }
      }
    }
  }

  def makeExecutables(schedule: PartialSchedule, kernelPath: String) {
    for (sch <- schedule if sch.size > 0) {
      val location = sch.peek.scheduledResource
      new CppMainExecutableGenerator(location, kernelPath).makeExecutable(sch) // native execution plan
      new ScalaNativeExecutableGenerator(location, kernelPath).makeExecutable() // JNI launcher scala source
    }
    // Register header file for the Cpp sync objects
    CppCompile.addHeader(syncObjects.mkString(""),"cppSyncObjects")
    CppMultiLoopHeaderGenerator.createHeaderFile()
  }

}

class ScalaNativeExecutableGenerator(override val location: Int, override val kernelPath: String) extends ScalaMainExecutableGenerator(location, kernelPath) {

  def makeExecutable() {
    writeHeader()
    writeNativeLoad()
    writeMethodHeader()
    writeNativeCall()
    writeMethodFooter()
    writeFooter()
    addSource(out.toString)
  }

  private def writeNativeLoad() {
    val degName = ppl.delite.runtime.Delite.inputArgs(0).split('.')
    val appName = degName(degName.length-2)
    val tgt = OpHelper.scheduledTarget(location)
    out.append("@native def host" + executableName(location) + ": Unit\n")
    out.append("System.load(\"\"\"")
    out.append(Compilers(tgt).binCacheHome)
    out.append(tgt)
    out.append("Host" + appName + ".")
    out.append(OS.libExt)
    out.append("\"\"\")\n")
  }

  private def writeNativeCall() {
    out.append("host")
    out.append(executableName(location))
    out.append('\n')
  }
}
