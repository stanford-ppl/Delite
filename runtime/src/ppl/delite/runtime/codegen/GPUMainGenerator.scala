package ppl.delite.runtime.codegen

import ppl.delite.runtime.graph.targets.Targets
import collection.mutable.ArrayBuffer
import ppl.delite.runtime.scheduler.PartialSchedule
import ppl.delite.runtime.graph.ops.DeliteOP

trait GPUMainGenerator extends GPUExecutableGenerator {
  val compiler: CCompile
  val tgt: Targets.Value

  protected def executableName = "Executable"
  private val functions = new ArrayBuffer[String]

  def makeExecutable(schedule: PartialSchedule, kernelPath: String) {
    assert(schedule.numResources == 1) //this implementation does not attempt to create an Executable than can efficiently handle hosting multiple kernel streams

    val location = schedule(0).peek.scheduledResource //look up location id for this GPU device resource
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added

    addFunction(emitCppHeader)
    addFunction(emitCppBody(schedule(0), location, syncList))

    compiler.addSource(buildCppSource(), executableName + location)

    val scalaSource = (new GPUScalaExecutableGenerator(tgt)).emitScala(location, syncList, kernelPath)
    ScalaCompile.addSource(scalaSource, executableName + location)
  }

  private[codegen] def addFunction(function: String) = functions += function

  private def buildCppSource() = {
    val source = new StringBuilder
    for (f <- functions) source.append(f)
    source.toString
  }
}

object CudaMainGenerator extends GPUMainGenerator with CudaGPUExecutableGenerator {
  val compiler = CudaCompile
  val tgt = Targets.Cuda
}

object OpenCLMainGenerator extends GPUMainGenerator with OpenCLGPUExecutableGenerator {
  val compiler = OpenCLCompile
  val tgt = Targets.OpenCL
}