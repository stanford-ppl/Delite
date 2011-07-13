package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.DeliteOP
import ppl.delite.runtime.scheduler.PartialSchedule

/**
 * Author: Kevin J. Brown
 * Date: 1/21/11
 * Time: 1:14 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MainGenerator extends ExecutableGenerator {
  protected def executableName = "Executable"
}

object GPUMainGenerator extends GPUExecutableGenerator {

  def makeExecutable(schedule: PartialSchedule, kernelPath: String) {
    assert(schedule.numResources == 1) //this implementation does not attempt to create an Executable than can efficiently handle hosting multiple kernel streams

    val location = schedule(0).peek.scheduledResource //look up location id for this GPU device resource
    val syncList = new ArrayBuffer[DeliteOP] //list of ops needing sync added

    addFunction(emitCppHeader)
    addFunction(emitCppBody(schedule(0), location, syncList))
    CudaCompile.addSource(buildCppSource(), executableName + location)

    val scalaSource = GPUScalaMainGenerator.emitScala(location, syncList, kernelPath)
    ScalaCompile.addSource(scalaSource, executableName + location)
  }

  protected def executableName = "Executable"

  private val functions = new ArrayBuffer[String]

  private[codegen] def addFunction(function: String) = functions += function

  private def buildCppSource() = {
    val source = new StringBuilder
    for (f <- functions) source.append(f)
    source.toString
  }

}

object GPUScalaMainGenerator extends GPUScalaExecutableGenerator {
  protected def executableName = "Executable"
}
