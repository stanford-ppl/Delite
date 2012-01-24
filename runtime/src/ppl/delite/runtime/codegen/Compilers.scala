package ppl.delite.runtime.codegen

import java.util.ArrayDeque
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.scheduler.{StaticSchedule, PartialSchedule}
import ppl.delite.runtime.Config

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:41:08 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Compilers {

  def compileSchedule(graph: DeliteTaskGraph): StaticSchedule = {
    //generate executable(s) for all the ops in each proc
    //TODO: this is a poor method of separating CPU from GPU, should be encoded
    val numThreads = Config.numThreads
    val numGPUs = Config.numGPUs
    val schedule = graph.schedule
    assert((numThreads + numGPUs) == schedule.numResources)
    MainGenerator.makeExecutables(schedule.slice(0,numThreads), graph.kernelPath)
    for (i <- 0 until numGPUs) {
      if (Config.useOpenCL){
        OpenCLMainGenerator.makeExecutable(schedule.slice(numThreads+i, numThreads+i+1), graph.kernelPath)
      }
      else
        CudaMainGenerator.makeExecutable(schedule.slice(numThreads+i, numThreads+i+1), graph.kernelPath)
    }

    if (Config.printSources) { //DEBUG option
      ScalaCompile.printSources()
      if (Config.useOpenCL) OpenCLCompile.printSources() else CudaCompile.printSources()
    }

    //assert(false)


    if (Config.useOpenCL) OpenCLCompile.compile() else CudaCompile.compile()
    val classLoader = ScalaCompile.compile

    val queues = new Array[ArrayDeque[DeliteExecutable]](schedule.numResources)
    for (i <- 0 until schedule.numResources) {
      val cls = classLoader.loadClass("Executable"+i) //load the Executable class
      val executable = cls.getMethod("self").invoke(null).asInstanceOf[DeliteExecutable] //retrieve the singleton instance

      queues(i) = new ArrayDeque[DeliteExecutable]
      queues(i).add(executable)
    }

    new StaticSchedule(queues)
  }

}
