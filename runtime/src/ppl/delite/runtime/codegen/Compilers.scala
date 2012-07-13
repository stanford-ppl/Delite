package ppl.delite.runtime.codegen

import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.ops.Sync
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.hosts.Hosts
import ppl.delite.runtime.scheduler.{OpHelper, StaticSchedule}

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:41:08 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Compilers {

  def apply(target: Targets.Value): CodeCache = {
    target match {
      case Targets.Scala => ScalaCompile
      case Targets.Cuda => CudaCompile
      case Targets.OpenCL => OpenCLCompile
      case Targets.Cpp => CppCompile
      case _ => throw new RuntimeException("Undefined target for runtime compilers")
    }
  }

  def compileSchedule(graph: DeliteTaskGraph): StaticSchedule = {
    //generate executable(s) for all the ops in each proc
    //TODO: this is a poor method of separating CPU from GPU, should be encoded
    val schedule = graph.schedule
    assert((Config.numThreads + Config.numCpp + Config.numCuda + Config.numOpenCL) == schedule.numResources)
    Sync.addSync(graph)
    ScalaExecutableGenerator.makeExecutables(schedule.slice(0,Config.numThreads), graph.kernelPath)

    // Hack to collect global inputTypesMap (TODO: Get rid of this)
    CppExecutableGenerator.collectInputTypesMap(graph)
    CppExecutableGenerator.makeExecutables(schedule.slice(Config.numThreads, Config.numThreads+Config.numCpp), graph.kernelPath)

    /*
    for (i <- 0 until numGPUs) {
      if (Config.useOpenCL){
        OpenCLMainGenerator.makeExecutable(schedule.slice(numThreads+i, numThreads+i+1), graph.kernelPath)
      }
      else
        CudaMainGenerator.makeExecutable(schedule.slice(numThreads+i, numThreads+i+1), graph.kernelPath)
    }
    */

    if (Config.printSources) { //DEBUG option
      ScalaCompile.printSources()
      CppCompile.printSources()
      CudaCompile.printSources()
      OpenCLCompile.printSources()
    }

    if (Config.numCpp > 0) CppCompile.compile()
    if (Config.numCuda > 0) CudaCompile.compile()
    if (Config.numOpenCL > 0) OpenCLCompile.compile()

    val classLoader = ScalaCompile.compile

    val queues = StaticSchedule(schedule.numResources)
    for (i <- 0 until schedule.numResources) {
      val cls = classLoader.loadClass("Executable"+i) //load the Executable class
      val executable = cls.getMethod("self").invoke(null).asInstanceOf[DeliteExecutable] //retrieve the singleton instance
      queues(i) += executable
    }
    queues
  }

}
