package ppl.delite.runtime.codegen

import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.ops.Sync
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.scheduler.{OpHelper, StaticSchedule, PartialSchedule}

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
    //TODO: this is a poor method of separating CPU from GPU, should be encoded - essentially need to loop over all nodes
    val schedule = graph.schedule

    assert((Config.numThreads + (if(graph.targets(Targets.Cpp)) Config.numCpp else 0) + (if(graph.targets(Targets.Cuda)) Config.numCuda else 0) + (if(graph.targets(Targets.OpenCL)) Config.numOpenCL else 0)) == schedule.numResources)
    Sync.addSync(graph)
    
    val scalaSchedule = schedule.slice(0, Config.numThreads)
    if (Config.numThreads > 0) checkRequestedResource(scalaSchedule, Targets.Scala)
    ScalaExecutableGenerator.makeExecutables(scalaSchedule, graph.kernelPath)

    // Hack to collect global inputTypesMap (TODO: Get rid of this)
    CppExecutableGenerator.typesMap = Map[Targets.Value, Map[String,String]]()
    val cppSchedule = schedule.slice(Config.numThreads, Config.numThreads+Config.numCpp)
    if (Config.numCpp > 0) checkRequestedResource(cppSchedule, Targets.Cpp)
    CppExecutableGenerator.collectInputTypesMap(graph)
    CppExecutableGenerator.makeExecutables(cppSchedule, graph.kernelPath)
    CppMultiLoopHeaderGenerator.clear()
    
    CudaExecutableGenerator.typesMap = Map[Targets.Value, Map[String,String]]()
    val cudaSchedule = schedule.slice(Config.numThreads+Config.numCpp, Config.numThreads+Config.numCpp+Config.numCuda)
    if (Config.numCuda > 0) checkRequestedResource(cudaSchedule, Targets.Cuda)
    CudaExecutableGenerator.collectInputTypesMap(graph)
    CudaExecutableGenerator.makeExecutables(cudaSchedule, graph.kernelPath)

    if (Config.printSources) { //DEBUG option
      ScalaCompile.printSources()
      CppCompile.printSources()
      CudaCompile.printSources()
      OpenCLCompile.printSources()
    }

    if (Config.numCpp>0 && graph.targets(Targets.Cpp)) CppCompile.compile()
    if (Config.numCuda>0 && graph.targets(Targets.Cuda)) CudaCompile.compile()
    if (Config.numOpenCL>0 && graph.targets(Targets.OpenCL)) OpenCLCompile.compile()

    val classLoader = ScalaCompile.compile

    val queues = StaticSchedule(schedule.numResources)
    for (i <- 0 until schedule.numResources if !schedule(i).isEmpty) {
      val cls = classLoader.loadClass("Executable"+i) //load the Executable class
      val executable = cls.getMethod("self").invoke(null).asInstanceOf[DeliteExecutable] //retrieve the singleton instance
      queues(i) += executable
    }
    queues
  }

  def checkRequestedResource(schedule: PartialSchedule, target: Targets.Value) {
    if (schedule.map(_.size).reduce(_ + _) == 0)
      println("WARNING: no kernels scheduled on " + target)
  }

}
