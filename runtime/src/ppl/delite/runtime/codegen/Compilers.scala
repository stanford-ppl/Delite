package ppl.delite.runtime.codegen

import kernels.cpp.CppMultiLoopHeaderGenerator
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.ops.Sync
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.codegen.hosts.Hosts
import ppl.delite.runtime.scheduler.{OpHelper, StaticSchedule, PartialSchedule}
import ppl.delite.runtime.DeliteMesosExecutor

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
    if(Config.clusterMode!=1) assert((Config.numThreads + (if(graph.targets(Targets.Cpp)) Config.numCpp else 0) + (if(graph.targets(Targets.Cuda)) Config.numCuda else 0) + (if(graph.targets(Targets.OpenCL)) Config.numOpenCL else 0)) == schedule.numResources)
    Sync.addSync(graph)
    

    //TODO: Fix this!
    if(Config.clusterMode != 2 || Config.numCuda == 0) 
      ScalaExecutableGenerator.makeExecutables(schedule.slice(0,Config.numThreads), graph.kernelPath)
    else 
      new ScalaMainExecutableGenerator(0, graph.kernelPath).makeExecutable(PartialSchedule(1).apply(0))

    // Hack to collect global inputTypesMap (TODO: Get rid of this)
    CppExecutableGenerator.typesMap = Map[Targets.Value, Map[String,String]]()
    CppExecutableGenerator.collectInputTypesMap(graph)
    CppExecutableGenerator.makeExecutables(schedule.slice(Config.numThreads, Config.numThreads+Config.numCpp), graph.kernelPath)

    CudaExecutableGenerator.typesMap = Map[Targets.Value, Map[String,String]]()
    CudaExecutableGenerator.collectInputTypesMap(graph)
    CudaExecutableGenerator.makeExecutables(schedule.slice(Config.numThreads+Config.numCpp, Config.numThreads+Config.numCpp+Config.numCuda), graph.kernelPath)

    /*
    for (i <- Config.numThreads+Config.numCpp until Config.numThreads+Config.numCpp+Config.numCuda) {
      CudaExecutableGenerator.makeExecutables(schedule.slice(i, i+1), graph.kernelPath)
    }
    */

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
    DeliteMesosExecutor.classLoader = classLoader

    val queues = StaticSchedule(schedule.numResources)
    for (i <- 0 until schedule.numResources) {
      val cls = classLoader.loadClass("Executable"+i) //load the Executable class
      val executable = cls.getMethod("self").invoke(null).asInstanceOf[DeliteExecutable] //retrieve the singleton instance
      queues(i) += executable
    }
    queues
  }

}
