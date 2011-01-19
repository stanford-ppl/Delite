package ppl.delite.runtime

import codegen._
import executor._
import graph.ops.{EOP, Arguments}
import graph.{TestGraph, DeliteTaskGraph}
import java.io.File
import profiler._
import scheduler._
import tools.nsc.io.Directory

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 5:02:38 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Delite {

  private def printArgs(args: Array[String]) {
    if(args.size == 0) {
      println("Not enough arguments.\nUsage: [Launch Runtime Command] filename.deg arguments*")
      system.exit(-1)
    }
    println("Delite Runtime executing with the following arguments:")
    println(args.mkString(","))
  }

  private def printConfig {
    println("Delite Runtime executing with " + Config.numThreads + " CPU thread(s) and " + Config.numGPUs + " GPU(s)")
  }

  def main(args: Array[String]) {
    printArgs(args)

    printConfig

    //extract application arguments
    Arguments.args = args.drop(1)

    val scheduler = Config.scheduler match {
      case "SMPStaticScheduler" => new SMPStaticScheduler
      case "StressTest" => new DieRollStaticScheduler
      case "GPUOnlyStaticScheduler" => new GPUOnlyStaticScheduler
      case "default" => new SMPStaticScheduler
      case _ => throw new IllegalArgumentException("Requested scheduler is not recognized")
    }

    val executor = Config.executor match {
      case "SMPExecutor" => new SMPExecutor
      case "SMP+GPUExecutor" => new SMP_GPU_Executor
      case "default" => new SMPExecutor
      case _ => throw new IllegalArgumentException("Requested executor type is not recognized")
    }

    executor.init() //call this first because could take a while and can be done in parallel

    //load task graph
    val graph = loadDeliteDEG(args(0))
    //val graph = new TestGraph

    //load kernels & data structures
    loadScalaSources(graph)

    //schedule
    val schedule = scheduler.schedule(graph)

    //compile
    val executable = Compilers.compileSchedule(schedule, graph)

  //execute
    val numTimes = Config.numRuns
    for (i <- 1 to numTimes) {
      println("Beginning Execution Run " + i)
      PerformanceTimer.start("all", false)
      executor.run(executable) //TODO: need to reset the executables
      EOP.await //await the end of the application program
      PerformanceTimer.stop("all", false)
      PerformanceTimer.print("all")
      // check if we are timing another component
      if(Config.dumpStatsComponent != "all")
        PerformanceTimer.print(Config.dumpStatsComponent)
    }

    if(Config.dumpStats)
      PerformanceTimer.dumpStats

    executor.shutdown()

  }

  def loadDeliteDEG(filename: String) = {
    val file = new File(filename)
    if(file.isFile == false) throw new RuntimeException(filename + " doesn't appear to be a valid file")
    DeliteTaskGraph(file)
  }

  def loadScalaSources(graph: DeliteTaskGraph) {
    val sourceFiles = new Directory(new File(graph.kernelPath + java.io.File.separator + "scala" + java.io.File.separator)).deepFiles.filter(_.extension == "scala") //obtain all files in path
    for (file <- sourceFiles) ScalaCompile.addSourcePath(file.path)
  }

}








