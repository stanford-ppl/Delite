package ppl.delite.runtime

import codegen._
import executor._
import graph.ops.{EOP_Global, Arguments}
import graph.targets.Targets
import graph.{TestGraph, DeliteTaskGraph}
import profiler.{PerformanceTimer, Profiler}
import scheduler._
import tools.nsc.io._

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 5:02:38 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Delite {

  private var mainThread: Thread = null

  private def printArgs(args: Array[String]) {
    if(args.length == 0) {
      println("Not enough arguments.\nUsage: [Launch Runtime Command] filename.deg arguments*")
      exit(-1)
    }
    println("Delite Runtime executing with the following arguments:")
    println(args.mkString(","))
  }

  private def printConfig() {
    println("Delite Runtime executing with " + Config.numThreads + " CPU thread(s) and " + Config.numGPUs + " GPU(s)")
  }

  def main(args: Array[String]) {
    embeddedMain(args, Map())
  }

  def embeddedMain(args: Array[String], staticData: Map[String,_]) {
    mainThread = Thread.currentThread
    
    printArgs(args)
    printConfig()

    //extract application arguments
    Arguments.args = args.drop(1)
    Arguments.staticDataMap = staticData

    val scheduler = Config.scheduler match {
      case "SMP" => new SMPStaticScheduler
      case "SMP+GPU" => new SMP_GPU_StaticScheduler
      case "default" => {
        if (Config.numGPUs == 0) new SMPStaticScheduler
        else if (Config.numGPUs == 1) new SMP_GPU_StaticScheduler
        else error("No scheduler currently exists that can handle the requested resources")
      }
      case _ => throw new IllegalArgumentException("Requested scheduler is not recognized")
    }

    val executor = Config.executor match {
      case "SMP" => new SMPExecutor
      case "SMP+GPU" => new SMP_GPU_Executor
      case "OpenCLExecutor" => new OpenCLExecutor     //TODO: Remove this option after debugging
      case "default" => {
        if (Config.numGPUs == 0) new SMPExecutor
        else new SMP_GPU_Executor
      }
      case _ => throw new IllegalArgumentException("Requested executor is not recognized")
    }

    def abnormalShutdown() {
      executor.shutdown()
      if (!Config.noRegenerate)
        Directory(Path(Config.codeCacheHome)).deleteRecursively() //clear the code cache (could be corrupted)
    }

    try {

      executor.init() //call this first because could take a while and can be done in parallel

      //load task graph
      val graph = loadDeliteDEG(args(0))
      //val graph = new TestGraph
      Config.deliteBuildHome = graph.kernelPath

      //load kernels & data structures
      loadSources(graph)

      //initialize profiler
      Profiler.init(sourceInfo, graph)
      
      //schedule
      scheduler.schedule(graph)

      //compile
      val executable = Compilers.compileSchedule(graph)

      //execute
      val numTimes = Config.numRuns
      for (i <- 1 to numTimes) {
        println("Beginning Execution Run " + i)
        val globalStart = System.currentTimeMillis
        val globalStartNanos = System.nanoTime()
        PerformanceTimer.start("all", false)
        executor.run(executable)
        EOP_Global.await //await the end of the application program
        PerformanceTimer.stop("all", false)
        PerformanceTimer.printAll(globalStart, globalStartNanos)
        if (Config.dumpProfile) PerformanceTimer.dumpProfile(globalStart, globalStartNanos)
        if (Config.dumpStats) PerformanceTimer.dumpStats()        
	    System.gc()
      }

      //println("Done Executing " + numTimes + " Runs")
      
      if(Config.dumpStats)
        PerformanceTimer.dumpStats()

      executor.shutdown()
    }
    catch {
      case i: InterruptedException => abnormalShutdown(); throw i //a worker thread threw the original exception        
      case e: Exception => abnormalShutdown(); throw e       
    }
    finally {
      Arguments.args = null
      Arguments.staticDataMap = null
    }
  }

  def loadDeliteDEG(filename: String) = {
    val deg = Path(filename)
    if (!deg.isFile)
      error(filename + " does not exist")
    DeliteTaskGraph(deg.jfile)
  }

  def loadSources(graph: DeliteTaskGraph) {
    if (graph.targets contains Targets.Scala)
      ScalaCompile.cacheDegSources(Directory(Path(graph.kernelPath + File.separator + ScalaCompile.target + File.separator).toAbsolute))
    if (graph.targets contains Targets.Cuda) {
      CudaCompile.cacheDegSources(Directory(Path(graph.kernelPath + File.separator + CudaCompile.target + File.separator).toAbsolute))
    }
    if (graph.targets contains Targets.OpenCL)
      OpenCLCompile.cacheDegSources(Directory(Path(graph.kernelPath + File.separator + OpenCLCompile.target + File.separator).toAbsolute))
  }

  //abnormal shutdown
  def shutdown() {
    mainThread.interrupt()
  }

  // maps op ids to the op's source info (fileName, line, opName)
  // used in the profiler
  var sourceInfo: Map[String, (String, Int, String)] = Map()
}
