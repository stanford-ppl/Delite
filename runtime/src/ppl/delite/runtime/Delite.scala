package ppl.delite.runtime

import runtime.executor.SMPExecutor
import walktime.graph.{DeliteTaskGraph, TestGraph}
import walktime.scheduler.SMPStaticScheduler
import java.io.File

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
      exit(-1)
    }
    println("Delite Runtime executing with following arguments:")
    println(args.mkString(","))
  }

  def main(args: Array[String]) {
    printArgs(args)

    val scheduler = Config.scheduler match {
      case "SMPStaticScheduler" => new SMPStaticScheduler
      case "default" => new SMPStaticScheduler
      case _ => throw new IllegalArgumentException("Requested scheduler is not recognized")
    }

    val executor = Config.executor match {
      case "SMPExecutor" => new SMPExecutor
      case "default" => new SMPExecutor
      case _ => throw new IllegalArgumentException("Requested executor type is not recognized")
    }

    //load task graph
    //TODO: this is a compile hack
    val graph = loadDeliteDEG(args(0))  


    //load kernels

    //load data structures

    //schedule
    val schedule = scheduler.schedule(graph)

    //execute
    executor.run(schedule)
    
  }

  def loadDeliteDEG(filename: String) = {
    val file = new File(filename)
    if(file.isFile == false) throw new RuntimeException(filename + " doesn't appear to be a valid file")
    DeliteTaskGraph(file)
    // REMOVE FOLLOWING LINE TO ACTUALLY USED DEG FILE
    new TestGraph     
  }
}