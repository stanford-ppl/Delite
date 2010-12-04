package ppl.delite.runtime.codegen

import java.util.ArrayDeque
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.scheduler.{StaticSchedule, PartialSchedule}

/**
 * Author: Kevin J. Brown
 * Date: Dec 2, 2010
 * Time: 9:41:08 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Compilers {

  def compileSchedule(schedule: PartialSchedule, graph: DeliteTaskGraph): StaticSchedule = {
    //generate executable(s) for all the ops in each proc
    ExecutableGenerator.makeExecutables(schedule, graph.kernelPath)

    val classLoader = ScalaCompile.compile
    val queues = new Array[ArrayDeque[DeliteExecutable]](schedule.resources.length)
    for (i <- 0 until schedule.resources.length) {
      val cls = classLoader.loadClass("Executable"+i) //load the Executable class
      val executable = cls.getMethod("self").invoke(null).asInstanceOf[DeliteExecutable] //retrieve the singleton instance

      queues(i) = new ArrayDeque[DeliteExecutable]
      queues(i).add(executable)
    }

    CudaCompile.compile

    new StaticSchedule(queues)
  }

}
