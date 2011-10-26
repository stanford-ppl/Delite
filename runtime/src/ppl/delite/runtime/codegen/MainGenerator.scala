package ppl.delite.runtime.codegen

import collection.mutable.ArrayBuffer
import ppl.delite.runtime.graph.ops.DeliteOP
import ppl.delite.runtime.scheduler.PartialSchedule
import ppl.delite.runtime.graph.targets.Targets
import ppl.delite.runtime.Config

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