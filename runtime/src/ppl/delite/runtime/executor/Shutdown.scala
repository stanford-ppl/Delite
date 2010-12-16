package ppl.delite.runtime.executor

import ppl.delite.runtime.codegen.DeliteExecutable

/**
 * Author: Kevin J. Brown
 * Date: 12/14/10
 * Time: 1:07 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This Executable will cause an ExecutionThread to terminate immediately after its execution
 */

final class Shutdown(thread: ExecutionThread) extends DeliteExecutable {

  def run() {
    thread.continue = false
  }

}
