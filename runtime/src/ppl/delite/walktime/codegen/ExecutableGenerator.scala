package ppl.delite.walktime.codegen

import java.io.{StringWriter, PrintWriter}
import java.util.ArrayDeque
import ppl.delite.walktime.graph.ops.DeliteOP
import reflect.ClassManifest

/**
 * Author: Kevin J. Brown
 * Date: Oct 26, 2010
 * Time: 8:19:19 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ExecutableGenerator {

  /**
   * inputs: simple := generate getter for every dependency
   * inputs: optimized := generate getter for dependencies from other threads, use local variable for dependencies in same generated block
   *
   * outputs: simple := generate setter for every dependency
   * outputs: optimized := generate setter only for outputs that need to be communicated (saves locking & publishing result)
   */

  def makeExecutable(opQueue: ArrayDeque[DeliteOP], num: Int) {

    val stream = new PrintWriter(new StringWriter)

    /**
     * the header
     */
    stream.println("import java.util.concurrent.locks._") //locking primitives
    stream.println("object Executable" + num + " extends DeliteExecutable {")

    /**
     * the run() method
     */
    stream.println("def run() {")
    for (i <- 0 until opQueue.size) {
      val op = opQueue.poll
      //get dependencies

      //execute
      stream.println("val x"+i+" = "+"Hello World")

      //set result
      stream.println("")
      
    }
    stream.println("}")

    /**
     * the communication
     */
    //TODO: OPTIMIZE (see above)
    //TODO: add a Manifest in order to inject the proper type
    //TODO: how do we iterate through the elements in the list multiple times efficiently (already iterated above)
    //TODO: consider optimize version: maybe we empty out queue to generate run(); creating new lists for what needs to be done here as we go, then iterate through new list(s) here
    for (i <- 0 until opQueue.size) {
      stream.println("private object Result"+i+" {")

      //the state
      stream.println("private var notReady: Boolean = true")
      stream.println("private var _result: " + "String" + " = _")
      stream.println("private val lock = new ReentrantLock")
      stream.println("private val condition = lock.newCondition")

      //the getter
      stream.println("def get: " + "String" + " = if (notReady) block; _result")
      stream.println("def block { lock.lock; try { while(notReady) { condition.await } } finally { lock.unlock } }")

      //the setter
      stream.println("def set(result: "+"String"+") { lock.lock; try { _result = result; notReady = false; condition.signalAll } finally { lock.unlock } }")

      stream.println("}")
    }

    /**
     *    the footer
     */
    stream.println("}")
    stream.flush


  }
}