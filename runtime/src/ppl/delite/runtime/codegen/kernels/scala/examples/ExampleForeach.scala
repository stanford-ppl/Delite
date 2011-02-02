package ppl.delite.runtime.codegen.kernels.scala.examples

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock

/**
 * Author: Kevin J. Brown
 * Date: 12/17/10
 * Time: 5:32 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * The Delite framework only provides the foreach function as a kernel to the runtime
 * The full foreach kernel must be provided, as illustrated below
 * This is an example of what a generated Foreach kernel for an Array should look like; it should not be invoked
 */

object ExampleForeachHeader {
  def apply(in0: Array[Int], in1: Array[Double], in2: Double) = new ExampleForeachHeader(in0, in1, in2)
}

final class ExampleForeachHeader(in0: Array[Int], in1: Array[Double], in2: Double) {

  //this is the apply method of another (kernel) object: shouldn't be generated
  def kernel_apply(in0: Array[Int], in1: Array[Double], in2: Double): Foreach = {
    new Foreach {
      def in = in0
      def foreach(elem: Int) { in1(elem) = (in1(elem-1) + in1(elem) + in1(elem+1))/3 }
      def sync(idx: Int) = List(in0(idx-1), in0(idx), in0(idx+1))
    }
  }

  abstract class Foreach {
    def in: Array[Int]
    def foreach(elem: Int)
    def sync(idx: Int): List[Any]
  }

  val closure = kernel_apply(in0, in1, in2)
  val lockMap = new ConcurrentHashMap[Any, ReentrantLock]
}

object ExampleForeach {

  def apply(foreach: ExampleForeachHeader) {
    val in = foreach.closure.in
    val size = in.size
    var i = size*2/4 //size*chunkIdx/numChunks
    val end = size*3/4 //size*(chunkIdx+1)/numChunks

    while (i < end) {
      val sync = foreach.closure.sync(i).sortBy(System.identityHashCode(_)) //TODO: optimize locking mechanism
      for (e <- sync) {
        foreach.lockMap.putIfAbsent(e, new ReentrantLock)
        foreach.lockMap.get(e).lock
      }

      foreach.closure.foreach(in(i))

      for (e <- sync.reverse) {
        foreach.lockMap.get(e).unlock
      }

      i += 1
    }
  }
}
