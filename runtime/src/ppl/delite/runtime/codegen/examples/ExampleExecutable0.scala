package ppl.delite.runtime.codegen.examples

import ppl.delite.runtime.data.Data
import ppl.delite.runtime.codegen.DeliteExecutable
import ppl.delite.runtime.graph.ops.EOP_Kernel

/**
 * Author: Kevin J. Brown
 * Date: Oct 25, 2010
 * Time: 11:24:51 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This is an example of what a generated DeliteExecutable should look like; it should not be invoked
 * Ex0 shows how to consume results from other threads
 * Ex1 shows how to produce results for other threads
 */

object ExampleExecutable0 extends DeliteExecutable {

  //the other executable objects this object can communicate with
  val thread1 = ExampleExecutable1
  //val thread2 = ExampleExecutable2

  def run() {
    val x1 = kernel1_run(0) //to be replaced with a call to the appropriate kernel object

    val x2 = thread1.get1 //this call blocks
    val x3 = kernel2_run(x1,x2)

    val x4 = thread1.get2
    val x5 = kernel3_run(x3,x4)

    val x6 = EOP_Kernel()

  }

  //ignore everything below
  def kernel1_run(arg0: Int): Data[Int] = null

  def kernel2_run(arg0: Data[Int], arg1: Data[Int]): Data[Int] = null

  def kernel3_run(arg0: Data[Int], arg1: Int): Data[Int] = null

}