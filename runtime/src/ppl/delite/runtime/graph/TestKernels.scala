package ppl.delite.runtime.graph

/**
 * Author: Kevin J. Brown
 * Date: Nov 9, 2010
 * Time: 3:17:02 AM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object TestKernel1a {
  def apply() = println("op1a")
}

object TestKernel1b {
  def apply() = println("op1b")
}

object TestKernel1c {
  def apply() = println("op1c")
}

object TestKernel1d {
  def apply() = println("op1d")
}

object TestKernel2a {
  def apply() = println("op2a")
}

object TestKernel2b {
  def apply() = println("op2b")
}

object TestKernel2c {
  def apply() = println("op2c")
}

object TestKernel2d {
  def apply() = println("op2d")
}

object TestKernel3 {
  def apply() = println("op3")
}

object TestKernelBegin {
  def apply() = Array[Int](0,1,2,3,4,5,6,7,8)
}

object TestKernelMap {
  def apply(e: Int) = e + 1
}

object TestKernelReduce {
  def apply(left: Int, right: Int) = left + right
}

object TestKernelPrint {
  def apply(result: Int) { println(result) }
}

object TestKernelEnd {
  def apply(out: Array[Int]) = {
    for (e <- out) print(e)
    print('\n')
  }
}

abstract class DeliteOPMapReduce[C, A, R] {
  def in: C
  def map(elem: A): R
  def reduce(acc: R, elem: R): R
}

object TestKernelMapReduce {
  def apply(in0: Array[Int]): DeliteOPMapReduce[Array[Int],Int,Int] = {
    new DeliteOPMapReduce[Array[Int],Int,Int] {
      def in = in0
      def map(elem: Int): Int = elem * elem
      def reduce(acc: Int, elem: Int): Int = acc + elem
    }
  }
}
