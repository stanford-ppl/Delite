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

object TestKernelPrint {
  def apply(result: Int) { println(result) }
}

object TestKernelEnd {
  def apply(out: Array[Int]) = {
    print("[ ")
    for (e <- out) print(e + " ")
    print("]\n")
  }
}

abstract class DeliteOPMap[A,B, CA,CB] {
  def in: CA
  def out: CB
  def map(a: A): B
}

object TestKernelMap {
  def apply(in0: Array[Int], in1: Array[Int]): DeliteOPMap[Int,Int, Array[Int],Array[Int]] = {
    new DeliteOPMap[Int,Int, Array[Int],Array[Int]] {
      def in = in1
      def out = in0
      def map(a: Int) = a + 1
    }
  }
}

abstract class DeliteOPReduce[R, CA] {
  def in: CA
  def reduce(r1: R, r2: R): R
}

object TestKernelReduce {
  def apply(in0: Array[Int]): DeliteOPReduce[Int, Array[Int]] = {
    new DeliteOPReduce[Int, Array[Int]] {
      def in = in0
      def reduce(r1: Int, r2: Int) = r1 + r2
    }
  }
}

abstract class DeliteOPZip[A,B,R, CA,CB,CR] {
  def inA: CA
  def inB: CB
  def out: CR
  def zip(a: A, b: B): R
}

object TestKernelZip {
  def apply(in0: Array[Int], in1: Array[Int], in2: Array[Int]): DeliteOPZip[Int,Int,Int, Array[Int],Array[Int],Array[Int]] = {
    new DeliteOPZip[Int,Int,Int, Array[Int],Array[Int],Array[Int]] {
      def inA = in1
      def inB = in2
      def out = in0
      def zip(a: Int, b: Int) = a + b
    }
  }
}

abstract class DeliteOPMapReduce[A,R, CA] {
  def in: CA
  def map(elem: A): R
  def reduce(r1: R, r2: R): R
  def mapreduce(acc: R, elem: A) = reduce(acc, map(elem))
}

object TestKernelMapReduce {
  def apply(in0: Array[Int]): DeliteOPMapReduce[Int,Int, Array[Int]] = {
    new DeliteOPMapReduce[Int,Int, Array[Int]] {
      def in = in0
      def map(elem: Int): Int = elem * elem
      def reduce(acc: Int, elem: Int): Int = acc + elem
    }
  }
}
