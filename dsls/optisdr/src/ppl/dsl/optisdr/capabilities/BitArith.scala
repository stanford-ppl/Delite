package ppl.dsl.optisdr.capabilities

import java.io.PrintWriter

import scala.reflect.SourceContext

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._

import scala.virtualization.lms.internal.{CLikeCodegen}

import ppl.dsl.optisdr._
import ppl.dsl.optila.DenseVector

/*
 * Bit math operations for supported types. This should be all integer types as well as Bit types. Not sure we should support floats
 * unless they are converted to a known format!
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

/* Type class for basic math, but less restrictive than Numeric. */

trait BitArithInternal[Rep[X],T] {
  def unary_~(a: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def &(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def |(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  def ^(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) : Rep[T]
  
  // The double </> operators have the notion that for bitstreams, bits will carry over
  // Should I make it ... Rep[IntLike]?
  def <<(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) : Rep[T]
  def <<<(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) : Rep[T]
  def >>(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) : Rep[T]
  def >>>(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) : Rep[T]
}

trait BitArithOps extends Variables with OverloadHack {
  this: OptiSDR =>
  
  type BitArith[X] = BitArithInternal[Rep,X]

  /**
   * Interface, enables using BitAriths with operator notation.
   */
  implicit def bitArithToBitArithOps[T:BitArith:Manifest](n: T) = new BitArithOpsCls(unit(n))
  implicit def repBitArithToBitArithOps[T:BitArith:Manifest](n: Rep[T]) = new BitArithOpsCls(n)
  implicit def varBitArithToBitArithOps[T:BitArith:Manifest](n: Var[T]) = new BitArithOpsCls(readVar(n))

  class BitArithOpsCls[T](lhs: Rep[T])(implicit mT: Manifest[T], arith: BitArith[T]){
    def unary_~(a: Rep[T])(implicit ctx: SourceContext) = arith.unary_~(a)
    def &(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) = arith.&(a, b)
    def |(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) = arith.|(a, b)
    def ^(a: Rep[T], b: Rep[T])(implicit ctx: SourceContext) = arith.^(a, b)
    
    def <<(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) = arith.<<(a,b)
    def <<<(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) = arith.<<(a,b)
    def >>(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) = arith.>>(a,b)
    def >>>(a: Rep[T], b: Rep[Int])(implicit ctx: SourceContext) = arith.>>>(a,b)
  }
  
  implicit val intBitArith : BitArith[Int] = new BitArith[Int] {
    def unary_~(a: Rep[Int])(implicit ctx: SourceContext) = ~a
    def &(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = a & b
    def |(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = a | b
    def ^(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = a ^ b
    
    def <<(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = a << b
    def <<<(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = a << b
    def >>(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = a >> b
    def >>>(a: Rep[Int], b: Rep[Int])(implicit ctx: SourceContext) = a >>> b
  }
  
  implicit val uintBitArith : BitArith[UInt] = new BitArith[UInt] {
    def unary_~(a: Rep[UInt])(implicit ctx: SourceContext) = ~a
    def &(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = a & b
    def |(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = a | b
    def ^(a: Rep[UInt], b: Rep[UInt])(implicit ctx: SourceContext) = a ^ b
		
    def <<(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) = a << b
    def <<<(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) = a << b
    def >>(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) = a >> b
    def >>>(a: Rep[UInt], b: Rep[Int])(implicit ctx: SourceContext) = a >>> b
  }
  
  implicit def denseVectorBitArith[T:BitArith:Manifest]: BitArith[DenseVector[T]] = new BitArith[DenseVector[T]] {
    def unary_~(a: Rep[DenseVector[T]])(implicit ctx: SourceContext) = ~repToSDRVectorOps(a)
    def &(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToSDRVectorOps(a) & b
    def |(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToSDRVectorOps(a) | b
    def ^(a: Rep[DenseVector[T]], b: Rep[DenseVector[T]])(implicit ctx: SourceContext) = repToSDRVectorOps(a) ^ b
    
    def <<(a: Rep[DenseVector[T]], b: Rep[Int])(implicit ctx: SourceContext) = repToSDRVectorOps(a) << b
    def <<<(a: Rep[DenseVector[T]], b: Rep[Int])(implicit ctx: SourceContext) = repToSDRVectorOps(a) << b
    def >>(a: Rep[DenseVector[T]], b: Rep[Int])(implicit ctx: SourceContext) = repToSDRVectorOps(a) >> b
    def >>>(a: Rep[DenseVector[T]], b: Rep[Int])(implicit ctx: SourceContext) = repToSDRVectorOps(a) >>> b
  }
  
  /* implicit def tuple2BitArith[A:Manifest:BitArith,B:Manifest:BitArith] : BitArith[Tuple2[A,B]] = new BitArith[Tuple2[A,B]] {
    def unary_~(a: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) = Tuple2(~a._1, ~a._2)
    def &(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) = Tuple2(a._1 & b._1, a._2 & b._2)
    def |(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) = Tuple2(a._1 | b._1, a._2 | b._2)
    def ^(a: Rep[Tuple2[A,B]], b: Rep[Tuple2[A,B]])(implicit ctx: SourceContext) = Tuple2(a._1 ^ b._1, a._2 ^ b._2)
    
    def <<(a: Rep[Tuple2[A,B]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple2(a._1 << b, a._2 << b)
    def <<<(a: Rep[Tuple2[A,B]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple2(a._1 << b, a._2 << b)
    def >>(a: Rep[Tuple2[A,B]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple2(a._1 >> b, a._2 >> b)
    def >>>(a: Rep[Tuple2[A,B]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple2(a._1 >>> b, a._2 >>> b)
  }
  
  implicit def tuple3BitArith[A:Manifest:BitArith,B:Manifest:BitArith,C:Manifest:BitArith] : BitArith[Tuple3[A,B,C]] = new BitArith[Tuple3[A,B,C]] {
    def unary_~(a: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) = Tuple3(~a._1, ~a._2, ~a._3)
    def &(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) = Tuple3(a._1 & b._1, a._2 & b._2, a._3 & b._3)
    def |(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) = Tuple3(a._1 | b._1, a._2 | b._2, a._3 | b._3)
    def ^(a: Rep[Tuple3[A,B,C]], b: Rep[Tuple3[A,B,C]])(implicit ctx: SourceContext) = Tuple3(a._1 ^ b._1, a._2 ^ b._2, a._3 ^ b._3)
    
    def <<(a: Rep[Tuple3[A,B,C]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple3(a._1 << b, a._2 << b, a._3 << b)
    def <<<(a: Rep[Tuple3[A,B,C]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple3(a._1 << b, a._2 << b, a._3 << b)
    def >>(a: Rep[Tuple3[A,B,C]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple3(a._1 >> b, a._2 >> b, a._3 >> b)
    def >>>(a: Rep[Tuple3[A,B,C]], b: Rep[Int])(implicit ctx: SourceContext) = Tuple3(a._1 >>> b, a._2 >>> b, a._3 >>> b)
  }
  
  implicit def tuple4BitArith[A:Manifest:BitArith,B:Manifest:BitArith,C:Manifest:BitArith,D:Manifest:BitArith] : BitArith[Tuple4[A,B,C,D]] = new BitArith[Tuple4[A,B,C,D]] {
    def unary_~(a: Rep[Tuple4[A,B,C]])(implicit ctx: SourceContext) = Tuple4(~a._1, ~a._2, ~a._3, ~a._4)
    def &(a: Rep[Tuple4[A,B,C]], b: Rep[Tuple4[A,B,C]])(implicit ctx: SourceContext) = Tuple4(a._1 & b._1, a._2 & b._2, a._3 & b._3, a._4 & b._4)
    def |(a: Rep[Tuple4[A,B,C]], b: Rep[Tuple4[A,B,C]])(implicit ctx: SourceContext) = Tuple4(a._1 | b._1, a._2 | b._2, a._3 | b._3, a._4 | b._4)
    def ^(a: Rep[Tuple4[A,B,C]], b: Rep[Tuple4[A,B,C]])(implicit ctx: SourceContext) = Tuple4(a._1 ^ b._1, a._2 ^ b._2, a._3 ^ b._3, a._4 ^ b._4)
  } */
}

trait BitArithOpsExp extends BitArithOps with VariablesExp {
  this: OptiSDRExp => 
}