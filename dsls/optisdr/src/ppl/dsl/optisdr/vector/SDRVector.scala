package ppl.dsl.optisdr.vector

import java.io._

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

import ppl.dsl.optila.DenseVector

import ppl.dsl.optisdr._

trait SDRVectorOps extends Variables {
  this: OptiSDR =>
  
  // Convert from Rep[Vector] to our Vector ops
  implicit def repToSDRVectorOps[A:Manifest](x: Rep[DenseVector[A]]) = new SDRVectorOpsCls(x)
  implicit def varToSDRVectorOps[A:Manifest](x: Var[DenseVector[A]]) = new SDRVectorOpsCls(readVar(x))
  
  // Objects methods
  class SDRVectorOpsCls[A:Manifest](val x: Rep[DenseVector[A]]) {
    def conj(implicit sa: SDRArith[A], ctx: SourceContext) = sdrvector_conj(x)
    // def block exponent
    
    def unary_~()(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_binarynot(x)
    def &(y: Rep[DenseVector[A]])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_binaryand(x,y)
    def |(y: Rep[DenseVector[A]])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_binaryor(x,y)
    def ^(y: Rep[DenseVector[A]])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_binaryxor(x,y)
    
    def <<(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_lshift(x, y)
    def <<<(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_lshift(x, y)
    def >>(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_rshift(x, y)
    def >>>(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_rashift(x, y)
    
    def !<<(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_lvshift(x, y)
    def !>>(y: Rep[Int])(implicit ba: BitArith[A], ctx: SourceContext) = sdrvector_rvshift(x, y)
  }
  
  // infix convolve
  // infix fir
  // infix iir
  // infix correlation
  // infix fft, ifft
  
  // sum squares (just capture the pattern?)
  // weighted vector sum
  
  def convolve[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = sdrvector_convolve(x,y)
  
  def sdrvector_conj[A:Manifest:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_convolve[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_fft[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  
  // Square and add
  def sdrvector_lms[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  
  def sdrvector_block_exp[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_correlation[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_auto_correlation[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  
  def sdrvector_binarynot[A:Manifest:BitArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_binaryand[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_binaryor[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_binaryxor[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  
  def sdrvector_lshift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_rshift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_rashift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  
  def sdrvector_lvshift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_rvshift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
}

trait SDRVectorOpsExp extends SDRVectorOps with VariablesExp with BaseFatExp {
  this: OptiSDRExp =>
  
  case class SDRVectorConj[A:Manifest:SDRArith](in: Exp[DenseVector[A]])
    extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => implicitly[SDRArith[A]].conj(e)

    val mA = manifest[A]
  }
  
  case class SDRVectorConvolve[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class SDRVectorFFT[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  
  case class SDRVectorLMS[A:Manifest:Arith:SDRArith](in: Exp[DenseVector[A]]) extends DeliteOpMapReduce[A,A] {
    val size = copyTransformedOrElse(_.size)(in.length)
    val zero = implicitly[Arith[A]].empty
    def map = e => e*e
    def reduce = (a,b) => a + b
    
    val mA = manifest[A]
  }
  
  case class SDRVectorBlockExponent[A:Manifest:Arith](x: Exp[DenseVector[A]]) extends Def[A]
  case class SDRVectorCorrelation[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) extends Def[A]
  case class SDRVectorAutoCorrelation[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]]) extends Def[A]
  
  case class SDRVectorBinaryNot[A:Manifest:BitArith](in: Exp[DenseVector[A]])
    extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => implicitly[BitArith[A]].unary_~(e)

    val mA = manifest[A]
  }
  
  case class SDRVectorBinaryAnd[A:Manifest:BitArith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
    extends DeliteOpZipWith[A,A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(inA.length)

    def alloc = DenseVector[A](inA.length, !inA.isRow)
    def func = (a,b) => implicitly[BitArith[A]].&(a,b)

    val mA = manifest[A]
  }
  
  case class SDRVectorBinaryOr[A:Manifest:BitArith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
      extends DeliteOpZipWith[A,A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(inA.length)

    def alloc = DenseVector[A](inA.length, !inA.isRow)
    def func = (a,b) => implicitly[BitArith[A]].|(a,b)

    val mA = manifest[A]
  }
  
  case class SDRVectorBinaryXor[A:Manifest:BitArith](inA: Exp[DenseVector[A]], inB: Exp[DenseVector[A]])
      extends DeliteOpZipWith[A,A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(inA.length)

    def alloc = DenseVector[A](inA.length, !inA.isRow)
    def func = (a,b) => implicitly[BitArith[A]].^(a,b)

    val mA = manifest[A]
  }
  
  case class SDRVectorLShift[A:Manifest:BitArith](in: Exp[DenseVector[A]], y: Exp[Int])
      extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => implicitly[BitArith[A]].<<(e, y)

    val mA = manifest[A]
  }
  
  case class SDRVectorRShift[A:Manifest:BitArith](in: Exp[DenseVector[A]], y: Exp[Int])
      extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => implicitly[BitArith[A]].>>(e, y)

    val mA = manifest[A]
  }
  
  case class SDRVectorRAShift[A:Manifest:BitArith](in: Exp[DenseVector[A]], y: Exp[Int])
      extends DeliteOpMap[A,A,DenseVector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)

    def alloc = DenseVector[A](in.length, !in.isRow)
    def func = e => implicitly[BitArith[A]].>>>(e, y)

    val mA = manifest[A]
  }
  
  // Only these shifts cross element boundaries. Based on element size, access previous and next... not sure how to do this..
  case class SDRVectorLVShift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int]) extends Def[DenseVector[A]]
  case class SDRVectorRVShift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int]) extends Def[DenseVector[A]]
  
  def sdrvector_conj[A:Manifest:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorConj(x))
  def sdrvector_convolve[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorConvolve(x,y))
  def sdrvector_fft[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorFFT(x))
  def sdrvector_lms[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorLMS(x))
  def sdrvector_block_exp[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorBlockExponent(x))
  def sdrvector_correlation[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorCorrelation(x,y))
  def sdrvector_auto_correlation[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorAutoCorrelation(x))
  
  def sdrvector_binarynot[A:Manifest:BitArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorBinaryNot(x))
  def sdrvector_binaryand[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorBinaryAnd(x,y))
  def sdrvector_binaryor[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorBinaryOr(x,y))
  def sdrvector_binaryxor[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorBinaryXor(x,y))
  
  def sdrvector_lshift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRVectorLShift(x,y))
  def sdrvector_rshift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRVectorRShift(x,y))
  def sdrvector_rashift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRVectorRAShift(x,y))
  
  def sdrvector_lvshift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRVectorLVShift(x,y))
  def sdrvector_rvshift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int])(implicit ctx: SourceContext) = reflectPure(SDRVectorRVShift(x,y))
}

trait SDRVectorOpsExpOpt extends SDRVectorOpsExp {
  this: OptiSDRExp =>
}

trait BaseGenSDRVectorOps extends GenericFatCodegen {
  val IR: SDRVectorOpsExp
  import IR._
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    // What is this for???
    case _ => super.unapplySimpleIndex(e)
  }  
}

trait ScalaGenSDRVectorOps extends BaseGenSDRVectorOps with ScalaGenFat {
  val IR: SDRVectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    
    // case v@DenseVectorEmpty() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[" + remap(v.mA) + "]")+"(0,true)")
    
    case _ => super.emitNode(sym, rhs)
  }
}