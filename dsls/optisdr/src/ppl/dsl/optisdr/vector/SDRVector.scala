package ppl.dsl.optisdr.vector

import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import ppl.dsl.optila.DenseVector

import ppl.dsl.optisdr._

trait SDRVectorOps extends Variables {
  this: OptiSDR =>
  
  // Convert from Rep[Vector] to our Vector ops
  implicit def repToSDRVectorOps[A:Manifest](x: Rep[DenseVector[A]]) = new SDRVectorOpsCls(x)
  implicit def varToSDRVectorOps[A:Manifest](x: Var[DenseVector[A]]) = new SDRVectorOpsCls(readVar(x))
  
  // Objects methods
  class SDRVectorOpsCls[A:Manifest](val x: Rep[DenseVector[A]]) {
	def conj(implicit sa: SDRArith[A], ctx: SourceContext) = vector_conj(x)
    // def block exponent
  }
  
  // infix convolve
  // infix fir
  // infix iir
  // infix correlation
  // lms
  // infix fft, ifft
  
  // sum squares (just capture the pattern?)
  // weighted vector sum
  
  def convolve[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = vector_convolve(x,y)
  
  def vector_conj[A:Manifest:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def vector_convolve[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def vector_fft[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def vector_lms[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def vector_block_exp[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def vector_correlation[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def vector_auto_correlation[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
}

trait SDRVectorOpsExp extends SDRVectorOps {
  this: OptiSDRExp =>
  
  case class SDRVectorConj[A:Manifest:SDRArith](x: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class SDRVectorConvolve[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class SDRVectorFFT[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class SDRVectorLMS[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]]) extends Def[A]
  case class SDRVectorBlockExponent[A:Manifest:Arith](x: Exp[DenseVector[A]]) extends Def[A]
  case class SDRVectorCorrelation[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) extends Def[A]
  case class SDRVectorAutoCorrelation[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]]) extends Def[A]
  
  def vector_conj[A:Manifest:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorConj(x))
  def vector_convolve[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorConvolve(x,y))
  def vector_fft[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorFFT(x))
  def vector_lms[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorLMS(x))
  def vector_block_exp[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorBlockExponent(x))
  def vector_correlation[A:Manifest:Arith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorCorrelation(x,y))
  def vector_auto_correlation[A:Manifest:Arith:SDRArith](x: Exp[DenseVector[A]])(implicit ctx: SourceContext) = reflectPure(SDRVectorAutoCorrelation(x))
}

trait SDRVectorOpsExpOpt extends SDRVectorOpsExp {
  this: OptiSDRExp =>
}