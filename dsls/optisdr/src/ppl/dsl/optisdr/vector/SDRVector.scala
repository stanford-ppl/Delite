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
    def conj(implicit sa: SDRArith[A], ctx: SourceContext) = sdrvector_conj(x)
    // def block exponent
    
    def unary_~()(implicit ctx: SourceContext) = sdrvector_binarynot(x)
    def &(y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = sdrvector_binaryand(x,y)
    def |(y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = sdrvector_binaryor(x,y)
    def ^(y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = sdrvector_binaryxor(x,y)
    
    def <<(y: Rep[Int])(implicit ctx: SourceContext) = sdrvector_lshift(x, y)
    def <<<(y: Rep[Int])(implicit ctx: SourceContext) = sdrvector_lshift(x, y)
    def >>(y: Rep[Int])(implicit ctx: SourceContext) = sdrvector_rshift(x, y)
    def >>>(y: Rep[Int])(implicit ctx: SourceContext) = sdrvector_rashift(x, y)
  }
  
  // infix convolve
  // infix fir
  // infix iir
  // infix correlation
  // lms
  // infix fft, ifft
  
  // sum squares (just capture the pattern?)
  // weighted vector sum
  
  def convolve[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) = sdrvector_convolve(x,y)
  
  def sdrvector_conj[A:Manifest:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_convolve[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_fft[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_lms[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_block_exp[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_correlation[A:Manifest:Arith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_auto_correlation[A:Manifest:Arith:SDRArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  
  def sdrvector_binarynot[A:Manifest:BitArith](x: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_binaryand[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_binaryor[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  def sdrvector_binaryxor[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[DenseVector[A]])(implicit ctx: SourceContext) : Rep[A]
  
  def sdrvector_lshift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_rshift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
  def sdrvector_rashift[A:Manifest:BitArith](x: Rep[DenseVector[A]], y: Rep[Int])(implicit ctx: SourceContext) : Rep[DenseVector[A]]
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
  
  case class SDRVectorBinaryNot[A:Manifest:BitArith](x: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class SDRVectorBinaryAnd[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class SDRVectorBinaryOr[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  case class SDRVectorBinaryXor[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[DenseVector[A]]) extends Def[DenseVector[A]]
  
  case class SDRVectorLShift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int]) extends Def[DenseVector[A]]
  case class SDRVectorRShift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int]) extends Def[DenseVector[A]]
  case class SDRVectorRAShift[A:Manifest:BitArith](x: Exp[DenseVector[A]], y: Exp[Int]) extends Def[DenseVector[A]]
  
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
}

trait SDRVectorOpsExpOpt extends SDRVectorOpsExp {
  this: OptiSDRExp =>
}