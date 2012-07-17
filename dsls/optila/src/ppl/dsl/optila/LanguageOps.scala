package ppl.dsl.optila

import datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import reflect.Manifest
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

/* Machinery provided by OptiLA itself (language features and control structures).
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Nov 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait LanguageOps extends Base { this: OptiLA =>

  /**
   * random
   */
  // this version is for optila's use exclusively, so it does not interfere with application behavior
  private def _random[A](implicit mA: Manifest[A]): Rep[A] =
    mA match {
      case Manifest.Double => optila_internal_rand_double.AsInstanceOf[A]
      case Manifest.Float => optila_internal_rand_float.AsInstanceOf[A]
      case Manifest.Int => optila_internal_rand_int.AsInstanceOf[A]
      case Manifest.Long => optila_internal_rand_long.AsInstanceOf[A]
      case Manifest.Boolean => optila_internal_rand_boolean.AsInstanceOf[A]
      case _ => throw new UnsupportedOperationException()
  }

  // public version for application use
  def random[A](implicit mA: Manifest[A], ctx: SourceContext): Rep[A] =
    mA match {
      case Manifest.Double => optila_rand_double.AsInstanceOf[A]
      case Manifest.Float => optila_rand_float.AsInstanceOf[A]
      case Manifest.Int => optila_rand_int.AsInstanceOf[A]
      case Manifest.Long => optila_rand_long.AsInstanceOf[A]
      case Manifest.Boolean => optila_rand_boolean.AsInstanceOf[A]
      case _ => throw new UnsupportedOperationException()
  }

  def random(max: Rep[Int])(implicit ctx: SourceContext): Rep[Int] = optila_rand_int_max(max)

  def random[A:Manifest](elems: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A] = optila_rand_elem(elems)
  
  def randomGaussian(implicit ctx: SourceContext) = optila_rand_gaussian

  def reseed(implicit ctx: SourceContext) {
    // reseeds for all threads
    optila_reseed()
  }
     
  def identityHashCode(x:Rep[Any])(implicit ctx: SourceContext): Rep[Int]

  def optila_internal_rand_double(): Rep[Double]
  def optila_internal_rand_float(): Rep[Float]
  def optila_internal_rand_int(): Rep[Int]
  def optila_internal_rand_long(): Rep[Long]
  def optila_internal_rand_boolean(): Rep[Boolean]

  def optila_rand_double()(implicit ctx: SourceContext): Rep[Double]
  def optila_rand_float()(implicit ctx: SourceContext): Rep[Float]
  def optila_rand_int()(implicit ctx: SourceContext): Rep[Int]
  def optila_rand_int_max(max: Rep[Int])(implicit ctx: SourceContext): Rep[Int]
  def optila_rand_long()(implicit ctx: SourceContext): Rep[Long]
  def optila_rand_boolean()(implicit ctx: SourceContext): Rep[Boolean]
  def optila_rand_gaussian()(implicit ctx: SourceContext): Rep[Double]
  def optila_rand_elem[A:Manifest](elems: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  
  def optila_reseed()(implicit ctx: SourceContext): Rep[Unit]

  /**
   * range 
   */  
  implicit def intToRangeOp(i: Int) = new RangeOp(unit(i))
  implicit def repIntToRangeOp(i: Rep[Int]) = new RangeOp(i)

  class RangeOp(val _end : Rep[Int]) {
    def ::(_start : Rep[Int])(implicit ctx: SourceContext) = Vector.range(_start, _end)
  }
  
  /**
   * sum
   */
  def sum[A:Manifest:Arith](vals: Interface[Vector[A]])(implicit ctx: SourceContext) = vals.sum
  def sum[A:Manifest:Arith](vals: Interface[Matrix[A]])(implicit ctx: SourceContext, o: Overloaded1) = vals.sum
    
  /**
   * min
   */
  def min[A:Manifest:Ordering:HasMinMax,V[X] <: Vector[X]](vals: Rep[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], ctx: SourceContext) = toIntf(vals).min
  def min[A:Manifest:Ordering:HasMinMax,M[X] <: Matrix[X]](vals: Rep[M[A]])(implicit toIntf: Rep[M[A]] => Interface[Matrix[A]], ctx: SourceContext, o: Overloaded1) = toIntf(vals).min  
  def min[A:Manifest:Ordering:HasMinMax](vals: Interface[Vector[A]])(implicit ctx: SourceContext) = vals.min
  def min[A:Manifest:Ordering:HasMinMax](vals: Interface[Matrix[A]])(implicit ctx: SourceContext, o: Overloaded1) = vals.min
  //def min[A:Manifest:Ordering:HasMinMax](vals: A*) = repVecToVecOps(Vector(vals: _*)).min
  def min[A:Manifest:Ordering:HasMinMax](vals: Rep[A]*)(implicit ctx: SourceContext) = repToDenseVecOps(DenseVector(vals: _*)).min

  /**
   * max
   */
  def max[A:Manifest:Ordering:HasMinMax,V[X] <: Vector[X]](vals: Rep[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], ctx: SourceContext) = toIntf(vals).max
  def max[A:Manifest:Ordering:HasMinMax,M[X] <: Matrix[X]](vals: Rep[M[A]])(implicit toIntf: Rep[M[A]] => Interface[Matrix[A]], ctx: SourceContext, o: Overloaded1) = toIntf(vals).max    
  def max[A:Manifest:Ordering:HasMinMax](vals: Interface[Vector[A]])(implicit ctx: SourceContext) = vals.max
  def max[A:Manifest:Ordering:HasMinMax](vals: Interface[Matrix[A]])(implicit ctx: SourceContext, o: Overloaded1) = vals.max
  //def max[A:Manifest:Ordering:HasMinMax](vals: A*) = repVecToVecOps(Vector(vals: _*)).max
  def max[A:Manifest:Ordering:HasMinMax](vals: Rep[A]*)(implicit ctx: SourceContext) = repToDenseVecOps(DenseVector(vals: _*)).max

  /**
   * median
   */   
  def median[A:Manifest:Ordering:HasMinMax,V[X] <: Vector[X]](vals: Rep[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], ctx: SourceContext) = toIntf(vals).median
  def median[A:Manifest:Ordering:HasMinMax](vals: Interface[Vector[A]])(implicit ctx: SourceContext) = vals.median
  def median[A:Manifest:Ordering:HasMinMax](vals: Rep[A]*)(implicit ctx: SourceContext) = repToDenseVecOps(DenseVector(vals: _*)).median  
  
  /**
   * mean
   */   
  def mean[A:Manifest:Arith,V[X] <: Vector[X]](vals: Rep[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], conv: Rep[A] => Rep[Double], ctx: SourceContext) = optila_vector_mean(toIntf(vals))
  def mean[A:Manifest:Arith,M[X] <: Matrix[X]](vals: Rep[M[A]])(implicit toIntf: Rep[M[A]] => Interface[Matrix[A]], conv: Rep[A] => Rep[Double], ctx: SourceContext, o: Overloaded1) = optila_matrix_mean(toIntf(vals))
  def mean[A:Manifest:Arith](vals: Interface[Vector[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = optila_vector_mean(vals)
  def mean[A:Manifest:Arith](vals: Interface[Matrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext, o: Overloaded1) = optila_matrix_mean(vals)
  def mean[A:Manifest:Arith](vals: Rep[A]*)(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = optila_vector_mean(denseVecToInterface(DenseVector(vals: _*)))
  
  // explicit to resolve method call ambiguities above
  def optila_vector_mean[A:Manifest:Arith](vals: Interface[Vector[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext) = conv(vals.sum) / vals.length
  def optila_matrix_mean[A:Manifest:Arith](vals: Interface[Matrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext, o: Overloaded2) = conv(vals.sum) / vals.size
  
  /**
   * abs
   */
  def abs[A:Manifest:Arith](elem: Rep[A])(implicit ctx: SourceContext) = repArithToArithOps(elem).abs
  def abs[A:Manifest:Arith](vals: Interface[Vector[A]])(implicit ctx: SourceContext, o: Overloaded1) = vals.abs
  def abs[A:Manifest:Arith](vals: Interface[Matrix[A]])(implicit ctx: SourceContext, o: Overloaded2) = vals.abs
  
  /**
   * log
   */
  def log[A:Manifest](vals: Interface[Vector[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext, o: Overloaded1) = vals.map(e=>optila_scalar_log(conv(e)))
  def log[A:Manifest](vals: Interface[Matrix[A]])(implicit conv: Rep[A] => Rep[Double], ctx: SourceContext, o: Overloaded2) = vals.map(e=>optila_scalar_log(conv(e)))
  
  // library calls should preserve the return type, not box to interface
  def log[A:Manifest,V[X] <: Vector[X]](vals: Rep[V[A]])(implicit conv: Rep[A] => Rep[Double], toOps: Rep[V[A]] => VecOpsCls[A], ctx: SourceContext, o: Overloaded1) = toOps(vals).map(e=>optila_scalar_log(conv(e))).asInstanceOf[Rep[V[Double]]] // not ideal
  def log[A:Manifest,M[X] <: Matrix[X]](vals: Rep[M[A]])(implicit conv: Rep[A] => Rep[Double], toOps: Rep[M[A]] => MatOpsCls[A], ctx: SourceContext, o: Overloaded2) = toOps(vals).map(e=>optila_scalar_log(conv(e))).asInstanceOf[Rep[M[Double]]] // not ideal
  
  /**
   * exp
   */
  def exp[A:Manifest:Arith](vals: Interface[Vector[A]])(implicit ctx: SourceContext, o: Overloaded1) = vals.exp
  def exp[A:Manifest:Arith](vals: Interface[Matrix[A]])(implicit ctx: SourceContext, o: Overloaded2) = vals.exp
  
  def exp[A:Manifest:Arith,V[X] <: Vector[X]](vals: Rep[V[A]])(implicit toOps: Rep[V[A]] => VecOpsCls[A], ctx: SourceContext, o: Overloaded1) = toOps(vals).exp.asInstanceOf[Rep[V[A]]] // not ideal
  def exp[A:Manifest:Arith,M[X] <: Matrix[X]](vals: Rep[M[A]])(implicit toOps: Rep[M[A]] => MatOpsCls[A], ctx: SourceContext, o: Overloaded2) = toOps(vals).exp.asInstanceOf[Rep[M[A]]] // not ideal

  /**
   * square
   */
   def square[A:Manifest:Arith](vals: Interface[Vector[A]])(implicit ctx: SourceContext, o: Overloaded1) = vals.map(e=>e*e)
   def square[A:Manifest:Arith](vals: Interface[Matrix[A]])(implicit ctx: SourceContext, o: Overloaded2) = vals.map(e=>e*e)

   def square[A:Manifest:Arith,V[X] <: Vector[X]](vals: Rep[V[A]])(implicit toOps: Rep[V[A]] => VecOpsCls[A], ctx: SourceContext, o: Overloaded1) = toOps(vals).map(e=>e*e).asInstanceOf[Rep[V[A]]] // not ideal
   def square[A:Manifest:Arith,M[X] <: Matrix[X]](vals: Rep[M[A]])(implicit toOps: Rep[M[A]] => MatOpsCls[A], ctx: SourceContext, o: Overloaded2) = toOps(vals).map(e=>e*e).asInstanceOf[Rep[M[A]]] // not ideal
  
  /**
   * aliases for other scala.math._ operations supported by optila
   */
  def INF = Double.PositiveInfinity  
  def nINF = Double.NegativeInfinity   
  def sqrt(e: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def ceil(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double] 
  def floor(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def exp(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def log(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double] = optila_scalar_log(x)
  def sin(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def cos(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def acos(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def atan(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def atan2(x: Rep[Double], y: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def pow(x: Rep[Double], y: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  def max[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def min[A:Manifest:Numeric](x: Rep[A], y: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def Pi(implicit ctx: SourceContext): Rep[Double]
  def E(implicit ctx: SourceContext): Rep[Double]
  
  // explicit to resolve method call ambiguities above
  def optila_scalar_log(x: Rep[Double])(implicit ctx: SourceContext): Rep[Double]
  
  /**
   * misc. linear algebra
   */
  def det[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]) = optila_determinant(x)
  
  def optila_determinant[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]): Rep[A]  
  
  /**
   * distance
   */
  class DistanceMetric
  object ABS extends DistanceMetric
  object EUC extends DistanceMetric
  object SQUARE extends DistanceMetric

  implicit val scalarDiff: (Rep[Double], Rep[Double]) => Rep[Double] = (a,b) => abs(a-b)
  implicit val vecDiff: (Rep[DenseVector[Double]], Rep[DenseVector[Double]]) => Rep[Double] = (v1,v2) => dist(v1,v2)
  implicit val matDiff: (Rep[DenseMatrix[Double]], Rep[DenseMatrix[Double]]) => Rep[Double] = (m1,m2) => dist(m1,m2)

  // in 2.9, multiple overloaded values cannot all define default arguments
  def dist[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit ctx: SourceContext) = optila_vector_dist_abs(v1,v2)
  def dist[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]], metric: DistanceMetric)(implicit ctx: SourceContext) = metric match {
    case ABS => optila_vector_dist_abs(v1,v2)
    case EUC => optila_vector_dist_euc(v1,v2)
    case SQUARE => optila_vector_dist_square(v1,v2)
    case _ => throw new IllegalArgumentException("Unknown distance metric selected")
  }

  def dist[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])(implicit ctx: SourceContext, o: Overloaded1) = optila_matrix_dist_abs(m1,m2)
  def dist[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]], metric: DistanceMetric)(implicit ctx: SourceContext, o: Overloaded2) = metric match {
    case ABS => optila_matrix_dist_abs(m1,m2)
    case EUC => optila_matrix_dist_euc(m1,m2)
    case SQUARE => optila_matrix_dist_square(m1,m2)
    case _ => throw new IllegalArgumentException("Unknown distance metric selected")
  }

  def optila_vector_dist_abs[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  def optila_vector_dist_euc[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  def optila_vector_dist_square[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  def optila_matrix_dist_abs[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[A]
  def optila_matrix_dist_euc[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[A]
  def optila_matrix_dist_square[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])(implicit ctx: SourceContext): Rep[A]


  /**
   * Sampling
   */

  abstract class SampleMethod
  object RANDOM extends SampleMethod

  // sampling of input to reduce data size
  def sample[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean] = unit(true), method: SampleMethod = RANDOM)(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA] = {
    method match {
      case RANDOM => optila_randsample_matrix[A,I,MA](m, numSamples, sampleRows)
      case _ => throw new UnsupportedOperationException("unknown sampling type selected")
    }
  }

  def sample[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Rep[Int])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA] = {
    optila_randsample_vector[A,VA](v, numSamples)
  }
  
  def sample[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Rep[Int], method: SampleMethod)(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA] = {
    method match {
      case RANDOM => optila_randsample_vector[A,VA](v, numSamples)
      case _ => throw new UnsupportedOperationException("unknown sampling type selected")
    }
  }

  def optila_randsample_matrix[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA]
  def optila_randsample_vector[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Rep[Int])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  
 
  /**
   * interpolation
   */
  //def interpolate[A](m: Matrix[A]) : Matrix[A]
  //def interpolate[A](v: Vector[A]) : Vector[A]


  /**
   *   Profiling
   */
  // lightweight profiling, matlab style
  def tic(deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_start(unit("app"),deps)
  def tic(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_start(component, deps)
  def toc(deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_stop(unit("app"),deps)
  def toc(component: Rep[String], deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_stop(component, deps)
  def time(deps: Rep[Any]*)(implicit ctx: SourceContext) = profile_time(deps)

  def profile_start(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def profile_stop(component: Rep[String], deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Unit]
  def profile_time(deps: Seq[Rep[Any]])(implicit ctx: SourceContext): Rep[Double]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: OptiLAExp with LanguageImplOps =>
    
  /**
   * Random
   */
  case class InternalRandDouble() extends Def[Double]
  case class InternalRandFloat() extends Def[Float]
  case class InternalRandInt() extends Def[Int]
  case class InternalRandLong() extends Def[Long]
  case class InternalRandBoolean() extends Def[Boolean]

  case class RandDouble() extends Def[Double]
  case class RandFloat() extends Def[Float]
  case class RandInt() extends Def[Int]
  case class RandIntMax(max: Exp[Int]) extends Def[Int]
  case class RandLong() extends Def[Long]
  case class RandBoolean() extends Def[Boolean]
  case class RandGaussian() extends Def[Double]
  case class RandElem[A:Manifest](v: Interface[Vector[A]]) extends DeliteOpSingleWithManifest[A,A](reifyEffects(optila_randelem_impl(v))) 

  case class RandReseed() extends Def[Unit]
  
  case class IdentityHashCode(x: Exp[Any]) extends Def[Int]

  def optila_internal_rand_double() = reflectEffect(InternalRandDouble())
  def optila_internal_rand_float() = reflectEffect(InternalRandFloat())
  def optila_internal_rand_int() = reflectEffect(InternalRandInt())
  def optila_internal_rand_long() = reflectEffect(InternalRandLong())
  def optila_internal_rand_boolean() = reflectEffect(InternalRandBoolean())

  def optila_rand_double()(implicit ctx: SourceContext) = reflectEffect(RandDouble())
  def optila_rand_float()(implicit ctx: SourceContext) = reflectEffect(RandFloat())
  def optila_rand_int()(implicit ctx: SourceContext) = reflectEffect(RandInt())
  def optila_rand_int_max(max: Exp[Int])(implicit ctx: SourceContext) = reflectEffect(RandIntMax(max))
  def optila_rand_long()(implicit ctx: SourceContext) = reflectEffect(RandLong())
  def optila_rand_boolean()(implicit ctx: SourceContext) = reflectEffect(RandBoolean())
  def optila_rand_gaussian()(implicit ctx: SourceContext) = reflectEffect(RandGaussian())
  def optila_rand_elem[A:Manifest](elems: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectEffect(RandElem(elems))

  def optila_reseed()(implicit ctx: SourceContext) = reflectEffect(RandReseed())
  
  def identityHashCode(x:Exp[Any])(implicit ctx: SourceContext) = reflectPure(IdentityHashCode(x))

  /**
   * aliases for scala.math._ operations supported by optila
   */
  def sqrt(e: Exp[Double])(implicit ctx: SourceContext) = Math.sqrt(e)   
  def ceil(x: Exp[Double])(implicit ctx: SourceContext) = Math.ceil(x)
  def floor(x: Exp[Double])(implicit ctx: SourceContext) = Math.floor(x)
  def exp(x: Exp[Double])(implicit ctx: SourceContext) = Math.exp(x)
  def sin(x: Exp[Double])(implicit ctx: SourceContext) = Math.sin(x)
  def cos(x: Exp[Double])(implicit ctx: SourceContext) = Math.cos(x)
  def acos(x: Exp[Double])(implicit ctx: SourceContext) = Math.acos(x)
  def atan(x: Exp[Double])(implicit ctx: SourceContext) = Math.atan(x)
  def atan2(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = Math.atan2(x,y)
  def pow(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = Math.pow(x,y)
  def max[A:Manifest:Numeric](x: Exp[A], y: Exp[A])(implicit ctx: SourceContext) = Math.max(x,y)
  def min[A:Manifest:Numeric](x: Exp[A], y: Exp[A])(implicit ctx: SourceContext) = Math.min(x,y)
  def Pi(implicit ctx: SourceContext) = Math.Pi
  def E(implicit ctx: SourceContext) = Math.E
  
  def optila_scalar_log(x: Rep[Double])(implicit ctx: SourceContext) = Math.log(x)
  
  /**
   * misc. linear algebra
   */
  
  case class MatrixDeterminant22[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]) extends DeliteOpSingleTask[A](reifyEffects(optila_matrix_determinant22_impl(x)))
  case class MatrixDeterminant33[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]) extends DeliteOpSingleTask[A](reifyEffects(optila_matrix_determinant33_impl(x)))
  case class MatrixDeterminant44[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]) extends DeliteOpSingleTask[A](reifyEffects(optila_matrix_determinant44_impl(x)))
  case class MatrixDeterminant[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]) extends DeliteOpSingleTask[A](reifyEffects(optila_matrix_determinant_impl(x))) {
    val m = manifest[A]
    val a = implicitly[Arith[A]]
    val n = implicitly[Numeric[A]]
  }
  
  def optila_determinant[A:Manifest:Arith:Numeric](x: Interface[Matrix[A]]) = x.ops.elem.asInstanceOf[Rep[Matrix[A]]] match {
    case Def(s@Reflect(DenseMatrixObjectNew(Const(a),Const(b)), u, es)) if context.contains(s) && (a == 2) && (b == 2) => reflectPure(MatrixDeterminant22(x))
    case Def(s@Reflect(DenseMatrixObjectNew(Const(a),Const(b)), u, es)) if context.contains(s) && (a == 3) && (b == 3) => reflectPure(MatrixDeterminant33(x))
    case Def(s@Reflect(DenseMatrixObjectNew(Const(a),Const(b)), u, es)) if context.contains(s) && (a == 4) && (b == 4) => reflectPure(MatrixDeterminant44(x))
    case _ => reflectPure(MatrixDeterminant(x))
  }
     
  /**
   *  dist
   */
  trait VectorDistance

  case class VectorDistanceAbs[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optila_vectordistance_abs_impl(v1,v2))) with VectorDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]] //TODO factor into DeliteOp subclass
    }

  case class VectorDistanceEuc[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optila_vectordistance_euc_impl(v1,v2))) with VectorDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  case class VectorDistanceSquare[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optila_vectordistance_square_impl(v1,v2))) with VectorDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }


  trait MatrixDistance

  case class MatrixDistanceAbs[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optila_matrixdistance_abs_impl(m1,m2))) with MatrixDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  case class MatrixDistanceEuc[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optila_matrixdistance_euc_impl(m1,m2))) with MatrixDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  case class MatrixDistanceSquare[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optila_matrixdistance_square_impl(m1,m2))) with MatrixDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  def optila_vector_dist_abs[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorDistanceAbs(v1,v2))
  def optila_vector_dist_euc[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorDistanceEuc(v1,v2))
  def optila_vector_dist_square[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorDistanceSquare(v1,v2))
  def optila_matrix_dist_abs[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectPure(MatrixDistanceAbs(m1,m2))
  def optila_matrix_dist_euc[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectPure(MatrixDistanceEuc(m1,m2))
  def optila_matrix_dist_square[A:Manifest:Arith](m1: Interface[Matrix[A]], m2: Interface[Matrix[A]])(implicit ctx: SourceContext) = reflectPure(MatrixDistanceSquare(m1,m2))


  /**
   * Sampling
   */
  
  case class RandSampleMatrix[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], numSamples: Exp[Int], sampleRows: Exp[Boolean])(implicit b: MatrixBuilder[A,I,MA])
    extends DeliteOpSingleTask[MA](reifyEffects(optila_randsample_matrix_impl[A,I,MA](m, numSamples, sampleRows)))

  case class RandSampleVector[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Exp[Int])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffects(optila_randsample_vector_impl[A,VA](v, numSamples)))
  
  def optila_randsample_matrix[A:Manifest,I:Manifest,MA:Manifest](m: Interface[Matrix[A]], numSamples: Exp[Int], sampleRows: Exp[Boolean])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext) = {
    reflectPure(RandSampleMatrix[A,I,MA](m, numSamples, sampleRows))
  }

  def optila_randsample_vector[A:Manifest,VA:Manifest](v: Interface[Vector[A]], numSamples: Exp[Int])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = {
    reflectPure(RandSampleVector[A,VA](v, numSamples))
  }


  /**
   *   Profiling
   */
  case class ProfileStart(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class ProfileStop(component: Exp[String], deps: List[Exp[Any]]) extends Def[Unit]
  case class ProfileTime(deps: List[Exp[Any]]) extends Def[Double]

  def profile_start(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ProfileStart(component, deps.toList))
  def profile_stop(component: Exp[String], deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ProfileStop(component, deps.toList))
  def profile_time(deps: Seq[Exp[Any]])(implicit ctx: SourceContext) = reflectEffect(ProfileTime(deps.toList))
  
  /**
   * Mirroring
   */
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@VectorDistanceAbs(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDistanceAbs(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDistanceEuc(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDistanceEuc(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDistanceSquare(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDistanceSquare(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixDistanceAbs(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDistanceAbs(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixDistanceEuc(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDistanceEuc(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixDistanceSquare(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDistanceSquare(f(x),f(y))(e.m,e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@MatrixDeterminant(x) => reflectPure(new { override val original = Some(f,e) } with MatrixDeterminant(f(x))(e.m,e.a,e.n))(mtype(manifest[A]),implicitly[SourceContext])
    case e@RandSampleVector(x,n) => reflectPure(new { override val original = Some(f,e) } with RandSampleVector(f(x),f(n))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case Reflect(e@VectorDistanceAbs(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDistanceAbs(f(x),f(y))(e.m,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(RandReseed(), u, es) => reflectMirrored(Reflect(RandReseed(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(RandInt(), u, es) => reflectMirrored(Reflect(RandInt(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(RandDouble(), u, es) => reflectMirrored(Reflect(RandDouble(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(RandGaussian(), u, es) => reflectMirrored(Reflect(RandGaussian(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(RandFloat(), u, es) => reflectMirrored(Reflect(RandFloat(), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(RandIntMax(max), u, es) => reflectMirrored(Reflect(RandIntMax(f(max)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@RandElem(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with RandElem(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@MatrixDeterminant(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixDeterminant(f(x))(e.m,e.a,e.n), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ProfileStart(c,deps), u, es) => reflectMirrored(Reflect(ProfileStart(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ProfileStop(c,deps), u, es) => reflectMirrored(Reflect(ProfileStop(f(c),f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ProfileTime(deps), u, es) => reflectMirrored(Reflect(ProfileTime(f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
  import IR._

}

trait ScalaGenLanguageOps extends ScalaGenEffect with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    // TODO: need fully qualified package name to Global? should remove the dependency on the package name.
    rhs match {
      case InternalRandDouble() => emitValDef(sym, "generated.scala.Global.intRandRef.nextDouble()")
      case InternalRandFloat() => emitValDef(sym, "generated.scala.Global.intRandRef.nextFloat()")
      case InternalRandInt() => emitValDef(sym, "generated.scala.Global.intRandRef.nextInt()")
      case InternalRandLong() => emitValDef(sym, "generated.scala.Global.intRandRef.nextLong()")
      case InternalRandBoolean() => emitValDef(sym, "generated.scala.Global.intRandRef.nextBoolean()")
      case RandDouble() => emitValDef(sym, "generated.scala.Global.randRef.nextDouble()")
      case RandFloat() => emitValDef(sym, "generated.scala.Global.randRef.nextFloat()")
      case RandInt() => emitValDef(sym, "generated.scala.Global.randRef.nextInt()")
      case RandIntMax(max) => emitValDef(sym, "generated.scala.Global.randRef.nextInt(" + quote(max) + ")")
      case RandLong() => emitValDef(sym, "generated.scala.Global.randRef.nextLong()")
      case RandBoolean() => emitValDef(sym, "generated.scala.Global.randRef.nextBoolean()")
      case RandGaussian() => emitValDef(sym, "generated.scala.Global.randRef.nextGaussian()")
      case RandReseed() => emitValDef(sym, "{ generated.scala.Global.randRef.setSeed(generated.scala.Global.INITIAL_SEED);" +
                                           "   generated.scala.Global.intRandRef.setSeed(generated.scala.Global.INITIAL_SEED); }")
      case IdentityHashCode(x) => emitValDef(sym, "System.identityHashCode(" + quote(x) + ")")
      case ProfileStart(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(" + quote(c) + ", false)")
      case ProfileStop(c,deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(" + quote(c) + ", false)")
      case ProfileTime(deps) => emitValDef(sym, "System.currentTimeMillis()/1000.0")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

/*
trait CudaGenLanguageOps extends CudaGenBase with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
     }
  }
}
*/
