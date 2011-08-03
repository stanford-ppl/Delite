package ppl.dsl.optiml

import datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp
import java.io.PrintWriter
import reflect.Manifest
import scala.virtualization.lms.internal.GenericFatCodegen
import scala.virtualization.lms.common._

/* Machinery provided by OptiML itself (language features and control structures).
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * created: Nov 29, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

trait LanguageOps extends Base { this: OptiML =>

  /**
   * random
   */
  // this version is for optiml's use exclusively, so it does not interfere with application behavior
  private def _random[A](implicit mA: Manifest[A]): Rep[A] =
    mA match {
      case Manifest.Double => optiml_internal_rand_double.asInstanceOfL[A]
      case Manifest.Float => optiml_internal_rand_float.asInstanceOfL[A]
      case Manifest.Int => optiml_internal_rand_int.asInstanceOfL[A]
      case Manifest.Long => optiml_internal_rand_long.asInstanceOfL[A]
      case Manifest.Boolean => optiml_internal_rand_boolean.asInstanceOfL[A]
      case _ => throw new UnsupportedOperationException()
  }

  // public version for application use
  def random[A](implicit mA: Manifest[A]): Rep[A] =
    mA match {
      case Manifest.Double => optiml_rand_double.asInstanceOfL[A]
      case Manifest.Float => optiml_rand_float.asInstanceOfL[A]
      case Manifest.Int => optiml_rand_int.asInstanceOfL[A]
      case Manifest.Long => optiml_rand_long.asInstanceOfL[A]
      case Manifest.Boolean => optiml_rand_boolean.asInstanceOfL[A]
      case _ => throw new UnsupportedOperationException()
  }

  def random(max: Rep[Int]): Rep[Int] = optiml_rand_int_max(max)

  def randomGaussian = optiml_rand_gaussian

  def reseed {
    // reseeds for all threads
    optiml_reseed()
  }
  
  def identityHashCode(x:Rep[Any]): Rep[Int]

  def optiml_internal_rand_double(): Rep[Double]
  def optiml_internal_rand_float(): Rep[Float]
  def optiml_internal_rand_int(): Rep[Int]
  def optiml_internal_rand_long(): Rep[Long]
  def optiml_internal_rand_boolean(): Rep[Boolean]

  def optiml_rand_double(): Rep[Double]
  def optiml_rand_float(): Rep[Float]
  def optiml_rand_int(): Rep[Int]
  def optiml_rand_int_max(max: Rep[Int]): Rep[Int]
  def optiml_rand_long(): Rep[Long]
  def optiml_rand_boolean(): Rep[Boolean]
  def optiml_rand_gaussian(): Rep[Double]

  def optiml_reseed(): Rep[Unit]


  /**
   * aggregate
   */
  // 1D aggregate
  def aggregate[A:Manifest](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[Vector[A]]): Rep[Vector[A]] = {
    optiml_aggregate(start, end, block)
  }

  // 2D aggregate
  def aggregate[A:Manifest](rowStart: Rep[Int], rowEnd: Rep[Int], colStart: Rep[Int], colEnd: Rep[Int])
                           (block: (Rep[Int], Rep[Int]) => Rep[Vector[A]]): Rep[Vector[A]] = {
    optiml_aggregate2d(rowStart, rowEnd, colStart, colEnd, block)
  }

  def optiml_aggregate[A:Manifest](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[Vector[A]]): Rep[Vector[A]]
  def optiml_aggregate2d[A:Manifest](rowStart: Rep[Int], rowEnd: Rep[Int], colStart: Rep[Int], colEnd: Rep[Int],
                                     block: (Rep[Int], Rep[Int]) => Rep[Vector[A]]): Rep[Vector[A]]


  // TODO: for some reason, the implicit ops conversions aren't kicking in for sum/min/max
  /**
   * sum
   */
  def sum[A:Manifest:Arith:Cloneable](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)
  def sum[A:Manifest:Arith:Cloneable](vals: Rep[Vector[A]]) = repVecToVecOps(vals).sum
  def sum[A](vals: Rep[Matrix[A]])(implicit mA: Manifest[A], a: Arith[A], c: Cloneable[A], o: Overloaded1) = repMatToMatOps(vals).sum
  def sumIf[A:Manifest:Arith:Cloneable](start: Rep[Int], end: Rep[Int])(cond: Rep[Int] => Rep[Boolean])(block: Rep[Int] => Rep[A]) = optiml_sumif(start, end, cond, block)
  
  def optiml_sum[A:Manifest:Arith:Cloneable](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]): Rep[A]
  def optiml_sumif[A:Manifest:Arith:Cloneable](start: Rep[Int], end: Rep[Int], cond: Rep[Int] => Rep[Boolean], block: Rep[Int] => Rep[A]): Rep[A]

  /**
   * min
   */
  def min[A:Manifest:Ordering:HasMinMax](vals: Rep[Vector[A]]) = repVecToVecOps(vals).min
  def min[A](vals: Rep[Matrix[A]])(implicit mA: Manifest[A], ord: Ordering[A], mx: HasMinMax[A], o: Overloaded1) = repMatToMatOps(vals).min
  //def min[A:Manifest:Ordering:HasMinMax](vals: A*) = repVecToVecOps(Vector(vals: _*)).min
  def min[A:Manifest:Ordering:HasMinMax](vals: Rep[A]*) = repVecToVecOps(Vector(vals: _*)).min

  /**
   * max
   */
  def max[A:Manifest:Ordering:HasMinMax](vals: Rep[Vector[A]]) = repVecToVecOps(vals).max
  def max[A](vals: Rep[Matrix[A]])(implicit mA: Manifest[A], ord: Ordering[A], mx: HasMinMax[A], o: Overloaded1) = repMatToMatOps(vals).max
  //def max[A:Manifest:Ordering:HasMinMax](vals: A*) = repVecToVecOps(Vector(vals: _*)).max
  def max[A:Manifest:Ordering:HasMinMax](vals: Rep[A]*) = repVecToVecOps(Vector(vals: _*)).max


  /**
   * mean
   * TODO: implement this in vector/matrix
   */


  /**
   * abs
   */
  // TODO: sbt fails without the explicit invocation of arithToArithOps, but IDEA compiles. wtf?
  def abs[A:Manifest:Arith](elem: Rep[A]) = repArithToArithOps(elem).abs
  //def abs[A](vals: Rep[Vector[A]])(implicit mA: Manifest[A], a: Arith[A], o: Overloaded1) = vals.abs
  //def abs[A](vals: Rep[Matrix[A]])(implicit mA: Manifest[A], a: Arith[A], o: Overloaded2) = vals.abs


  /**
   *  IndexVector construction
   */
  implicit def intToIndexOp(i : Int) = new IndexOp(i)
  implicit def repIntToIndexOp(i : Rep[Int]) = new IndexOp(i)

  class IndexOp(val _end : Rep[Int]) {
    def ::(_start : Rep[Int]) = indexvector_range(_start, _end)
  }

  /**
   * IndexVector2 construction (Matrix construction)
   * ex.  (0::n, *) { { rowIdx => ... }
   * ex2. (*, 0::n) { { colIdx => ... }
   * ex3. (0::n,0::m) { (i,j) => .. }
   */
  class IndexWildcard
  val * = new IndexWildcard

  implicit def tuple2ToIndexVector1(tup: (Rep[IndexVector], IndexWildcard))(implicit overloaded1 : Overloaded1) = indexvector2_new(tup._1, indexvector2_wildcard())
// currently not allowed
//  implicit def tuple2ToIndexVector2(tup: (IndexWildcard, Rep[IndexVector]))(implicit overloaded2 : Overloaded2) = indexvector2_new(indexvector2_wildcard(), tup._2)
  implicit def tuple2ToIndexVector3(tup: (Rep[IndexVector], Rep[IndexVector]))(implicit overloaded3 : Overloaded3) = indexvector2_new(tup._1, tup._2)


  /**
   * untilconverged
   */
  def untilconverged[A](x: Rep[A],
                        thresh: Rep[Double],
                        max_iter: Rep[Int] = unit(1000),
                        clone_prev_val: Rep[Boolean] = unit(false))
                        (block: Rep[A] => Rep[A])
                        (implicit diff: (Rep[A],Rep[A]) => Rep[Double], mA: Manifest[A], c: Cloneable[A]): Rep[A]
    = optiml_untilconverged(x, thresh, max_iter, clone_prev_val, block, diff)


  def optiml_untilconverged[A:Manifest:Cloneable](x: Rep[A], thresh: Rep[Double], max_iter: Rep[Int], clone_prev_val: Rep[Boolean],
                                                  block: Rep[A] => Rep[A], diff: (Rep[A],Rep[A]) => Rep[Double]): Rep[A]

  def untilconverged[V <: Vertex, E <: Edge](g: Rep[Graph[V, E]])
                        (block: Rep[V] => Rep[Unit])
                        (implicit mV: Manifest[V], mE: Manifest[E]): Rep[Unit]
    = optiml_untilconverged(g, block)

  def optiml_untilconverged[V <: Vertex :Manifest, E <: Edge :Manifest](g: Rep[Graph[V, E]], block: Rep[V] => Rep[Unit]) : Rep[Unit]


  /**
   * gradient descent
   */
  def gradient(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double] = unit(.001), thresh: Rep[Double] = unit(.0001),
               maxIter: Rep[Int] = unit(10000))(hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]]
    = optiml_gradient(x, alpha, thresh, maxIter, hyp)

  // stochastic: block() updates every jth parameter for every ith training sample
  //    while not converged{
  //      for i from 0 until m
  //        for j from 0 until n
  //            updatej(i,j)
  // hypothesis function maps a training example to a prediction

  // stochastic can only be parallelized across features, which is generally << samples
  def stochastic(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double] = unit(.001), thresh: Rep[Double] = unit(.0001),
                 maxIter: Rep[Int] = unit(10000))(hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]]
    = optiml_stochastic(x, alpha, thresh, maxIter, hyp)

  // batch: block() updates each jth parameter from the sum of all ith training samples
  //    while not converged{
  //      for j from 0 until n
  //        j_update = sum((y(i) - h(x(i))*x(j,i)
  //        updatej(j_update)

  // in batch, the sum(...) loops over the entire training set independently, which is where the parallelism comes from
  // batch can be parallized across samples
  def batch(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double] = unit(.001), thresh: Rep[Double] = unit(.0001),
               maxIter: Rep[Int] = unit(10000))(hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]]
    = optiml_batch(x, alpha, thresh, maxIter, hyp)


  def optiml_gradient(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double], thresh: Rep[Double],
                      maxIter: Rep[Int], hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]]

  def optiml_stochastic(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double], thresh: Rep[Double],
                        maxIter: Rep[Int], hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]]

  def optiml_batch(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double], thresh: Rep[Double],
                   maxIter: Rep[Int], hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]]

  // coordinate ascent: analogous to stochastic gradient descent, but updates m parameters (alphas(0)...alphas(m-1))
  // at each update, all but alpha(i) must be held constant, so there are dependencies between every iteration

  //Loop until convergence {
  // For i = 1, . . . ,m, {
  //   alphas(i) := arg max alpha^(i) W(alphas(0), . . . , alphas(i-1), alphas^(i), alphas(i+1), . . . , alphas(m-1))
  // }
  //}

  /**
   *  i/o
   */
  def readMatrix(filename: Rep[String]) = MLInputReader.read(filename)
  def readVector(filename: Rep[String]) = MLInputReader.readVector(filename)
  def readImage(filename: Rep[String]) = MLInputReader.readGrayscaleImage(filename)

  def writeMatrix[A](x: Rep[Matrix[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double])
    = MLOutputWriter.write(x, filename)
  def writeVector[A](x: Rep[Vector[A]], filename: Rep[String])(implicit mA: Manifest[A], conv: Rep[A] => Rep[Double])
    = MLOutputWriter.writeVector(x, filename)

  /**
   * distance
   */
  class DistanceMetric
  object ABS extends DistanceMetric
  object EUC extends DistanceMetric
  object SQUARE extends DistanceMetric

  implicit val vecDiff: (Rep[Vector[Double]], Rep[Vector[Double]]) => Rep[Double] = (v1,v2) => dist(v1,v2)
  implicit val matDiff: (Rep[Matrix[Double]], Rep[Matrix[Double]]) => Rep[Double] = (m1,m2) => dist(m1,m2)

  // in 2.9, multiple overloaded values cannot all define default arguments
  def dist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = optiml_vector_dist_abs(v1,v2)
  def dist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: DistanceMetric) = metric match {
    case ABS => optiml_vector_dist_abs(v1,v2)
    case EUC => optiml_vector_dist_euc(v1,v2)
    case SQUARE => optiml_vector_dist_square(v1,v2)
    case _ => throw new IllegalArgumentException("Unknown distance metric selected")
  }

  def dist[A](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]])(implicit mA: Manifest[A], a: Arith[A], o: Overloaded1) = optiml_matrix_dist_abs(m1,m2)
  def dist[A](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: DistanceMetric)
             (implicit mA: Manifest[A], a: Arith[A], o: Overloaded1) = metric match {

    case ABS => optiml_matrix_dist_abs(m1,m2)
    case EUC => optiml_matrix_dist_euc(m1,m2)
    case SQUARE => optiml_matrix_dist_square(m1,m2)
    case _ => throw new IllegalArgumentException("Unknown distance metric selected")
  }

  def optiml_vector_dist_abs[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]): Rep[A]
  def optiml_vector_dist_euc[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]): Rep[A]
  def optiml_vector_dist_square[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]): Rep[A]
  def optiml_matrix_dist_abs[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]
  def optiml_matrix_dist_euc[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]
  def optiml_matrix_dist_square[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]): Rep[A]


  /**
   * Sampling
   */

  abstract class SampleMethod
  object RANDOM extends SampleMethod

  // sampling of input to reduce data size
  def sample[A:Manifest](m: Rep[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean] = unit(true), method: SampleMethod = RANDOM): Rep[Matrix[A]] = {
    method match {
      case RANDOM => optiml_randsample_matrix(m, numSamples, sampleRows)
      case _ => throw new UnsupportedOperationException("unknown sampling type selected")
    }
  }

  def sample[A:Manifest](v: Rep[Vector[A]], numSamples: Rep[Int]): Rep[Vector[A]] = {
    optiml_randsample_vector(v, numSamples)
  }

  def sample[A:Manifest](v: Rep[Vector[A]], numSamples: Rep[Int], method: SampleMethod): Rep[Vector[A]] = {
    method match {
      case RANDOM => optiml_randsample_vector(v, numSamples)
      case _ => throw new UnsupportedOperationException("unknown sampling type selected")
    }
  }

  def optiml_randsample_matrix[A:Manifest](m: Rep[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean]): Rep[Matrix[A]]
  def optiml_randsample_vector[A:Manifest](v: Rep[Vector[A]], numSamples: Rep[Int]): Rep[Vector[A]]
  
 
  /**
   * interpolation
   */
  //def interpolate[A](m: Matrix[A]) : Matrix[A]
  //def interpolate[A](v: Vector[A]) : Vector[A]


  /**
   * Nearest neighbor
   */
  // returns the index of the nearest neighbor of row inside data
  def nearestNeighborIndex[A:Manifest:Arith:Ordering:HasMinMax](row: Rep[Int], data: Rep[Matrix[A]], allowSame: Rep[Boolean] = unit(true)): Rep[Int]
    = optiml_nearest_neighbor_index(row, data, allowSame)

  def optiml_nearest_neighbor_index[A:Manifest:Arith:Ordering:HasMinMax](row: Rep[Int], data: Rep[Matrix[A]], allowSame: Rep[Boolean]): Rep[Int]

  /**
   *   Profiling
   */
  // lightweight profiling, matlab style
  def tic(deps: Rep[Any]*) = profile_start(deps)
  def toc(deps: Rep[Any]*) = profile_stop(deps)

  def profile_start(deps: Seq[Rep[Any]]): Rep[Unit]
  def profile_stop(deps: Seq[Rep[Any]]): Rep[Unit]
}

trait LanguageOpsExp extends LanguageOps with BaseFatExp with EffectExp {
  this: OptiMLExp with LanguageImplOps =>

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

  case class RandReseed() extends Def[Unit]
  
  case class IdentityHashCode(x: Exp[Any]) extends Def[Int]


  /**
   * Random
   */
  def optiml_internal_rand_double() = reflectEffect(InternalRandDouble())
  def optiml_internal_rand_float() = reflectEffect(InternalRandFloat())
  def optiml_internal_rand_int() = reflectEffect(InternalRandInt())
  def optiml_internal_rand_long() = reflectEffect(InternalRandLong())
  def optiml_internal_rand_boolean() = reflectEffect(InternalRandBoolean())

  def optiml_rand_double() = reflectEffect(RandDouble())
  def optiml_rand_float() = reflectEffect(RandFloat())
  def optiml_rand_int() = reflectEffect(RandInt())
  def optiml_rand_int_max(max: Exp[Int]) = reflectEffect(RandIntMax(max))
  def optiml_rand_long() = reflectEffect(RandLong())
  def optiml_rand_boolean() = reflectEffect(RandBoolean())
  def optiml_rand_gaussian() = reflectEffect(RandGaussian())

  def optiml_reseed() = reflectEffect(RandReseed())
  
  def identityHashCode(x:Exp[Any]) = reflectPure(IdentityHashCode(x))


  /**
   * Aggregate
   */
  def optiml_aggregate[A:Manifest](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[Vector[A]]) = {
    // unroll
    (start::end) flatMap { block }

    // Other impl.: use a for loop to iterate over the data structure and use synchronization to aggregate values
  }

  def optiml_aggregate2d[A:Manifest](rowStart: Exp[Int], rowEnd: Exp[Int], colStart: Exp[Int], colEnd: Exp[Int],
                                     block: (Exp[Int], Exp[Int]) => Exp[Vector[A]]) = {
    // unroll
    (rowStart::rowEnd) flatMap { y =>
      (colStart::colEnd) flatMap { x =>
        block(x, y)
      }
    }
  }


  /**
   * Sum
   */

  case class Sum[A:Manifest:Arith:Cloneable](start: Exp[Int], end: Exp[Int], map: Exp[Int] => Exp[A], init: Exp[A])
    extends DeliteOpMapReduce[Int,A] {

    override val mutable = true // can we do this automatically?
    
    val in = copyTransformedOrElse(_.in)(start::end)
    val size = copyTransformedOrElse(_.size)(end - start)
    val zero = copyTransformedOrElse(_.zero)(reifyEffects(a.zero(init).mutable)) // FIXME: zero can be a fresh matrix, mutable calls cloneL
    def reduce = (a,b) => a += b
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]    
    def c = implicitly[Cloneable[A]]
  }

  case class SumIf[A:Manifest:Arith:Cloneable](start: Exp[Int], end: Exp[Int], cond: Exp[Int] => Exp[Boolean], func: Exp[Int] => Exp[A], init: Exp[A])
    extends DeliteOpFilterReduce[Int,A] {

    override val mutable = true // can we do this automatically?
    
    val in = copyTransformedOrElse(_.in)((start::end))
    val size = copyTransformedOrElse(_.size)(end - start)
    val zero = copyTransformedOrElse(_.zero)(reifyEffects(a.zero(init).mutable))
    def reduce = (a,b) => a += b  
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
    def c = implicitly[Cloneable[A]]
  }

  // FIXME: peeling off the first iteration manually can prevent fusion because the LoopOp is 1 smaller than its cousins
  // TODO: what is the deired behavior if the range is empty?
  def optiml_sum[A:Manifest:Arith:Cloneable](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A]) = {
    val firstBlock = block(start)
    val out = reflectPure(Sum(start+1, end, block, firstBlock))
    out + firstBlock
  }

  def optiml_sumif[A:Manifest:Arith:Cloneable](start: Exp[Int], end: Exp[Int], cond: Exp[Int] => Exp[Boolean], block: Exp[Int] => Exp[A]) = {
    val firstCond = cond(start)
    val firstBlock = block(start)
    val out = reflectPure(SumIf(start+1, end, cond, block, firstBlock))
    flatIf (firstCond) {
      out + firstBlock
    } {
      out
    }
  }

/*  
  def optiml_sum[A:Manifest:Arith:Cloneable](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A]) = {

    //Sum(start, end, block)
    // HACK -- better scheduling performance in our apps, forces some expensive dependencies to be hoisted
    //reflectEffect(Sum(start, end, block))
    val firstBlock = block(start)
    val out = reflectPure(Sum(start+1, end, block, firstBlock))
    // add the first value back in (exploit commutativity of +)
//    out += firstBlock      
//    out.unsafeImmutable
    out + firstBlock
  }
  
  def optiml_sumif[A:Manifest:Arith:Cloneable](start: Exp[Int], end: Exp[Int], cond: Exp[Int] => Exp[Boolean], block: Exp[Int] => Exp[A]) = {
    val firstCond = cond(start)
    val firstBlock = block(start)
    val out = reflectPure(SumIf(start+1, end, cond, block, firstBlock))
    // add the first value back in (exploit commutativity of +)
    if (firstCond) {
//      out += firstBlock
//      ()
      out + firstBlock
    }
//    out.unsafeImmutable
    else {
      out
    }
  }
*/  
  
  

  /**
   * untilconverged
   */
  
//  case class UntilConverged[A:Manifest:Cloneable](x: Exp[A], thresh: Exp[Double], max_iter: Exp[Int], clone_prev_val: Exp[Boolean],
//                                                  func: Exp[A] => Exp[A], diff: (Exp[A],Exp[A]) => Exp[Double])
//    extends DeliteOpSingleTask[A](reifyEffects(optiml_untilconverged_impl(x,thresh,max_iter,clone_prev_val,func,diff)))
//

//  def optiml_untilconverged[A:Manifest:Cloneable](x: Exp[A], thresh: Exp[Double], max_iter: Exp[Int], clone_prev_val: Exp[Boolean],
//                                                  block: Exp[A] => Exp[A], diff: (Exp[A],Exp[A]) => Exp[Double]) =
//    reflectEffect(UntilConverged(x, thresh, max_iter, clone_prev_val, block, diff))
//

  // for now, just unroll the implementation
  // we need a concept of a composite op to do this without unrolling, so that we can have a different result type than the while
  def optiml_untilconverged[V <: Vertex : Manifest, E <: Edge : Manifest](g: Rep[Graph[V, E]], block: Rep[V] => Rep[Unit]) = {
    val vertices = g.vertices
    val tasks : Rep[Vertices[V]] = vertices.mutable
    val seen = Set[V]()
    
    while(tasks.length > 0) {
      tasks.mforeach(block)
      tasks.clear()
      //var totalTasks = unit(0)
      
      for(i <- 0 until vertices.length) {
        val vtasks = vertices(i).tasks
        //totalTasks += vtasks.length
        for(j <- 0 until vtasks.length) {
          val task = vtasks(j).asInstanceOfL[V]
          if(!seen.contains(task)) {
            tasks += task   //TODO TR: non-mutable write (use mclone)
            seen.add(task)   //TODO TR: non-mutable write
          }
        }

        vertices(i).clearTasks()
      }

      //println("tasks: " + tasks.length)
      seen.clear()
    }
  }

  def optiml_untilconverged[A:Manifest:Cloneable](x: Exp[A], thresh: Exp[Double], max_iter: Exp[Int], clone_prev_val: Exp[Boolean],
                                                  block: Exp[A] => Exp[A], diff: (Exp[A],Exp[A]) => Exp[Double]) = {

    var delta = var_new(unit(scala.Double.MaxValue))
    var cur = var_new(x)
    var iter = var_new(unit(0))

    while ((Math.abs(delta) > thresh) && (iter < max_iter)){
      val prev = if (clone_prev_val)
        cur.cloneL()
      else
        cur

//      try{
        val next = block(cur)
//      }
//      catch{
//        case e: Exception => throw new ConvergenceException("Converging block threw exception: " + e)
//      }
      iter += 1
      //prev.asInstanceOfLOfL[Matrix[Any]].pprint
      //next.asInstanceOfL[Matrix[Any]].pprint
      delta = diff(next,prev)
      cur = next
      //println("(" + delta + ")")
    }

    if (iter == max_iter){
      //throw new ConvergenceException("Maximum iterations exceeded")
      println(unit("Maximum iterations exceeded"))
      returnL()
    }

    cur
  }

  /**
   * gradient descent
   */
  private val MIN_BATCH_PROCS = 4
  def optiml_gradient(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double], thresh: Rep[Double],
                      maxIter: Rep[Int], hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]] = {

    val y = x.labels
    val numProcs = 8 //Delite.threadNum // dynamically set
    if (numProcs < MIN_BATCH_PROCS){
      stochastic(x, alpha, thresh, maxIter)(hyp)
    }
    else{
      batch(x, alpha, thresh, maxIter)(hyp)
    }
  }

  def optiml_stochastic(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double], thresh: Rep[Double],
                        maxIter: Rep[Int], hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]] = {

    val y = x.labels
    val theta = Vector.zeros(x.numFeatures).mutable
    untilconverged(theta, thresh, maxIter, unit(true)) { theta =>
      for (i <- 0 until x.numSamples) {
        for (j <- 0 until x.numFeatures ) {
          theta(j) = theta(j) + alpha*(y(i) - hyp(x(i)))*x(i)(j)
        }
      }
      theta
    }
  }

  def optiml_batch(x: Rep[TrainingSet[Double,Double]], alpha: Rep[Double], thresh: Rep[Double],
                   maxIter: Rep[Int], hyp: Rep[Vector[Double]] => Rep[Double]): Rep[Vector[Double]] = {

    val y = x.labels
    val theta = Vector.zeros(x.numFeatures).mutable
    untilconverged(theta, thresh, maxIter, unit(true)) { theta =>
      for (j <- 0 until x.numFeatures) {
        val acc = sum(0, x.numSamples) { i =>
          (y(i) - hyp(x(i))*x(i)(j))   // parallel work
        }
        theta(j) = theta(j) + alpha*acc
      }
      theta
    }
  }


  /**
   *  dist
   */
  trait VectorDistance

  case class VectorDistanceAbs[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_vectordistance_abs_impl(v1,v2))) with VectorDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]] //TODO factor into DeliteOp subclass
    }

  case class VectorDistanceEuc[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_vectordistance_euc_impl(v1,v2))) with VectorDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  case class VectorDistanceSquare[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_vectordistance_square_impl(v1,v2))) with VectorDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }


  trait MatrixDistance

  case class MatrixDistanceAbs[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_matrixdistance_abs_impl(m1,m2))) with MatrixDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  case class MatrixDistanceEuc[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_matrixdistance_euc_impl(m1,m2))) with MatrixDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  case class MatrixDistanceSquare[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_matrixdistance_square_impl(m1,m2))) with MatrixDistance {
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }

  def optiml_vector_dist_abs[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = reflectPure(VectorDistanceAbs(v1,v2))
  def optiml_vector_dist_euc[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = reflectPure(VectorDistanceEuc(v1,v2))
  def optiml_vector_dist_square[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = reflectPure(VectorDistanceSquare(v1,v2))
  def optiml_matrix_dist_abs[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = reflectPure(MatrixDistanceAbs(m1,m2))
  def optiml_matrix_dist_euc[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = reflectPure(MatrixDistanceEuc(m1,m2))
  def optiml_matrix_dist_square[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]]) = reflectPure(MatrixDistanceSquare(m1,m2))


  /**
   * Sampling
   */
  
  case class RandSampleMatrix[A:Manifest](m: Exp[Matrix[A]], numSamples: Exp[Int], sampleRows: Exp[Boolean])
    extends DeliteOpSingleTask[Matrix[A]](reifyEffects(optiml_randsample_matrix_impl(m, numSamples, sampleRows)))

  case class RandSampleVector[A:Manifest](v: Exp[Vector[A]], numSamples: Exp[Int])
    extends DeliteOpSingleTask[Vector[A]](reifyEffects(optiml_randsample_vector_impl(v, numSamples)))
  
  def optiml_randsample_matrix[A:Manifest](m: Exp[Matrix[A]], numSamples: Exp[Int], sampleRows: Exp[Boolean]): Exp[Matrix[A]] = {
    reflectPure(RandSampleMatrix(m, numSamples, sampleRows))
  }

  def optiml_randsample_vector[A:Manifest](v: Exp[Vector[A]], numSamples: Exp[Int]): Exp[Vector[A]] = {
    reflectPure(RandSampleVector(v, numSamples))
  }


  /**
   * Nearest neighbor
   */
  def optiml_nearest_neighbor_index[A:Manifest:Arith:Ordering:HasMinMax](row: Rep[Int], m: Rep[Matrix[A]], allowSame: Rep[Boolean]): Rep[Int] = {
    // unroll
    val dists = (0::m.numRows){ i =>
      val d = dist(m(row),m(i))
      if (d == implicitly[Arith[A]].empty && !allowSame) implicitly[HasMinMax[A]].maxValue else d
    }
    dists.minIndex
    /*
    if (allowSame) dists.minIndex
    else {
      val same = dist(m(row), m(row))
      val f = dists filter {  _ != same }
      val x = f.min
      val z = dists find (x)
      z(0)
    }
    */
  }


  /**
   *   Profiling
   */
  case class ProfileStart(deps: List[Exp[Any]]) extends Def[Unit]
  case class ProfileStop(deps: List[Exp[Any]]) extends Def[Unit]

  def profile_start(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStart(deps.toList))
  def profile_stop(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStop(deps.toList))
  
  
  /**
   * Mirroring
   */
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case e@VectorDistanceAbs(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDistanceAbs(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
    case e@VectorDistanceEuc(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDistanceEuc(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
    case e@VectorDistanceSquare(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDistanceSquare(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
    case e@MatrixDistanceAbs(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDistanceAbs(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
    case e@MatrixDistanceEuc(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDistanceEuc(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
    case e@MatrixDistanceSquare(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixDistanceSquare(f(x),f(y))(e.m,e.a))(mtype(manifest[A]))
    case e@Sum(st,en,b,init) => reflectPure(new { override val original = Some(f,e) } with Sum(f(st),f(en),f(b),f(init))(e.m, e.a, e.c))(mtype(manifest[A]))
    case e@SumIf(st,en,c,b,init) => reflectPure(new { override val original = Some(f,e) } with SumIf(f(st),f(en),f(c),f(b),f(init))(e.m, e.a, e.c))(mtype(manifest[A]))
    case Reflect(ProfileStart(deps), u, es) => reflectMirrored(Reflect(ProfileStart(f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ProfileStop(deps), u, es) => reflectMirrored(Reflect(ProfileStop(f(deps)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@Sum(st,en,b,init), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with Sum(f(st),f(en),f(b),f(init))(e.m, e.a, e.c), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SumIf(st,en,c,b,init), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SumIf(f(st),f(en),c,b,f(init))(e.m,e.a,e.c), mapOver(f,u), f(es)))(mtype(manifest[A]))
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

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
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
      case ProfileStart(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(\"app\", false)")
      case ProfileStop(deps) => emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.stop(\"app\", false)")
      case _ => super.emitNode(sym, rhs)
    }
  }
}

/*
trait CudaGenLanguageOps extends CudaGenBase with BaseGenLanguageOps {
  val IR: LanguageOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
     }
  }
}
*/
