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
  def sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int])(block: Rep[Int] => Rep[A]) = optiml_sum(start, end, block)
  def sum[A:Manifest:Arith](vals: Rep[Vector[A]]) = repVecToVecOps(vals).sum
  def sum[A](vals: Rep[Matrix[A]])(implicit mA: Manifest[A], a: Arith[A], o: Overloaded1) = repMatToMatOps(vals).sum

  def optiml_sum[A:Manifest:Arith](start: Rep[Int], end: Rep[Int], block: Rep[Int] => Rep[A]): Rep[A]


  /**
   * min
   */
  def min[A:Manifest:Ordering](vals: Rep[Vector[A]]) = repVecToVecOps(vals).min
  def min[A](vals: Rep[Matrix[A]])(implicit mA: Manifest[A], ord: Ordering[A], o: Overloaded1) = repMatToMatOps(vals).min
  //def min[A:Manifest:Ordering](vals: A*) = repVecToVecOps(Vector(vals: _*)).min
  def min[A:Manifest:Ordering](vals: Rep[A]*) = repVecToVecOps(Vector(vals: _*)).min

  /**
   * max
   */
  def max[A:Manifest:Ordering](vals: Rep[Vector[A]]) = repVecToVecOps(vals).max
  def max[A](vals: Rep[Matrix[A]])(implicit mA: Manifest[A], ord: Ordering[A], o: Overloaded1) = repMatToMatOps(vals).max
  //def max[A:Manifest:Ordering](vals: A*) = repVecToVecOps(Vector(vals: _*)).max
  def max[A:Manifest:Ordering](vals: Rep[A]*) = repVecToVecOps(Vector(vals: _*)).max


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
  implicit def tuple2ToIndexVector2(tup: (IndexWildcard, Rep[IndexVector]))(implicit overloaded2 : Overloaded2) = indexvector2_new(indexvector2_wildcard(), tup._2)
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
   *  input parsing
   */
  def loadMatrix(filename: Rep[String]) = MLInputReader.read(filename)
  def loadVector(filename: Rep[String]) = MLInputReader.readVector(filename)
  def loadImage(filename: Rep[String]) = MLInputReader.readGrayscaleImage(filename)


  /**
   * distance
   */
  val ABS = 0
  val EUC = 1
  val SQUARE = 2

  implicit val vecDiff: (Rep[Vector[Double]], Rep[Vector[Double]]) => Rep[Double] = (v1,v2) => dist(v1,v2)
  implicit val matDiff: (Rep[Matrix[Double]], Rep[Matrix[Double]]) => Rep[Double] = (m1,m2) => dist(m1,m2)

  // in 2.9, multiple overloaded values cannot all define default arguments
  def dist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]]) = {
    optiml_vector_dist(v1,v2,ABS)
  }

  def dist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int]) = {
    optiml_vector_dist(v1,v2,metric)
  }

  def dist[A](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]])
             (implicit mA: Manifest[A], a: Arith[A], o: Overloaded1) = {
    optiml_matrix_dist(m1,m2,ABS)
  }
 
  def dist[A](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int])
             (implicit mA: Manifest[A], a: Arith[A], o: Overloaded1) = {
    optiml_matrix_dist(m1,m2,metric)
  }

  def optiml_vector_dist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int]): Rep[A]
  def optiml_matrix_dist[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int]): Rep[A]


  /**
   * Sampling
   */

  val RANDOM = 0

  // sampling of input to reduce data size
  def sample[A:Manifest](m: Rep[Matrix[A]], numSamples: Rep[Int], sampleRows: Rep[Boolean] = unit(true), method: Rep[Int] = RANDOM): Rep[Matrix[A]] = {
    method match {
      case RANDOM => optiml_randsample_matrix(m, numSamples, sampleRows)
      case _ => throw new UnsupportedOperationException("unknown sampling type selected")
    }
  }

  def sample[A:Manifest](v: Rep[Vector[A]], numSamples: Rep[Int]): Rep[Vector[A]] = {
    optiml_randsample_vector(v, numSamples)
  }

  def sample[A:Manifest](v: Rep[Vector[A]], numSamples: Rep[Int], method: Rep[Int]): Rep[Vector[A]] = {
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
  def nearestNeighborIndex[A:Manifest:Arith:Ordering](row: Rep[Int], data: Rep[Matrix[A]], allowSame: Rep[Boolean] = unit(true)): Rep[Int]
    = optiml_nearest_neighbor_index(row, data, allowSame)

  def optiml_nearest_neighbor_index[A:Manifest:Arith:Ordering](row: Rep[Int], data: Rep[Matrix[A]], allowSame: Rep[Boolean]): Rep[Int]

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
  case class Sum[A:Manifest:Arith](start: Exp[Int], end: Exp[Int], mV: Sym[Int], map: Exp[A])
    extends DeliteOpMapReduce[Int,A,Vector] {

    val in = Vector.range(start, end)
    val rV = (fresh[A],fresh[A])
    val reduce = reifyEffects(rV._1 += rV._2) //TODO TR write to non-mutable
    //val mapreduce = reifyEffects(ops.+=(acc, reifyEffects(block(mV))))
  }

  def optiml_sum[A:Manifest:Arith](start: Exp[Int], end: Exp[Int], block: Exp[Int] => Exp[A]) = {

    val mV = fresh[Int]
    val map = reifyEffects(block(mV))
    //Sum(start, end, mV, map)
    // HACK -- better scheduling performance in our apps, forces some expensive dependencies to be hoisted
    reflectEffect(Sum(start, end, mV, map))
  }


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

    val tasks = vertices.cloneL
    val seen = Set[V]()
    
    while(tasks.length > 0) {
      tasks.foreach(block)
      tasks.clear()
      var totalTasks = var_new(unit(0))
      
      for(i <- 0 until vertices.length) {
        val vtasks = vertices(i).tasks
        totalTasks += vtasks.length
        for(j <- 0 until vtasks.length) {
          val task = vtasks(j).asInstanceOfL[V]
          if(!seen.contains(task)) {
            tasks += task   //TODO TR: non-mutable write (use mclone)
            seen.add(task)   //TODO TR: non-mutable write
          }
        }

        vertices(i).clearTasks() //TODO TR: non-mutable write
      }

      //println("tasks: " + tasks.length)
      seen.clear()
    }
  }

  def optiml_untilconverged[A:Manifest:Cloneable](x: Exp[A], thresh: Exp[Double], max_iter: Exp[Int], clone_prev_val: Exp[Boolean],
                                                  block: Exp[A] => Exp[A], diff: (Exp[A],Exp[A]) => Exp[Double]) = {

    var delta = var_new(unit(scala.Double.MaxValue))
    var prev = var_new(unit(null).asInstanceOfL[A])
    var next = var_new(x)
    var iter = var_new(unit(0))

    while ((Math.abs(delta) > thresh) && (iter < max_iter)){
      if (clone_prev_val)
        prev = next.cloneL()
      else
        prev = next

//      try{
        next = block(next)
//      }
//      catch{
//        case e: Exception => throw new ConvergenceException("Converging block threw exception: " + e)
//      }
      iter += 1
      //prev.asInstanceOfL[Matrix[Any]].pprint
      //next.asInstanceOfL[Matrix[Any]].pprint
      delta = diff(next,prev)
      //println("(" + delta + ")")
    }

    if (iter == max_iter){
      //throw new ConvergenceException("Maximum iterations exceeded")
      println(unit("Maximum iterations exceeded"))
      returnL()
    }

    next
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
    val theta = Vector.mzeros(x.numFeatures)
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
    val theta = Vector.mzeros(x.numFeatures)
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
  case class VectorDistance[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_vectordistance_impl(v1,v2,metric)))

  case class MatrixDistance[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int])
    extends DeliteOpSingleTask[A](reifyEffects(optiml_matrixdistance_impl(m1,m2,metric)))

  def optiml_vector_dist[A:Manifest:Arith](v1: Rep[Vector[A]], v2: Rep[Vector[A]], metric: Rep[Int]) = VectorDistance(v1,v2,metric)
  def optiml_matrix_dist[A:Manifest:Arith](m1: Rep[Matrix[A]], m2: Rep[Matrix[A]], metric: Rep[Int]) = MatrixDistance(m1,m2,metric)


  /**
   * Sampling
   */
  
  case class RandSampleMatrix[A:Manifest](m: Exp[Matrix[A]], numSamples: Exp[Int], sampleRows: Exp[Boolean])
    extends DeliteOpSingleTask[Matrix[A]](reifyEffects(optiml_randsample_matrix_impl(m, numSamples, sampleRows)))

  case class RandSampleVector[A:Manifest](v: Exp[Vector[A]], numSamples: Exp[Int])
    extends DeliteOpSingleTask[Vector[A]](reifyEffects(optiml_randsample_vector_impl(v, numSamples)))
  
  def optiml_randsample_matrix[A:Manifest](m: Exp[Matrix[A]], numSamples: Exp[Int], sampleRows: Exp[Boolean]): Exp[Matrix[A]] = {
    RandSampleMatrix(m, numSamples, sampleRows)
  }

  def optiml_randsample_vector[A:Manifest](v: Exp[Vector[A]], numSamples: Exp[Int]): Exp[Vector[A]] = {
    RandSampleVector(v, numSamples)
  }


  /**
   * Nearest neighbor
   */
  def optiml_nearest_neighbor_index[A:Manifest:Arith:Ordering](row: Rep[Int], m: Rep[Matrix[A]], allowSame: Rep[Boolean]): Rep[Int] = {
    // unroll
    val dists = (0::m.numRows){ i => dist(m(row),m(i)) }
    if (allowSame) dists.minIndex
    else {
      val same = dist(m(row), m(row))
      dists filter { _ == same } minIndex
    }
  }


  /**
   *   Profiling
   */
  case class ProfileStart(deps: Exp[Seq[Any]]) extends Def[Unit]
  case class ProfileStop(deps: Exp[Seq[Any]]) extends Def[Unit]

  def profile_start(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStart(Seq(deps: _*)))
  def profile_stop(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStop(Seq(deps: _*)))
}

trait BaseGenLanguageOps extends GenericFatCodegen {
  val IR: LanguageOpsExp
  import IR._

  /*
  override def syms(e: Any): List[Sym[Any]] = e match {
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = rhs match {
    case _ => super.getFreeVarNode(rhs)
  }
  */
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
