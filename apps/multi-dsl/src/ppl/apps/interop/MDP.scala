package ppl.apps.interop

import scala.virtualization.lms.common.Record
import ppl.dsl.optiml._
import ppl.delite.framework.datastructures.DeliteArray

/**
 * An "open-world" example of DSL interoperability 
 * The application nests data structures and operations between OptiLA and OptiCollections
**/

trait OptiCollections extends OptiMLApplication {

  abstract class CMap[K,V] extends Record
  abstract class CSeq[A] extends Record

  object CSeq {
    def fromArray[A:Manifest](a: Rep[DeliteArray[A]]) = sequence_fromArray(a)
  }

  implicit def collectionsSeqToSeqOps[A:Manifest](x: Rep[CSeq[A]]) = new OptiCollectionsSeqClass(x)
  class OptiCollectionsSeqClass[A:Manifest](x: Rep[CSeq[A]]) {
    def apply(n: Rep[Int]): Rep[A] = sequence_apply(x,n)
    def map[B:Manifest](f: Rep[A] => Rep[B]) = sequence_map(x,f)
    def reduce(f: (Rep[A],Rep[A]) => Rep[A])(implicit zero: Rep[A]) = sequence_reduce(x,f,zero)
  }

  def sequence_apply[A:Manifest](x: Rep[CSeq[A]], n: Rep[Int]): Rep[A]
  def sequence_fromArray[A:Manifest](x: Rep[DeliteArray[A]]): Rep[CSeq[A]]
  def sequence_map[A:Manifest,B:Manifest](x: Rep[CSeq[A]], f: Rep[A] => Rep[B]): Rep[CSeq[B]]
  def sequence_reduce[A:Manifest](x: Rep[CSeq[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A]): Rep[A]


  object CMap {
    def empty[K:Manifest,V:Manifest]() = map_fromArray(DeliteArray[(K,V)](0))
    def fromArray[K:Manifest,V:Manifest](data: Rep[DeliteArray[(K,V)]]) = map_fromArray(data)
  }

  implicit def collectionsMapToMapOps[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]) = new OptiCollectionsMapClass(x)
  class OptiCollectionsMapClass[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]) {
    def head = map_head(x)
    def keys = map_keys(x)
    def values = map_values(x)
    def map[K2:Manifest,V2:Manifest](f: Rep[(K,V)] => Rep[(K2,V2)]) = map_map(x,f)
    def reduce(f: (Rep[(K,V)],Rep[(K,V)]) => Rep[(K,V)])(implicit zero: Rep[(K,V)]) = map_reduce(x,f,zero)
    def filter(f: Rep[(K,V)] => Rep[Boolean]) = map_filter(x,f)
    def find(f: Rep[(K,V)] => Rep[Boolean]) = map_find(x,f)
  }

  def map_fromArray[K:Manifest,V:Manifest](data: Rep[DeliteArray[(K,V)]]): Rep[CMap[K,V]]
  def map_head[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]): Rep[(K,V)]
  def map_keys[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]): Rep[CSeq[K]]
  def map_values[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]): Rep[CSeq[V]]
  def map_map[K:Manifest,V:Manifest,K2:Manifest,V2:Manifest](x: Rep[CMap[K,V]], f: Rep[(K,V)] => Rep[(K2,V2)]): Rep[CMap[K2,V2]]
  def map_reduce[K:Manifest,V:Manifest](x: Rep[CMap[K,V]], f: (Rep[(K,V)],Rep[(K,V)]) => Rep[(K,V)], zero: Rep[(K,V)]): Rep[(K,V)]
  def map_filter[K:Manifest,V:Manifest](x: Rep[CMap[K,V]], f: Rep[(K,V)] => Rep[Boolean]): Rep[CMap[K,V]]
  def map_find[K:Manifest,V:Manifest](x: Rep[CMap[K,V]], f: Rep[(K,V)] => Rep[Boolean]): Rep[(K,V)]

  def infix_key[K:Manifest,V:Manifest](x: Rep[(K,V)]): Rep[K]
  def infix_value[K:Manifest,V:Manifest](x: Rep[(K,V)]): Rep[V]

}

trait OptiCollectionsExp extends OptiCollections with OptiMLApplicationRunner {
  
  private def infix_data[A:Manifest](x: Rep[CSeq[A]]) = field[DeliteArray[A]](x, "data")
  def sequence_apply[A:Manifest](x: Rep[CSeq[A]], n: Rep[Int]) = x.data.apply(n)
  def sequence_fromArray[A:Manifest](x: Rep[DeliteArray[A]]) = struct(classTag[CSeq[A]], "data" -> x)
  def sequence_map[A:Manifest,B:Manifest](x: Rep[CSeq[A]], f: Rep[A] => Rep[B]) = CSeq.fromArray(x.data.map(f))
  def sequence_reduce[A:Manifest](x: Rep[CSeq[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A]) = x.data.reduce(f,zero)

  private def infix_data[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]) = field[DeliteArray[(K,V)]](x, "data")
  def map_fromArray[K:Manifest,V:Manifest](data: Rep[DeliteArray[(K,V)]]) = struct(classTag[CMap[K,V]], "data" -> data)
  def map_head[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]) = x.data.apply(0)
  def map_keys[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]) = CSeq.fromArray(x.data.map(_._1))
  def map_values[K:Manifest,V:Manifest](x: Rep[CMap[K,V]]) = CSeq.fromArray(x.data.map(_._2))
  def map_map[K:Manifest,V:Manifest,K2:Manifest,V2:Manifest](x: Rep[CMap[K,V]], f: Rep[(K,V)] => Rep[(K2,V2)]) = CMap.fromArray(x.data.map(f))
  def map_reduce[K:Manifest,V:Manifest](x: Rep[CMap[K,V]], f: (Rep[(K,V)],Rep[(K,V)]) => Rep[(K,V)], zero: Rep[(K,V)]) = x.data.reduce(f,zero)
  def map_filter[K:Manifest,V:Manifest](x: Rep[CMap[K,V]], f: Rep[(K,V)] => Rep[Boolean]) = CMap.fromArray(x.data.filter(f))
  def map_find[K:Manifest,V:Manifest](x: Rep[CMap[K,V]], f: Rep[(K,V)] => Rep[Boolean]) = x.filter(f).head

  def infix_key[K:Manifest,V:Manifest](x: Rep[(K,V)]) = x._1
  def infix_value[K:Manifest,V:Manifest](x: Rep[(K,V)]) = x._2

}


object MDPRunner extends OptiMLApplicationRunner with OptiCollectionsExp with MDPModel

trait MDPModel extends OptiMLApplication with OptiCollections {

  type Action = Double //Record { ... }

  def valueIterationSimple(prob: Rep[DenseMatrix[Double]], costs: Rep[DenseVector[Double]], initValue: Rep[DenseVector[Double]], discountFactor: Rep[Double], tolerance: Rep[Double] = 1e-3, maxIter: Rep[Int] = 1000) = {
    val optimalValue = untilconverged(initValue, tolerance, maxIter) { value => 
      val newValue = costs + discountFactor * (prob * value)
      newValue
    }
    optimalValue
  }

  implicit def zeroVector = Vector.zeros(0)
  implicit def zeroPair = make_tuple2((unit(0.0), zeroVector))

  //infinite horizon value iteration for a Markov Decision Process
  def valueIteration(actionResults: Rep[CMap[Double, (DenseMatrix[Double],DenseVector[Double])]], initValue: Rep[DenseVector[Double]], discountFactor: Rep[Double], tolerance: Rep[Double] = 1e-3, maxIter: Rep[Int] = 1000) = {
    var optimalActions = Vector[Double](0) //FIXME: initializing this to null causes violated ordering of effects
    val optimalValue = untilconverged(initValue, tolerance, maxIter) { value => 
      //check the value for each action at each state
      val allValues: Rep[CMap[Double,DenseVector[Double]]] = actionResults map { e => (e._1, e._2._1 * value * discountFactor + e._2._2) }
      //choose the best action per state and the corresponding value
      val newValue = allValues.values reduce { (v1,v2) => (0::value.length){ i => if (v1(i) <= v2(i)) v1(i) else v2(i) } }
      //optimalActions = allValues reduce { (v1,v2) => make_tuple2(v1._1, (0::v1._2.length){ i => if (v1._2(i) <= v2._2(i)) v1._1 else v2._1 }) }.value
      optimalActions = allValues.values reduce { (v1,v2) => (0::value.length){ i => if (v1(i) <= v2(i)) 0.0 else 1.0 } }
      newValue
    }
    (optimalValue, optimalActions)
  }

  def valueIteration2(actionResults: Rep[CMap[Double, (DenseMatrix[Double], DenseVector[Double])]], initValue: Rep[DenseVector[Double]], discountFactor: Rep[Double], tolerance: Rep[Double], maxIter: Rep[Int]) = {
    var optimalActions = Vector[Double](0)
    var iter = 0
    var value = initValue
    var delta = Double.MaxValue
    while (abs(delta) > tolerance && iter < maxIter) {
      val allValues = actionResults map { e => (e._1, (0::value.length){ i => (e._2._1(i) * value(i) * discountFactor + e._2._2(i)).sum }) }
      val allValues1 = allValues/*.unsafeMutable.unsafeImmutable*/
      val allValues2 = allValues/*.unsafeMutable.unsafeImmutable*/
      val newValue = allValues1.values reduce { (v1,v2) => (0::value.length){ i => if (v1(i) <= v2(i)) v1(i) else v2(i) } }
      optimalActions = allValues2.values reduce { (v1,v2) => (0::value.length){ i => if (v1(i) <= v2(i)) 0.0 else 1.0 } }
      newValue

      iter += 1
      delta = diff(newValue, value)
      value = newValue
    }
    println("iters " + iter)
    (value, optimalActions)
  }

  def diff(x: Rep[DenseVector[Double]], y: Rep[DenseVector[Double]]) = (x-y).abs.sum

  def printUsage = {
    println("MDP <#states> <#actions> <max iters>")
    exit(-1)
  }

  def main() {
    if (args.length < 3) printUsage    

    val size = args(0).toInt
    val numActions = args(1).toInt
    val maxIter  = args(2).toInt
    val tolerance = 1e-3


    val initValue = Vector.rand(size).unsafeImmutable
    val discountFactor = 0.9

    val arr = DeliteArray[(Action, (DenseMatrix[Double], DenseVector[Double]))](numActions)
    for (i <- 0::numActions) {
      val cost = Vector.rand(size).unsafeImmutable
      val P = Matrix.rand(size,size).unsafeImmutable
      arr(i) = make_tuple2(i, make_tuple2(P, cost)) //FIXME: tuple lifting implicits
    }

    val actionResults = CMap.fromArray(arr.unsafeImmutable)

    tic()
    val (value, actions) = valueIteration2(actionResults, initValue, discountFactor, tolerance, maxIter)
    toc(value)
    println(value)
    println(actions)
  }

}
