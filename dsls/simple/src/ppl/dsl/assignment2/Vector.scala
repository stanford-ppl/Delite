package ppl.dsl.assignment2

import virtualization.lms.common.{Variables, VariablesExp, BaseFatExp}
import scala.reflect.SourceContext
import ppl.delite.framework.ops._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.datastructures._
import java.io.PrintWriter

/**
 * Operations
 */

trait Vector[A] extends DeliteCollection[A]

trait VectorOps extends Variables {
  this: SimpleVector =>
    
  //syntax
  def Vector[A:Manifest](length: Rep[Int]) = vectorNew(length)
  def infix_+[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]) = vectorPlus(x,y)
  def infix_+[A](x: Rep[Vector[A]], y: Rep[A])(implicit m: Manifest[A], n: Numeric[A], o: Overloaded1) = vectorPlusScalar(x,y)
  
  def infix_*[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]) = scalarTimes(x,y)
  def infix_sum[A:Manifest:Numeric](x: Rep[Vector[A]]) = vectorSum(x)
  def infix_filter[A:Manifest](x: Rep[Vector[A]]) = vectorFilter(x)
  
  def infix_length[A:Manifest](x: Rep[Vector[A]]) = length(x)
  def infix_apply[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int]) = apply(x, idx)
  
  def infix_pprint[A:Manifest](x: Rep[Vector[A]]) = vectorPrint(x)
  
  //operations
  def vectorNew[A:Manifest](length: Rep[Int]): Rep[Vector[A]]
  
  def vectorPlus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vectorPlusScalar[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def scalarTimes[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vectorSum[A:Manifest:Numeric](x: Rep[Vector[A]]): Rep[A]
  def vectorFilter[A:Manifest](x: Rep[Vector[A]]): Rep[Vector[A]]

  
  def length[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def apply[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int]): Rep[A]
  
  def vectorPrint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp with DeliteCollectionOpsExp {
  this: SimpleVectorExp =>
    
  //implemented via kernel embedding (sequential)
  case class PPrint[A:Manifest](x: Exp[Vector[A]], print: Block[Unit])
    extends DeliteOpSingleTask(print)
  
  //implemented via Delite ops
  case class VectorPlus[A:Manifest:Numeric](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector[A]] {

    override def alloc = Vector[A](inA.length) // ignored
    override def allocWithArray = data => struct[Vector[A]](List("Vector"), Map("data" -> data))
    val size = copyTransformedOrElse(_.size)(inA.length)
    
    def func = (a,b) => a + b
  }
  
  abstract class VectorMap[A:Manifest:Numeric](in: Exp[Vector[A]]) extends DeliteOpMap[A,A,Vector[A]] {
    override def alloc = Vector[A](in.length) // not used
    override def allocWithArray = data => struct[Vector[A]](List("Vector"), Map("data" -> data))
    val size = copyTransformedOrElse(_.size)(in.length)
  }

  case class VectorPlusScalar[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A]) extends VectorMap[A](in) {
    def func = e => e + s
  }
  
  case class VectorTimesScalar[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A]) extends VectorMap[A](in) {
    def func = e => e * s
  }
  
  case class VectorSum[A:Manifest:Numeric](in: Exp[Vector[A]]) extends DeliteOpReduce[A] {
    val size = copyTransformedOrElse(_.size)(in.length)
    val zero = unit(0).AsInstanceOf[A]
    def func = (a,b) => a + b
  }
  
  case class VectorFilter[A:Manifest](in: Exp[Vector[A]]) extends DeliteOpFilter[A,A,Vector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)
    override def alloc = Vector[A](0) // not used
    override def allocWithArray = data => struct[Vector[A]](List("Vector"), Map("data" -> data))
    
    def func = a => a
    def cond = a => v > 50
  }
  
  def vectorNew[A:Manifest](length: Exp[Int]) = struct[Vector[A]](List("Vector"), Map("data" -> DeliteArray[A](length)))   
  private def infix_data[A:Manifest](x: Exp[Vector[A]]) = field[DeliteArray[A]](x, "data")
  
  //def length[A:Manifest](x: Exp[Vector[A]]) = x.data.length
  def length[A:Manifest](x: Exp[Vector[A]]) = darray_length(x.data)
  def apply[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int]) = darray_apply(x.data, idx)
  
  def vectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = VectorPlus(x,y)
  def vectorPlusScalar[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = VectorPlusScalar(x,y)
  def scalarTimes[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = VectorTimesScalar(x, y)
  def vectorSum[A:Manifest:Numeric](x: Exp[Vector[A]]) = VectorSum(x)
  def vectorFilter[A:Manifest](x: Exp[Vector[A]]) = VectorFilter(x)

  def vectorPrint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(PPrint(x, reifyEffectsHere(pprint_impl(x))))

  private def ifVector[A:Manifest, R](x: Exp[DeliteCollection[A]])(then: Exp[Vector[A]] => R)(orElse: => R): R = {
    if (x.tp.erasure == classOf[Vector[A]]) then(x.asInstanceOf[Exp[Vector[A]]]) else orElse
  }

  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext): Exp[Int] = ifVector(x)(length(_))(super.dc_size(x))
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], idx: Exp[Int])(implicit ctx: SourceContext): Exp[A] = ifVector(x)(apply(_, idx))(super.dc_apply(x, idx))
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], idx: Exp[Int], value: Exp[A])(implicit ctx: SourceContext): Exp[Unit] = ifVector(x)(v => reifyEffectsHere(darray_update(v.data, idx, value)))(super.dc_update(x,idx,value))  
}

/**
 * Implementation using kernel embedding
 */
trait VectorImplOps { this: SimpleVector =>
  def pprint_impl[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: SimpleVectorCompiler with SimpleVectorLift =>

  def pprint_impl[A:Manifest](x: Rep[Vector[A]]) = {
    print("[ ")
    for (i <- 0 until length(x)) {
      print(x.apply(i)); print(" ")
    }
    print("]\\n")
  }
}
