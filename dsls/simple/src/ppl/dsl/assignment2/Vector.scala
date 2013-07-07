package ppl.dsl.assignment2

import virtualization.lms.common.{Variables, VariablesExp, BaseFatExp}
import scala.reflect.SourceContext
import ppl.delite.framework.ops._
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._
import java.io.PrintWriter

/**
 * Operations
 */

trait VectorOps extends Variables {
  this: SimpleVector =>

  trait Vector[A] extends DeliteCollection[A]

  //syntax
  object Vector {
    def apply[A:Manifest](length: Rep[Int]) = vectorNew(length)
  }
    
  implicit class VectorOpsCls[A:Manifest](x: Rep[Vector[A]]) {
    def +(y: Rep[Vector[A]])(implicit n: Numeric[A]) = vectorPlus(x,y)
    def +(y: Rep[A])(implicit n: Numeric[A], o: Overloaded1) = vectorPlusScalar(x,y)
    def *(y: Rep[A])(implicit n: Numeric[A]) = scalarTimes(x,y)
    def sum(implicit n: Numeric[A]) = vectorSum(x)
    def filter(pred: Rep[A] => Rep[Boolean]) = vectorFilter(x, pred)
    def length = vectorLength(x)
    def apply(idx: Rep[Int]) = vectorApply(x, idx)
    def pprint = vectorPrint(x)  
  } 
  
  //operations
  def vectorNew[A:Manifest](length: Rep[Int]): Rep[Vector[A]]
  def vectorPlus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vectorPlusScalar[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def scalarTimes[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def vectorSum[A:Manifest:Numeric](x: Rep[Vector[A]]): Rep[A]
  def vectorFilter[A:Manifest](x: Rep[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[Vector[A]]
  def vectorLength[A:Manifest](x: Rep[Vector[A]]): Rep[Int]
  def vectorApply[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int]): Rep[A]
  def vectorPrint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorOpsExp extends VectorOps with DeliteCollectionOpsExp with DeliteStructsExp {
  this: SimpleVectorExp =>
    
  //implemented via kernel embedding (sequential)
  case class PPrint[A:Manifest](x: Exp[Vector[A]], print: Block[Unit])
    extends DeliteOpSingleTask(print)
  
  //implemented via Delite ops
  case class VectorPlus[A:Manifest:Numeric](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector[A]] {

    override def alloc(len: Exp[Int]) = Vector[A](len)
    val size = copyTransformedOrElse(_.size)(inA.length)
    
    def func = (a,b) => a + b
  }
  
  abstract class VectorMap[A:Manifest](in: Exp[Vector[A]]) extends DeliteOpMap[A,A,Vector[A]] {
    override def alloc(len: Exp[Int]) = Vector[A](len)
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
  
  case class VectorFilter[A:Manifest](in: Exp[Vector[A]], cond: Exp[A] => Exp[Boolean]) extends DeliteOpFilter[A,A,Vector[A]] {
    val size = copyTransformedOrElse(_.size)(in.length)
    override def alloc(len: Exp[Int]) = Vector[A](len)    
    def func = a => a
  }

  case class VectorNew[A:Manifest](len: Exp[Int]) extends DeliteStruct[Vector[A]] {
    val elems = copyTransformedElems(Seq("data" -> var_new(DeliteArrayBuffer[A](len)).e))
    val mA = manifest[A]
  }

  case class VectorNewImm[A:Manifest](data: Exp[DeliteArray[A]], len: Exp[Int]) extends DeliteStruct[Vector[A]] {
    val elems = copyTransformedElems(Seq("data" -> DeliteArrayBuffer(data, len)))
    val mA = manifest[A]
  }


  def vectorNew[A:Manifest](length: Exp[Int]) = reflectMutable(VectorNew(length))
  private def infix_data[A:Manifest](x: Exp[Vector[A]]) = field[DeliteArrayBuffer[A]](x, "data")
  def vectorLength[A:Manifest](x: Exp[Vector[A]]) = x.data.length
  def vectorApply[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int]) = x.data.apply(idx) 
  def vectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectPure(VectorPlus(x,y))
  def vectorPlusScalar[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = reflectPure(VectorPlusScalar(x,y))
  def scalarTimes[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = reflectPure(VectorTimesScalar(x, y))
  def vectorSum[A:Manifest:Numeric](x: Exp[Vector[A]]) = reflectPure(VectorSum(x))
  def vectorFilter[A:Manifest](x: Exp[Vector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(VectorFilter(x,pred))
  def vectorPrint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(PPrint(x, reifyEffectsHere(pprint_impl(x))))


  // /////////////////////
  // delite collection
  
  def isVec[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.tp.erasure,classOf[Vector[A]])  
  def asVec[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[Vector[A]]]
    
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isVec(x)) asVec(x).length
    else super.dc_size(x)
  }
  
  override def dc_set_logical_size[A:Manifest](x: Exp[DeliteCollection[A]], y: Exp[Int])(implicit ctx: SourceContext) = {
    if (isVec(x)) dc_set_logical_size(asVec(x).data, y)
    else super.dc_set_logical_size(x,y)        
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isVec(x)) asVec(x).apply(n)
    else super.dc_apply(x,n)    
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isVec(x)) dc_update(asVec(x).data, n, y)
    else super.dc_update(x,n,y)        
  }

  override def dc_appendable[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isVec(x)) unit(true)
    else super.dc_appendable(x,i,y)        
  }  
  
  override def dc_append[A:Manifest](x: Exp[DeliteCollection[A]], i: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isVec(x)) dc_append(asVec(x).data, i, y)
    else super.dc_append(x,i,y)        
  }  
  
  override def dc_alloc[A:Manifest,CA<:DeliteCollection[A]:Manifest](x: Exp[CA], size: Exp[Int])(implicit ctx: SourceContext): Exp[CA] = {
    if (isVec(x)) Vector[A](size).asInstanceOf[Exp[CA]]
    else super.dc_alloc[A,CA](x,size)
  }  
  
  override def dc_copy[A:Manifest](src: Exp[DeliteCollection[A]], srcPos: Exp[Int], dst: Exp[DeliteCollection[A]], dstPos: Exp[Int], size: Exp[Int])(implicit ctx: SourceContext): Exp[Unit] = {
    if (isVec(src) && isVec(dst)) dc_copy(asVec(src).data, srcPos, asVec(dst).data, dstPos, size)
    else super.dc_copy(src,srcPos,dst,dstPos,size)
  }

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = {
    val m = manifest[T]
    if (m.erasure == classOf[Vector[_]]) Some((classTag(m), List("data" -> manifest[DeliteArrayBuffer[T]])))
    else super.unapplyStructType
  }

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
    for (i <- 0 until x.length) {
      print(x(i)); print(" ")
    }
    print("]\n")
  }
}
