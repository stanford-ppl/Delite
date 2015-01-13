package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.delite.framework.ops._
import ppl.delite.framework.Util._
import ppl.delite.framework.Config
import scala.collection.mutable.HashSet

// n-dimensional pointer
trait DeliteMultiArrayView[T]

trait DeliteMultiArrayViewOps extends Base {

  implicit def repMultiViewtoMultiViewOps[T:Manifest](v: Rep[DeliteMultiArrayView[T]])(implicit ctx: SourceContext) = new DeliteMultiArrayViewOpsCls(v)

  class DeliteMultiArrayViewOpsCls[T:Manifest](v: Rep[DeliteMultiArrayView[T]])(implicit ctx: SourceContext) {
    def rank: Rep[Int] = dmaview_rank(ma)
    def dim(i: Rep[Int]): Rep[Int] = dmaview_dim(ma,i)
    def shape: Rep[Seq[Rep[Int]]] = dmaview_shape(ma)
    def size: Rep[Int] = dmaview_size(ma)
    def apply(i: Rep[Int]*): Rep[T] = dmaview_apply(ma,i)
    def update(i: Seq[Rep[Int]], x: Rep[T]): Rep[Unit] = dmaview_update(ma,i,x)
    def mutable = dmaview_mutable(ma)

    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteMultiArray[B]] = dmaview_map(ma,f)
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteMultiArray[R]] = dmaview_zipwith(ma,y,f)
    def reduce(f: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] = dmaview_reduce(ma,f,zero)
    def foreach(f: Rep[T] => Rep[Unit]): Rep[Unit] = dmaview_foreach(ma,f)
    def filter(f: Rep[T] => Rep[Boolean]): Rep[DeliteMultiArray[B]] = dmaview_filter(ma,f)
    def flatMap[B:Manifest](f: Rep[T] => Rep[DeliteMultiArray[B]])(implicit ctx: SourceContext) = dmaview_flatmap(ma,f)
    def groupByReduce[K:Manifest, V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]) = dmaview_groupByReduce(ma,key,value,reduce)
 
    def mmap(f: Rep[T] => Rep[T]): Rep[Unit] = dmaview_mmap(ma,f)
    def mzip(y: Rep[DeliteMultiArray[T]])(f: (Rep[T],Rep[T]) => Rep[T]) = dmaview_mzipwith(ma,y,f)
  }

  // --- Compiler internal
  def dmaview_element[A:Manifest](v: Rep[DeliteMultiArrayView[A]]): Rep[A]

  def dmaview_target[A:Manifest](v: Rep[DeliteMultiArrayView[A]]): Rep[DeliteMultiArray[A]]
  def dmaview_start[A:Manifest](v: Rep[DeliteMultiArrayView[A]]): Rep[Seq[Rep[Int]]]
  def dmaview_stride[A:Manifest](v: Rep[DeliteMultiArrayView[A]]): Rep[Seq[Rep[Int]]]

  // --- View creation
  def dmaview_new[A:Manifest](ma: Rep[DeliteMultiArray[A]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArrayView[A]]
  def dmaview_new_immutable[A:Manifest](ma: Rep[DeliteMultiArray[A]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArrayView[A]]

  // --- Array properties
  def dmaview_rank[A:Manifest](v: Rep[DeliteMultiArrayView[A]])(implicit ctx: SourceContext): Rep[Int]
  def dmaview_shape[A:Manifest](v: Rep[DeliteMultiArrayView[A]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]
  def dmaview_size[A:Manifest](v: Rep[DeliteMultiArrayView[A]])(implicit ctx: SourceContext): Rep[Int]

  // --- Array single element
  def dmaview_apply[A:Manifest](v: Rep[DeliteMultiArrayView[A]], i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[A]
  def dmaview_update[A:Manifest](v: Rep[DeliteMultiArrayView[A]], i: Seq[Rep[Int]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array copies / reshaping
  def dmaview_mutable[A:Manifest](v: Rep[DeliteMultiArrayView[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArrayView[A]]
  def dmaview_todense[A:Manifest](v: Rep[DeliteMultiArrayView[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Parallel ops 
  // TODO: do these need to be here? should they be two separate ops with dmaview_todense and dmultia_*?
  // if we need these, should there also be a zipwith(view,view)? should we have a zipwith(array,view?)
  def dmaview_map[A:Manifest,B:Manifest](v: Rep[DeliteMultiArrayView[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmaview_zipwith[A:Manifest,B:Manifest,R:Manifest](v1: Rep[DeliteMultiArrayView[A]], v2: Rep[DeliteMultiArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[DeliteMultiArray[R]]
  def dmaview_reduce[A:Manifest](v: Rep[DeliteMultiArrayView[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def dmaview_foreach[A:Manifest](v: Rep[DeliteMultiArrayView[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]

  def dmaview_mmap[A:Manifest](v: Rep[DeliteMultiArrayView[A]], f: Rep[T] => Rep[R])(implicit ctx: SourceContext): Rep[Unit]
  def dmaview_mzipwith[A:Manifest](v: Rep[DeliteMultiArrayView[A]], rhs: Rep[DeliteMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[Unit] 

  def dmaview_flatmap[A:Manifest,B:Manifest](v: Rep[DeliteMultiArrayView[A]], f: Rep[A] => Rep[DeliteMultiArray[B]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]

  // These operations are defined only on 1D arrays (will need a check to ensure this)
  def dmaview_filter[A:Manifest](v: Rep[DeliteMultiArrayView[A]], f: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmaview_groupByReduce[A:Manifest,K:Manifest,V:Manifest](v: Rep[DeliteMultiArrayView[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMap[K,V]]

  //def dmaview_scatter[A:Manifest](v: Rep[DeliteMultiArrayView[A]], rhs: Rep[DeliteMultiArray[A]])
}

trait DeliteMultiArrayOpsExp extends BaseExp with DeliteMultiArrayOps {
  this: DeliteOpsExp =>

  /////////////////////
  // Abstract IR Nodes

  // --- Compiler internal
  case class DeliteMultiArrayViewElement[T:Manifest](v: Exp[DeliteMultiArrayView[T]]) extends DefWithManifest[T,T]

  case class DeliteMultiArrayViewTarget[T:Manifest](v: Exp[DeliteMultiArrayView[T]]) extends Def[DeliteMultiArray[T]]
  case class DeliteMultiArrayViewStart[T:Manifest](v: Exp[DeliteMultiArrayView[T]]) extends DefWithManifest[T,Seq[Exp[Int]]]
  case class DeliteMultiArrayViewStride[T:Manifest](v: Exp[DeliteMultiArrayView[T]]) extends DefWithManifest[T,Seq[Exp[Int]]]

  // --- View creation
  case class DeliteMultiArrayViewNew[T:Manifest](ma: Exp[DeliteMultiArray[T]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) extends Def[DeliteMultiArrayView[T]]

  // --- Array properties
  case class DeliteMultiArrayViewRank[T:Manifest](v: Exp[DeliteMultiArrayView[T]]) extends DefWithManifest[T,Int]
  case class DeliteMultiArrayViewShape[T:Manifest](v: Exp[DeliteMultiArrayView[T]]) extends DefWithManifest[T,Seq[Exp[Int]]]
  case class DeliteMultiArrayViewSize[T:Manifest](v: Exp[DeliteMultiArrayView[T]]) extends DefWithManifest[T,Int]

  // --- Array single element ops
  case class DeliteMultiArrayViewApply[T:Manifest](v: Exp[DeliteMultiArrayView[T]], i: Seq[Exp[Int]]) extends DefWithManifest[T,T]
  case class DeliteMultiArrayViewUpdate[T:Manifest](v: Exp[DeliteMultiArrayView[T]], i: Seq[Exp[Int]], x: Exp[T]) extends DefWithManifest[T,Unit]

  // --- Array parallel ops
  case class DeliteMultiArrayViewMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArrayView[A]], func: Exp[A] => Exp[B])(implicit ctx: SourceContext) extends Def[DeliteMultiArray[B]] {
    val body = reifyEffects(func(dmaview_element(in)))

    val mA = manifest[A]
    val mB = manifest[B]
  }

  case class DeliteMultiArrayViewZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArrayView[A]], inB: Exp[DeliteMultiArray[B]], func: (Exp[A],Exp[B]) => Exp[R]) extends Def[DeliteMultiArray[R]] {
    val body = reifyEffects(func(dmaview_element(inA), dmaview_element(inB)))

    val mA = manifest[A]
    val mB = manifest[B]
    val mR = manifest[R]
  }
  case class DeliteMultiArrayViewReduce[A:Manifest](in: Exp[DeliteMultiArrayView[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,DeliteMultiArray[A]] {
    val body = reifyEffects(func(dmaview_element(in), dmaview_element(in)))
  }
  case class DeliteMultiArrayViewForeach[A:Manifest](in: Exp[DeliteMultiArrayView[A]], func: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit] {
    val body = reifyEffects(func(dmaview_element(in)))
  }

  case class DeliteMultiArrayViewMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteMultiArrayView[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) extends Def[DeliteMultiArray[B]] {
    val element = dmaview_element(in)
    val body = reifyEffects(func(element))
    val filt = reifyEffects(cond(element))

    val mA = manifest[A]
    val mB = manifest[B]
  }
  case class DeliteMultiArrayViewFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArrayView[A]], func: Exp[A] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) extends Def[DeliteMultiArray[B]] {
    val body = reifyEffects(func(dmaview_element(in)))

    val mA = manifest[A]
    val mR = manifest[R]
  }

  case class DeliteMultiArrayViewGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArrayView[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) extends Def[DeliteMap[K,V]] {
    val element = dmaview_element(ma)
    val keyFunc = reifyEffects(key(element))
    val valueFunc = reifyEffects(value(element))
    val sV = (fresh[V], fresh[V])
    val reducFunc = reifyEffects(reduce(sV._1,sV._2))

    val mA = manifest[A]
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class DeliteMultiArrayViewMutableMap[A:Manifest](in: Exp[DeliteMultiArrayView[A]], func: Exp[A] => Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit] {
    val body = reifyEffects(func(dmaview_element(ma)))
  }
  case class DeliteMultiArrayViewMutableZipWith[A:Manifest](inA: Exp[DeliteMultiArrayView[A]], inB: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit] {
    val body = reifyEffects(func(dmaview_element(inA), dmaview_element(inB)))
  }

  //////////////////////
  // Array IR Functions

  // --- Compiler internal
  def dmaview_element[A:Manifest](v: Exp[DeliteMultiArrayView[A]]) = reflectPure(DeliteMultiArrayViewElement(v))

  def dmaview_target[A:Manifest](v: Exp[DeliteMultiArrayView[A]]) = reflectPure(DeliteMultiArrayViewTarget(v))
  def dmaview_start[A:Manifest](v: Exp[DeliteMultiArrayView[A]]) = reflectPure(DeliteMultiArrayViewStart(v))
  def dmaview_stride[A:Manifest](v: Exp[DeliteMultiArrayView[A]]) = reflectPure(DeliteMultiArrayViewStride(v))

  // --- View creation
  def dmaview_new[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext)
    = reflectMutable(DeliteMultiArrayViewNew(v,start,stride,dims))
  def dmaview_new_immutable[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext)
    = reflectPure(DeliteMultiArrayViewNew(v,start,stride,dims))

  // --- Array properties
  def dmaview_rank[A:Manifest](v: Exp[DeliteMultiArrayView[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewRank(v))
  def dmaview_shape[A:Manifest](v: Exp[DeliteMultiArrayView[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewShape(v))
  def dmaview_size[A:Manifest](v: Exp[DeliteMultiArrayView[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewSize(v))

  // --- Array single element
  def dmaview_apply[A:Manifest](v: Exp[DeliteMultiArrayView[A]], i: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewApply(v,i))
  // definitely shouldn't reflect write on a Rep that's always going to be immutable
  def dmaview_update[A:Manifest](v: Exp[DeliteMultiArrayView[A]], i: Seq[Exp[Int]], x: Exp[A])(implicit ctx: SourceContext) 
    = reflectWrite(v /*, dmaview_target(v)*/)(DeliteMultiArrayViewUpdate(v,i,x))

  // --- Array copies / reshaping
  // TODO: Should view.mutable give a mutable view of the original target? or a mutable new array?
  // should a mutable view be allowed? maybe when the target is mutable, but if what if it's not?
  def dmaview_mutable[A:Manifest](v: Exp[DeliteMultiArrayView[A]])(implicit ctx: SourceContext) = reflectMutable(DeliteMultiArrayViewMap(v, (e: Exp[A])=>e))
  def dmaview_todense[A:Manifest](v: Exp[DeliteMultiArrayView[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewMap(v, (e: Exp[A])=>e))

  // --- Parallel ops 
  // TODO: do these need to be here? should they be two separate ops with dmaview_todense and dmultia_*?
  // if we need these, should there also be a zipwith(view,view)? should we have a zipwith(array,view?)
  def dmaview_map[A:Manifest,B:Manifest](v: Exp[DeliteMultiArrayView[A]], f: Exp[A] => Exp[B])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewMap(v,f))
  def dmaview_zipwith[A:Manifest,B:Manifest,R:Manifest](v1: Exp[DeliteMultiArrayView[A]], v2: Exp[DeliteMultiArray[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewZipWith(v1,v2,f))
  def dmaview_reduce[A:Manifest](v: Exp[DeliteMultiArrayView[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewReduce(v,f,zero))
  def dmaview_foreach[A:Manifest](v: Exp[DeliteMultiArrayView[A]], f: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewForeach(v,f))

  def dmaview_mmap[A:Manifest](v: Exp[DeliteMultiArrayView[A]], f: Exp[T] => Exp[R])(implicit ctx: SourceContext) = reflectWrite(v)(DeliteMultiArrayViewMutableMap(v,f))
  def dmaview_mzipwith[A:Manifest](v: Exp[DeliteMultiArrayView[A]], rhs: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A])(implicit ctx: SourceContext) = reflectWrite(v)(DeliteMultiArrayViewMutableZipWith(v,rhs,f))

  def dmaview_flatmap[A:Manifest,B:Manifest](v: Exp[DeliteMultiArrayView[A]], f: Exp[A] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewFlatMap(v,f))

  def dmaview_filter[A:Manifest](v: Exp[DeliteMultiArrayView[A]], f: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayViewMapFilter(v,f))
  def dmaview_groupByReduce[A:Manifest,K:Manifest,V:Manifest](v: Exp[DeliteMultiArrayView[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext)
    = reflectPure(DeliteMultiArrayViewGroupByReduce(v,key,value,reduce))

}
