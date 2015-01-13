package ppl.delite.framework.datastructures

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import ppl.delite.framework.ops._
import ppl.delite.framework.Util._
import ppl.delite.framework.Config
import scala.collection.mutable.HashSet

// Abstract, n-dimensional, multi-purpose array
// Intended for use at frontend w/ transformer to concrete data layouts
trait DeliteMultiArray[T]

trait DeliteMultiArrayOps extends Base {
  
  object DeliteMultiArray {
    def apply[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = dmultia_new(dims)
    def imm[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = dmultia_new_immutable(dims)
    def fromFunction[T:Manifest](dims: Rep[Int]*)(func: Rep[Seq[Rep[Int]]] => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(dims, func)

    def sortIndices(length: Rep[Int])(comparator: (Rep[Int], Rep[Int]) => Rep[Int])(implicit ctx: SourceContext) = dmultia_sortIndices(length, comparator)
  }

  implicit def repMultiAtoMultiAOps[T:Manifest](ma: Rep[DeliteMultiArray[T]])(implicit ctx: SourceContext) = new DeliteMultiArrayOpsCls(ma)

  class DeliteMultiArrayOpsCls[T:Manifest](ma: Rep[DeliteMultiArray[T]])(implicit ctx: SourceContext) {
    // --- properties
    def rank: Rep[Int] = dmultia_rank(ma)
    def dim(i: Rep[Int]): Rep[Int] = dmultia_dim(ma,i)
    def shape: Rep[Seq[Rep[Int]]] = dmultia_shape(ma)
    def size: Rep[Int] = dmultia_size(ma)
    
    // --- single element
    def apply(i: Rep[Int]*): Rep[T] = dmultia_apply(ma,i)
    def update(i: Seq[Rep[Int]], x: Rep[T]): Rep[Unit] = dmultia_update(ma,i,x)
    
    // --- copies/reshaping
    def mutable = dmultia_mutable(ma)
    
    // --- parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteMultiArray[B]] = dmultia_map(ma,f)
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteMultiArray[R]] = dmultia_zipwith(ma,y,f)
    def reduce(f: (Rep[T],Rep[T]) => Rep[T], zero: Rep[T]): Rep[T] = dmultia_reduce(ma,f,zero)
    def foreach(f: Rep[T] => Rep[Unit]): Rep[Unit] = dmultia_foreach(ma,f)
    def filter(f: Rep[T] => Rep[Boolean]): Rep[DeliteMultiArray[B]] = dmultia_filter(ma,f)
    def flatMap[B:Manifest](f: Rep[T] => Rep[DeliteMultiArray[B]])(implicit ctx: SourceContext) = dmultia_flatmap(ma,f)
    def groupByReduce[K:Manifest, V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]) = dmultia_groupByReduce(ma,key,value,reduce)
    
    def mmap(f: Rep[T] => Rep[T]): Rep[Unit] = dmultia_mmap(ma,f)
    def mzip(y: Rep[DeliteMultiArray[T]])(f: (Rep[T],Rep[T]) => Rep[T]) = dmultia_mzipwith(ma,y,f)

    // --- 1D ops
    def union(rhs: Rep[DeliteMultiArray[T]]) = dmultia_union(ma,rhs)
    def intersect(rhs: Rep[DeliteMultiArray[T]]) = dmultia_intersect(ma,rhs)
    //def take(n: Rep[Int]) = dmultia_take(ma,n)
    def sort = dmultia_sort(ma)
    def sortWith(comparator: (Rep[T],Rep[T]) => Rep[Int]) = dmultia_sortwith(ma,comparator)
    def toSeq = dmultia_toseq(ma)
  }

  // --- Compiler internal
  def dmultia_element[A:Manifest](ma: Rep[DeliteMultiArray[A]]): Rep[A]

  // --- Array constructors
  def dmultia_new[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_new_immutable[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_fromfunction[A:Manifest](dims: Seq[Rep[Int]], f: Rep[Seq[Rep[Int]]] => Rep[A])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Array properties
  def dmultia_rank[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]
  def dmultia_shape[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]
  def dmultia_size[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]

  // --- Array single element
  def dmultia_apply[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[A]
  def dmultia_update[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Seq[Rep[Int]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array copies / reshaping
  def dmultia_mutable[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_permute[A:Manifest](ma: Rep[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Array parallel ops
  def dmultia_map[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Rep[DeliteMultiArray[A]], mb: Rep[DeliteMultiArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[DeliteMultiArray[R]]
  def dmultia_reduce[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def dmultia_foreach[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  
  // Do these need to be here?
  def dmultia_mmap[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_mzipwith[A:Manifest](ma: Rep[DeliteMultiArray[A]], mb: Rep[DeliteMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[Unit] 

  // not sure if this is strictly 1D only or not...
  def dmultia_flatmap[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[DeliteMultiArray[B]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]

  // These operations are defined only on 1D arrays (will need a check to ensure this)
  def dmultia_filter[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Rep[DeliteMultiArray[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMap[K,V]]

  // (scatter and broadcast? similar to reason for having DeliteArray copy...)

  // --- Misc. 1D Operations
  def dmultia_mkstring[A:Manifest](ma: Rep[DeliteMultiArray[A]], del: Rep[String])(implicit ctx: SourceContext): Rep[String]
  def dmultia_union[A:Manifest](lhs: Rep[DeliteMultiArray[A]], rhs: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_intersect[A:Manifest](lhs: Rep[DeliteMultiArray[A]], rhs: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  def dmultia_sort[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_sortwith[A:Manifest](ma: Rep[DeliteMultiArray[A]], comparator: (Rep[A],Rep[A]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_sortIndices(length: Rep[Int], comparator: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_toseq[A:Manifest](a: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Seq[A]]

  // --- 2D Operations
  def dmultia_matmult[A:Manifest:Numeric](lhs: Rep[DeliteMultiArray[A]], rhs: DeliteMultiArray[A])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
}

trait DeliteMultiArrayOpsExp extends BaseExp with DeliteMultiArrayOps {
  this: DeliteOpsExp =>

  /////////////////////
  // Abstract IR Nodes

  // --- Compiler internal nodes
  // Used since we won't necessarily know how many indices to feed to the apply method
  case class DeliteMultiArrayElement[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,T]

  // --- Array constructors
  case class DeliteMultiArrayNew[T](dims: Seq[Exp[Int]], m:Manifest[T]) extends Def[DeliteMultiArray[T]]
  case class DeliteMultiArrayFromFunction[T](dims: Seq[Exp[Int]], func: Exp[Seq[Exp[Int]]] => Exp[T], m:Manifest[T]) extends Def[DeliteMultiArray[T]] {
    val indices = fresh[Seq[Rep[Int]]]
    lazy val body = reifyEffects(func(indices))
  }

  // --- Array properties
  case class DeliteMultiArrayRank[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Int]
  case class DeliteMultiArrayShape[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Seq[Exp[Int]]]
  case class DeliteMultiArraySize[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Int]

  // --- Array single element ops
  case class DeliteMultiArrayApply[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Seq[Exp[Int]]) extends DefWithManifest[T,T]
  case class DeliteMultiArrayUpdate[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Seq[Exp[Int]], x: Exp[T]) extends DefWithManifest[T,Unit]

  // --- Array reshaping
  case class DeliteMultiArrayPermute[T:Manifest](ma: Exp[DeliteMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext) extends DefWithManifest[T,DeliteMultiArray[T]]

  // --- Array parallel ops
  case class DeliteMultiArrayMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[B])(implicit ctx: SourceContext) extends Def[DeliteMultiArray[B]] {
    val body = reifyEffects(func(dmultia_element(in)))

    val mA = manifest[A]
    val mB = manifest[B]
  }

  case class DeliteMultiArrayZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], func: (Exp[A],Exp[B]) => Exp[R]) extends Def[DeliteMultiArray[R]] {
    val body = reifyEffects(func(dmultia_element(inA), dmultia_element(inB)))

    val mA = manifest[A]
    val mB = manifest[B]
    val mR = manifest[R]
  }
  case class DeliteMultiArrayReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,DeliteMultiArray[A]] {
    val body = reifyEffects(func(dmultia_element(in), dmultia_element(in)))
  }
  case class DeliteMultiArrayForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit] {
    val body = reifyEffects(func(dmultia_element(in)))
  }

  case class DeliteMultiArrayMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) extends Def[DeliteMultiArray[B]] {
    val element = dmultia_element(in)
    val body = reifyEffects(func(element))
    val filt = reifyEffects(cond(element))

    val mA = manifest[A]
    val mB = manifest[B]
  }
  case class DeliteMultiArrayFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) extends Def[DeliteMultiArray[B]] {
    val body = reifyEffects(func(dmultia_element(in)))

    val mA = manifest[A]
    val mR = manifest[R]
  }

  case class DeliteMultiArrayGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) extends Def[DeliteMap[K,V]] {
    val element = dmultia_element(in)
    val keyFunc = reifyEffects(key(element))
    val valueFunc = reifyEffects(value(element))
    val sV = (fresh[V], fresh[V])
    val reducFunc = reifyEffects(reduce(sV._1,sV._2))

    val mA = manifest[A]
    val mK = manifest[K]
    val mV = manifest[V]
  }
  case class DeliteMultiArrayMutableMap[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit] {
    val body = reifyEffects(func(dmultia_element(in)))
  }
  case class DeliteMultiArrayMutableZipWith[A:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit] {
    val body = reifyEffects(func(dmultia_element(inA), dmultia_element(inB)))
  }

  // --- Misc. 1D Operations
  case class DeliteMultiArrayMkString[T:Manifest](ma: Exp[DeliteMultiArray[T]], del: Exp[String]) extends DefWithManifest[T,String]
  case class DeliteMultiArrayUnion[T:Manifest](lhs: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArrayIntersect[T:Manifest](lhs: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T, DeliteMultiArray[T]]
  case class DeliteMultiArraySort[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArraySortWith[T:Manifest](ma: Exp[DeliteMultiArray[T]], comparator: (Exp[T],Exp[T]) => Exp[Int]) extends DefWithManifest[T,DeliteMultiArray[T]] {
    val sV = (dmultia_element(ma), dmultia_element(ma))
    val body = reifyEffects(comparator(sV._1, sV._2))
  }
  case class DeliteMultiArraySortIndices[T:Manifest](length: Exp[Int], comparator: (Exp[Int],Exp[Int]) => Exp[Int]) extends DefWithManifest[T,DeliteMultiArray[T]] {
    val sV = (fresh[Int], fresh[Int])
    val body = reifyEffects(comparator(sV._1, sV._2))
  }
  case class DeliteMultiArrayToSeq[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends Def[Seq[A]]

  // --- 2D Operations
  case class DeliteMatrixMultiply[T:Manifest:Numeric](lhs: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]


  //////////////////////
  // Array IR Functions

  // --- Compiler internal
  def dmultia_element[A:Manifest](ma: Rep[DeliteMultiArray[A]]): Rep[A] = reflectPure(DeliteMultiArrayElement(ma))

  // --- Array constructors
  def dmultia_new[A:Manifest](dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectMutable(DeliteMultiArrayNew(dims,manifest[A]))
  def dmultia_new_immutable[A:Manifest](dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayNew(dims,manifest[A]))
  def dmultia_fromfunction[A:Manifest](dims: Seq[Exp[Int]], f: Exp[Seq[Exp[Int]]] => Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayFromFunction(dims,f))

  // --- Array properties
  def dmultia_rank[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayRank(ma))
  def dmultia_shape[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayShape(ma))
  def dmultia_size[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySize(ma))

  // --- Array single element ops
  def dmultia_apply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayApply(ma,i))
  def dmultia_update[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Seq[Exp[Int]], x: Exp[A])(implicit ctx: SourceContext) = reflectWrite(ma)(DeliteMultiArrayUpdate(ma,i,x))

  // --- Array reshaping / mutability
  def dmultia_mutable[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectMutable(DeliteMultiArrayMap(ma,(e:Rep[T])=>e))
  def dmultia_permute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayPermute(ma,config))

  // --- Array parallel ops
  def dmultia_map[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[B])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMap(ma,f))
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[A]], mb: Exp[DeliteMultiArray[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayZipWith(ma,mb,f))
  def dmultia_reduce[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayReduce(ma,f))
  def dmultia_foreach[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForeach(ma,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }
  def dmultia_mmap[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[A])(implicit ctx: SourceContext) = reflectWrite(ma)(DeliteMultiArrayMutableMap(ma,f))
  def dmultia_mzipwith[A:Manifest](ma: Exp[DeliteMultiArray[A]], mb: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A])(implicit ctx: SourceContext) = reflectWrite(ma)(DeliteMultiArrayMutableZipWith(ma,mb,f))

  def dmultia_flatmap[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayFlatMap(ma,f))
  def dmultia_filter[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = dmultia_mapfilter(x, (e:Exp[A]) => e, f)
  def dmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayGroupByReduce(ma,key,value,reduce))

  def dmultia_mapfilter[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], map: Rep[A] => Rep[B], cond: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMapFilter(ma,map,cond))

  // --- Misc. 1D Operations
  def dmultia_mkstring[A:Manifest](ma: Exp[DeliteMultiArray[A]], del: Exp[String])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMkString(ma,del))
  def dmultia_union[A:Manifest](lhs: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayUnion(lhs,rhs))
  def dmultia_intersect[A:Manifest](lhs: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayIntersect(lhs,rhs))
  def dmultia_sort[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySort(ma))
  def dmultia_sortwith[A:Manifest](ma: Exp[DeliteMultiArray[A]], comparator: (Exp[A],Exp[A]) => Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySortWith(ma, comparator))
  def dmultia_sortIndices(length: Exp[Int], comparator: (Exp[Int],Exp[Int]) => Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySortIndices(length, comparator))
  def dmultia_toseq[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = DeliteMultiArrayToSeq(ma)

  // --- 2D Operations
  def dmultia_matmult[A:Manifest:Numeric](lhs: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMatrixMultiply(lhs,rhs))
}