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

  // Unfortunately needed for certain operations between DeliteMap and DeliteMultiArray
  // Could implement this with dmultia_fromfunction, but could produce an extra copy we may not actually need depending on mem format
  // TODO: do we need dmultia_to_sarray too? Or just need to change DeliteMap?
  def dmultia_from_sarray[A:Manifest](x: Rep[Array[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Array properties
  def dmultia_rank[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]
  def dmultia_shape[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Seq[Rep[Int]]]
  def dmultia_size[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]

  // --- Array single element
  def dmultia_apply[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[A]
  def dmultia_update[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Seq[Rep[Int]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array copies / reshaping
  def dmultia_mutable[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_immutable[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
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

  abstract class DefWithManifest2[A:Manifest,B:Manifest,R:Manifest] extends DefWithManifest[A,R] {
    val mB = manifest[B]
  }
  abstract class DefWithManifest3[A:Manifest,B:Manifest,T:Manifest,R:Manifest] extends DefWithManifest2[A,B,R] {
    val mT = manifest[T]
  }

  /////////////////////
  // Abstract IR Nodes

  // --- Compiler internal nodes
  // TODO: do we need this? would fresh[T] be good enough for reifyEffects on effectful loop bodies?
  case class DeliteMultiArrayElement[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,T]

  // --- Array constructors
  case class DeliteMultiArrayNew[T](dims: Seq[Exp[Int]], m:Manifest[T]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArrayFromFunction[T](dims: Seq[Exp[Int]], func: Exp[Seq[Exp[Int]]] => Exp[T], m:Manifest[T]) extends DefWithManifest[T,DeliteMultiArray[T]]

  // --- Array properties
  case class DeliteMultiArrayRank[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Int]
  case class DeliteMultiArrayShape[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Seq[Exp[Int]]]
  case class DeliteMultiArraySize[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Int]

  // --- Array single element ops
  case class DeliteMultiArrayApply[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Seq[Exp[Int]]) extends DefWithManifest[T,T]
  case class DeliteMultiArrayUpdate[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Seq[Exp[Int]], x: Exp[T]) extends DefWithManifest[T,Unit]

  // --- Array reshaping / permuting
  case class DeliteMultiArrayReshape[T:Manifest](ma: Exp[DeliteMultiArray[T]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArrayPermute[T:Manifest](ma: Exp[DeliteMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext) extends DefWithManifest[T,DeliteMultiArray[T]]

  // --- Array parallel ops
  case class DeliteMultiArrayMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[B])(implicit ctx: SourceContext) extends DefWithManifest2[A,B,DeliteMultiArray[B]]
  case class DeliteMultiArrayZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], func: (Exp[A],Exp[B]) => Exp[R]) extends DefWithManifest3[A,B,R,DeliteMultiArray[R]]
  case class DeliteMultiArrayReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,DeliteMultiArray[A]]
  case class DeliteMultiArrayForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit]
  case class DeliteMultiArrayForIndices[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[Seq[Exp[Int]]] => Exp[R])(implicit ctx: SourceContext) extends DefWithManifest2[A,R,Unit]
  case class DeliteMultiArrayMutableMap[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[A])(implicit ctx: SourceContext) extends DefWithManifest[A,Unit]
  case class DeliteMultiArrayMutableZipWith[A:Manifest,B:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], func: (Exp[A],Exp[A]) => Exp[A])(implicit ctx: SourceContext) extends DefWithManifest2[A,B,Unit]

  case class DeliteMultiArrayNDMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], mdims: Seq[Int], func: Exp[DeliteMultiArray[A]] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) extends DefWithManifest2[A,B,DeliteMultiArray[B]]

  // --- Buffer operations
  case class DeliteMultiArrayNDInsert[T:Manifest](ma: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext) extends DefWithManifest[T,Unit]
  case class DeliteMultiArrayNDAppend[T:Manifest](ma: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]], axis: Int)(implicit ctx: SourceContext) extends DefWithManifest[T,Unit]

  case class DeliteMultiArrayInsertAll[T:Manifest](ma: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext) extends DefWithManifest[T,Unit]
  case class DeliteMultiArrayAppendAll[T:Manifest](ma: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]], axis: Int)(implicit ctx: SourceContext) extends DefWithManifest[T,Unit]

  // --- Misc. Operations
  case class DeliteMultiArrayMkString[T:Manifest](ma: Exp[DeliteMultiArray[T]], dels: Seq[Exp[String]]) extends DefWithManifest[T,String]

  // --- 1D Operations
  case class DeliteMultiArraySortIndices[T:Manifest](length: Exp[Int], comparator: (Exp[Int],Exp[Int]) => Exp[Int]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArrayMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) extends DefWithManifest2[A,B,DeliteMultiArray[B]] {
  case class DeliteMultiArrayFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) extends DefWithManifest2[A,B,DeliteMultiArray[B]]
  case class DeliteMultiArrayGroupBy[A:Manifest,K:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K])(implicit ctx: SourceContext) extends DefWithManifest2[A,K,DeliteMultiMap[K,DeliteMultiArray[A]]]
  case class DeliteMultiArrayGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) extends DefWithManifest3[A,K,V,DeliteMultiMap[K,V]]

  case class DeliteMultiArrayInsert[T:Manifest](ma: Exp[DeliteMultiArray[A]], idx: Rep[Int], x: Exp[T]) extends DefWithManifest[T,Unit]
  case class DeliteMultiArrayAppend[T:Manifest](ma: Exp[DeliteMultiArray[A]], x: Exp[T]) extends DefWithManifest[T,Unit]


  // --- 2D Operations
  case class DeliteMatrixMultiply[T:Manifest:Numeric](lhs: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMatrixVectorMultiply[T:Manifest:Numeric](lhs: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]

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
  def dmultia_mutable[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectMutable(DeliteMultiArrayMap(ma,(e:Rep[A])=>e))
  def dmultia_immutable[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMap(ma,(e:Rep[A])=>e))
  def dmultia_permute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayPermute(ma,config))
  def dmultia_reshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayReshape(ma,dims))

  // --- Array parallel ops
  def dmultia_map[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[B])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMap(ma,f))
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[A]], mb: Exp[DeliteMultiArray[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayZipWith(ma,mb,f))
  def dmultia_reduce[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayReduce(ma,f))
  def dmultia_forindices[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[Seq[Exp[Int]]] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForIndices(ma,f)
    val v = fresh[Seq[Exp[Int]]]
    reflectEffect(df, summarizeEffects(reifyEffects(f(v))).star andAlso Simple())
  }
  def dmultia_foreach[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForeach(ma,f)
    reflectEffect(df, summarizeEffects(reifyEffects(f(dmultia_element(ma)))).star andAlso Simple())
  }
  def dmultia_mmap[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[A])(implicit ctx: SourceContext) = reflectWrite(ma)(DeliteMultiArrayMutableMap(ma,f))
  def dmultia_mzipwith[A:Manifest](ma: Exp[DeliteMultiArray[A]], mb: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A])(implicit ctx: SourceContext) = reflectWrite(ma)(DeliteMultiArrayMutableZipWith(ma,mb,f))

  // --- Misc.
  def dmultia_mkstring[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMkString(ma,dels))

  // --- Misc. 1D Operations
  def dmultia_sort[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySort(ma))
  def dmultia_sortwith[A:Manifest](ma: Exp[DeliteMultiArray[A]], comparator: (Exp[A],Exp[A]) => Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySortWith(ma, comparator))
  def dmultia_sortIndices(length: Exp[Int], comparator: (Exp[Int],Exp[Int]) => Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySortIndices(length, comparator))
  
  def dmultia_flatmap[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayFlatMap(ma,f))
  def dmultia_filter[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = dmultia_mapfilter(x, (e:Exp[A]) => e, f)
  def dmultia_groupBy[A:Manifest,K:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayGroupBy(ma,key))
  def dmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayGroupByReduce(ma,key,value,reduce))

  def dmultia_mapfilter[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], map: Rep[A] => Rep[B], cond: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMapFilter(ma,map,cond))


  // --- 2D Operations
  def dmultia_matmult[A:Manifest:Numeric](lhs: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMatrixMultiply(lhs,rhs))

  ///////////////////////
  // aliases and sharing

  // return x if e may be equal to x
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case _ => super.aliasSyms(e)
  }

  // return x if later apply to e may return x
  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case _ => super.containSyms(e)
  }

  // return x if dereferencing x may return e?
  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case _ => super.extractSyms(e)
  }

  // return x if (part?) of x (may?) have been copied into e?
  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case _ => super.copySyms(e)
  }    

  // symbols which are bound in a definition
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case _ => super.boundSyms(e)
  }
}