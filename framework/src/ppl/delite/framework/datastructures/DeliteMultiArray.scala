package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

import ppl.delite.framework.analysis.DeliteMetadata
import ppl.delite.framework.ops._

// Abstract, n-dimensional, multi-purpose array
// Intended for use at frontend w/ transformer to concrete data layouts
trait DeliteMultiArray[T]
trait DeliteArray1D[T] extends DeliteMultiArray[T]
trait DeliteArray2D[T] extends DeliteMultiArray[T]
trait DeliteArray3D[T] extends DeliteMultiArray[T]
trait DeliteArray4D[T] extends DeliteMultiArray[T]
trait DeliteArray5D[T] extends DeliteMultiArray[T]

// TODO: Really not sure about this abstraction yet, except for type signature [T,R]
// Seq[Seq[Int]] intended to represent various dimension layering and order... potentially could work

/*trait DeliteMultiArrayLayouts extends DeliteMetadata {

  abstract class Layout[T:Manifest,R:Manifest] extends Metadata {
    val dims: Seq[Seq[Int]]
    val mT = manifest[T]  // saving these for now, may need them later
    val mR = manifest[R]
  }

  // Assume in order for now - can potentially make other special case classes later
  case class FlatLayout[T:Manifest](rank: Int) extends Layout[T,T] { 
    override val dims = Seq(Seq.tabulate(rank){i=>i+1})
  }
  case class SinglyNested[T:Manifest](rank: Int, inner: Int) extends Layout[T,DeliteArray[T]] { 
    override val dims = Seq(Seq.tabulate(inner-1){i=>i+1},Seq.tabulate(rank-inner+1){i=>i+inner}) 
  }
}*/

// TODO: Modify this.. maybe want it to be a DeliteStruct? 
// Is there ever a case where we need this to be codegenned?
trait Indices

trait IndicesOps extends Base {
  object Indices { def apply(xs: Rep[Int]*) = indices_new(xs.toList) }

  implicit def repIndicesToIndicesOpsCls(x: Rep[Indices]) = new IndicesOpsCls(x)
  class IndicesOpsCls(x: Rep[Indices]) { def apply(i: Int) = indices_apply(x, i) }

  def indices_new(xs: Seq[Rep[Int]]): Rep[Indices]
  def indices_apply(s: Rep[Indices], i: Int): Rep[Int]
}

trait IndicesOpsExp extends IndicesOps with BaseExp {
  case class IndicesNew(xs: List[Exp[Int]]) extends DeliteStruct[Indices] {
    val elems = copyTransformedElems(xs.zipWithIndex.map{i => ("i" + i._1) -> i._2})
  }
  def indices_new(x: Seq[Exp[Int]]) = IndicesNew(x.toList)
  def indices_apply(x: Exp[Indices], i: Int) = field[Int](x, "i" + i)
}

trait DeliteMultiArrayOps extends Base with IndicesOps {
  
  // TODO: This general form for N indices will create a whole 
  // bunch of cases that look like:
  // val x: Rep[Seq[Int]] = Seq(orig_d0, orig_d1)
  // template_func(x):
  //    val d0 = x(0)
  //    val d1 = x(1)
  //    actual_func(d0, d1)

  // Need to transform this to just func(d0, d1)
  // Perhaps can be done with struct-like optimizer?
  /* 
  def seq_apply(x: Rep[Seq[T]], i: Exp[Int]) = (x,i) match {
    case (SeqNew(l), Const(i)) => l(i)
    case _ => SeqApply(x, i) 
  }  
  */

  // --- Rank type casts
  def dmultia_as_1D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray1D[T]]
  def dmultia_as_2D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray2D[T]]
  def dmultia_as_3D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray3D[T]]
  def dmultia_as_4D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray4D[T]]
  def dmultia_as_5D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray5D[T]]

  object DeliteMultiArray {
    def apply[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = dmultia_new[T](dims)
    def imm[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = dmultia_new_immutable[T](dims)
    def fromFunction[T:Manifest](dims: Rep[Int]*)(func: Rep[Indices] => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(dims, func)
  }

  implicit def repMultiAtoMultiAOps[T:Manifest](ma: Rep[DeliteMultiArray[T]])(implicit ctx: SourceContext) = new DeliteFloatingLoopsCls(ma)
  class DeliteFloatingLoopsCls[T:Manifest](ma: Rep[DeliteMultiArray[T]])(implicit ctx: SourceContext) {
    // --- rank casts
    def as1D: Rep[DeliteArray1D[T]] = dmultia_as_1D(ma)
    def as2D: Rep[DeliteArray2D[T]] = dmultia_as_2D(ma)
    def as3D: Rep[DeliteArray3D[T]] = dmultia_as_3D(ma)
    def as4D: Rep[DeliteArray4D[T]] = dmultia_as_4D(ma)
    def as5D: Rep[DeliteArray5D[T]] = dmultia_as_5D(ma)

    // --- properties
    def rank: Rep[Int] = dmultia_rank(ma)
    def shape: Rep[Indices] = dmultia_shape(ma)
    def size: Rep[Int] = dmultia_size(ma)
    def dim(i: Int) = dmultia_shape(ma).apply(i)
    
    // --- single element
    def apply(i: Rep[Int]*): Rep[T] = dmultia_apply(ma,indices_new(i.toList))
    def update(i: Seq[Rep[Int]], x: Rep[T]): Rep[Unit] = dmultia_update(ma,indices_new(i),x)

    def apply(i: Rep[Indices]): Rep[T] = dmultia_apply(ma,i)
    def update(i: Rep[Indices], x: Rep[T]): Rep[Unit] = dmultia_update(ma,i,x)
    
    // --- parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteMultiArray[B]] = dmultia_map(ma,f)
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteMultiArray[R]] = dmultia_zipwith(ma,y,f)
    def reduce(zero: Rep[T])(f: (Rep[T],Rep[T]) => Rep[T]): Rep[T] = dmultia_reduce(ma,f,zero)
    def foreach(f: Rep[T] => Rep[Unit]): Rep[Unit] = dmultia_foreach(ma,f)
    def forIndices(f: Rep[Indices] => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma.shape,f)
    def groupBy[K:Manifest](key: Rep[T] => Rep[K]) = dmultia_groupBy(ma,key)
    def groupByReduce[K:Manifest, V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]) = dmultia_groupByReduce(ma,key,value,reduce)
    
    def mmap(f: Rep[T] => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i))}
    def mzip[B:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i),y(i))}
  
    def mkString(dels: Rep[String]*) = dmultia_mkstring(ma,dels)
  }

  object DeliteArray1D {
    def apply[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext) = dmultia_new[T](List(len)).as1D
    def imm[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext) = dmultia_new_immutable[T](List(len)).as1D
    def fromFunction[T:Manifest](len: Rep[Int])(func: Rep[Int] => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(List(len), {x: Rep[Indices] => func(x(0))}).as1D
  
    def sortIndices(length: Rep[Int])(comparator: (Rep[Int], Rep[Int]) => Rep[Int])(implicit ctx: SourceContext) = dmultia_sortIndices(length, comparator)
  }

  implicit def repArray1DtoArray1DOps[T:Manifest](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) = new DeliteArray1DOpsCls(ma)
  class DeliteArray1DOpsCls[T:Manifest](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def length: Rep[Int] = dmultia_size(ma)

    // --- mutability / buffering
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = dmultia_update(ma,Indices(i),x)
    def insert(i: Rep[Int], x: Rep[T]): Rep[Unit] = dmultia_insert(ma,x,i)
    def append(x: Rep[T]): Rep[Unit] = dmultia_append(ma,x)
    def remove(start: Rep[Int], len: Rep[Int]): Rep[Unit] = dmultia_remove(ma, 0, start, len)
    def remove(i: Rep[Int]): Rep[Unit] = this.remove(i, unit(1))
  
    def insertAll(i: Rep[Int], rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_insertAll(ma, rhs, 0, i)
    def appendAll(rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 0)
    def ++=(rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 0)

    // --- 1D parallel ops
    def forIndices(f: Rep[Int] => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma.shape, {i: Rep[Indices] => f(i(0))})
    def filter(f: Rep[T] => Rep[Boolean]): Rep[DeliteArray1D[T]] = dmultia_filter(ma,f)
    def flatMap[B:Manifest](f: Rep[T] => Rep[DeliteArray1D[B]])(implicit ctx: SourceContext) = dmultia_flatmap(ma,f)  
  }

  object DeliteArray2D {
    def apply[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext) = dmultia_new[T](List(rows,cols)).as2D
    def imm[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext) = dmultia_new_immutable[T](List(rows,cols)).as2D
    def fromFunction[T:Manifest](rows: Rep[Int], cols: Rep[Int])(func: (Rep[Int], Rep[Int]) => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(List(rows,cols), {x: Rep[Indices] => func(x(0),x(1))}).as2D
  }

  implicit def repArray2DToArray2DOps[T:Manifest](ma: Rep[DeliteArray2D[T]])(implicit ctx: SourceContext) = new DeliteArray2DOpsCls(ma)
  class DeliteArray2DOpsCls[T:Manifest](ma: Rep[DeliteArray2D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def rows = dmultia_shape(ma).apply(0)
    def cols = dmultia_shape(ma).apply(1)

    // --- mutability/buffering
    def update(i: Rep[Int], j: Rep[Int], x: Rep[T]): Rep[Unit] = dmultia_update(ma,Indices(i,j),x)

    def insertRow(i: Rep[Int], rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_insertAll(ma, rhs, 0, i)
    def insertCol(j: Rep[Int], rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_insertAll(ma, rhs, 1, j)
    def appendRow(rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 0)
    def appendCol(rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 1)

    def insertRows(i: Rep[Int], rhs: Rep[DeliteArray2D[T]]): Rep[Unit] = dmultia_insertAll(ma, rhs, 0, i)
    def insertCols(j: Rep[Int], rhs: Rep[DeliteArray2D[T]]): Rep[Unit] = dmultia_insertAll(ma, rhs, 1, j)
    def appendRows(rhs: Rep[DeliteArray2D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 0)
    def appendCols(rhs: Rep[DeliteArray2D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 1)

    def removeRows(start: Rep[Int], len: Rep[Int]) = dmultia_remove(ma, 0, start, len)
    def removeCols(start: Rep[Int], len: Rep[Int]) = dmultia_remove(ma, 1, start, len)
    def removeRow(i: Rep[Int]) = this.removeRows(i, unit(1))
    def removeCol(j: Rep[Int]) = this.removeCols(j, unit(1))

    // --- 2D parallel ops
    def forIndices(f: (Rep[Int], Rep[Int]) => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma.shape, {i: Rep[Indices] => f(i(0), i(1))})
    def mapRows[R:Manifest](f: Rep[DeliteArray1D[T]] => Rep[DeliteArray1D[R]]) = dmultia_NDmap(ma,List(0),{r: Rep[DeliteMultiArray[T]] => f(r.as1D) }).as2D
    def mapCols[R:Manifest](f: Rep[DeliteArray1D[T]] => Rep[DeliteArray1D[R]]) = dmultia_NDmap(ma,List(1),{c: Rep[DeliteMultiArray[T]] => f(c.as1D) }).as2D
  }

  // --- Array constructors
  def dmultia_new[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_new_immutable[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_view[A:Manifest](ma: Rep[DeliteMultiArray[A]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Array properties
  def dmultia_rank[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]
  def dmultia_shape[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Indices]
  def dmultia_size[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]

  // --- Array single element
  def dmultia_apply[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Rep[Indices])(implicit ctx: SourceContext): Rep[A]
  def dmultia_update[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Rep[Indices], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array copies / reshaping
  def dmultia_mutable[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_immutable[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_permute[A:Manifest](ma: Rep[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_reshape[A:Manifest](ma: Rep[DeliteMultiArray[A]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Parallel Ops
  def dmultia_fromfunction[A:Manifest](dims: Seq[Rep[Int]], f: Rep[Indices] => Rep[A])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_map[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Rep[DeliteMultiArray[A]], mb: Rep[DeliteMultiArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[DeliteMultiArray[R]]
  def dmultia_reduce[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def dmultia_forindices[A:Manifest](dims: Exp[Indices], f: Rep[Indices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_foreach[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_NDmap[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], mdims: Seq[Int], func: Rep[DeliteMultiArray[A]] => Rep[DeliteMultiArray[B]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]

  def dmultia_groupBy[A:Manifest,K:Manifest](ma: Rep[DeliteMultiArray[A]], key: Rep[A] => Rep[K])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,DeliteArray1D[A]]]
  def dmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Rep[DeliteMultiArray[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteHashMap[K,V]]

  // --- 1D Parallel Ops
  def dmultia_flatmap[A:Manifest,B:Manifest](ma: Rep[DeliteArray1D[A]], f: Rep[A] => Rep[DeliteArray1D[B]])(implicit ctx: SourceContext): Rep[DeliteArray1D[B]]
  def dmultia_filter[A:Manifest](ma: Rep[DeliteArray1D[A]], f: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]
  
  // --- Buffer ops
  def dmultia_insert[A:Manifest](ma: Rep[DeliteArray1D[A]], x: Rep[A], index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_append[A:Manifest](ma: Rep[DeliteArray1D[A]], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_insertAll[A:Manifest](ma: Rep[DeliteMultiArray[A]], rhs: Rep[DeliteMultiArray[A]], axis: Int, index: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_appendAll[A:Manifest](ma: Rep[DeliteMultiArray[A]], rhs: Rep[DeliteMultiArray[A]], axis: Int)(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_remove[A:Manifest](ma: Rep[DeliteMultiArray[A]], axis: Int, start: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]  

  // --- 1D Ops
  def dmultia_sortIndices(len: Rep[Int], comp: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[Int]]
  def dmultia_mkstring[A:Manifest](ma: Rep[DeliteMultiArray[A]], dels: Seq[Rep[String]])(implicit ctx: SourceContext): Rep[String]
  def dmultia_string_split(str: Rep[String],pat: Rep[String],ofs: Rep[Int] = unit(0))(implicit __imp0: SourceContext): Rep[DeliteArray1D[String]]

  // --- 2D Ops
  def dmultia_matmult[A:Manifest:Numeric](lhs: Rep[DeliteArray2D[A]], rhs: Rep[DeliteArray2D[A]])(implicit ctx: SourceContext): Rep[DeliteArray2D[A]]
  def dmultia_matvecmult[A:Manifest:Numeric](mat: Rep[DeliteArray2D[A]], vec: Rep[DeliteArray1D[A]])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]

  // --- Pinning
  //def dmultia_pin[T:Manifest,R:Manifest](ma: Rep[DeliteMultiArray[T]], layout: Layout[T,R])(implicit ctx: SourceContext): Rep[DeliteArray[R]]
  //def dmultia_unpin[T:Manifest,R:Manifest](in: Rep[DeliteArray[R]], layout: Layout[T,R], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[T]]
}

trait DeliteMultiArrayOpsExp extends DeliteFloatingLoops with AtomicWriteOpsExp with EffectExp with IndicesOpsExp {
  this: DeliteOpsExp with DeliteArrayOpsExp /*with DeliteHashMapOpsExp*/ =>

  /** 
   * Base class for multi-loops that require transformation before codegen
   * Does not extend AbstractLoop...
   */
  abstract class FloatingLoop[A:Manifest,R:Manifest] extends DeliteOp[R] { 
    lazy val i: Sym[Indices] = copyTransformedOrElse(_.i)(fresh[Indices]).asInstanceOf[Sym[Indices]]
    val mA = manifest[A] 
    val mR = manifest[R]
  }
  abstract class FloatingLoop2[A:Manifest,B:Manifest,R:Manifest] extends FloatingLoop[A,R] { val mB = manifest[B] }
  abstract class FloatingLoop3[A:Manifest,B:Manifest,T:Manifest,R:Manifest] extends FloatingLoop2[A,B,R] { val mT = manifest[T] }

  /////////////////////
  // Abstract IR Nodes

  // --- Array constructors
  case class DeliteMultiArrayNew[T:Manifest](dims: Seq[Exp[Int]]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArrayView[T:Manifest](ma: Exp[DeliteMultiArray[T]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) extends DefWithManifest[T,DeliteMultiArray[T]]

  // --- Array properties
  case class DeliteMultiArrayRank[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Int]
  case class DeliteMultiArrayShape[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Indices]
  case class DeliteMultiArraySize[T:Manifest](ma: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Int]

  //case class DeliteMultiArrayViewTarget[T:Manifest](v: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArrayViewStart[T:Manifest](v: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Indices]
  case class DeliteMultiArrayViewStride[T:Manifest](v: Exp[DeliteMultiArray[T]]) extends DefWithManifest[T,Indices]

  // --- Array single element ops
  case class DeliteMultiArrayApply[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Exp[Indices]) extends DefWithManifest[T,T]
  case class DeliteMultiArrayUpdate[T:Manifest](ma: Exp[DeliteMultiArray[T]], i: Exp[Indices], x: Exp[T])(implicit ctx: SourceContext) extends AtomicWrite {
    def externalFields = List(i, x)
  }

  // --- Array permute / reshaping
  case class DeliteMultiArrayPermute[T:Manifest](ma: Exp[DeliteMultiArray[T]], config: Seq[Int])(implicit ctx: SourceContext) extends DefWithManifest[T,DeliteMultiArray[T]]
  case class DeliteMultiArrayReshape[T:Manifest](ma: Exp[DeliteMultiArray[T]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) extends DefWithManifest[T,DeliteMultiArray[T]]

  // --- Parallel Ops
  /** 
   * Parallel creation of a multidimensional array
   * @param dims - a list of dimensions for the array (length is equal to array rank)
   * @param func - function from multidimensional index (e.g. (d1, d2, ... dn)) to element
   */
  case class DeliteMultiArrayFromFunction[T:Manifest](dims: Seq[Exp[Int]], func: Exp[Indices] => Exp[T]) extends FloatingLoop[T,DeliteMultiArray[T]] {
    type OpType <: DeliteMultiArrayFromFunction[T]
    lazy val body: Block[T] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(i)))
  }

  /** 
   * Parallel map over all elements in multidimensional array
   * @param in   - input multi-array
   * @param func - lambda mapping function 
   */
  case class DeliteMultiArrayMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[R])(implicit ctx: SourceContext) extends FloatingLoop2[A,R,DeliteMultiArray[R]] {
    type OpType <: DeliteMultiArrayMap[A,R]
    lazy val fin: Exp[A] = copyTransformedOrElse(_.fin)(dmultia_apply(in,i))
    lazy val body: Block[R] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(fin)))
  }

  /**
   * Parallel zip over two multidimensional-arrays
   * Multidimensional arrays must be of the same rank, and are assumed to be of the same dimension(s)
   * @param inA  - first input multi-array
   * @param inB  - second input multi-array
   * @param func - zipWith function
   */
  case class DeliteMultiArrayZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], func: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) extends FloatingLoop3[A,B,R,DeliteMultiArray[R]] {
    type OpType <: DeliteMultiArrayZipWith[A,B,R]
    lazy val fin: (Exp[A],Exp[B]) = (copyTransformedOrElse(_.fin._1)(dmultia_apply(inA,i)), copyTransformedOrElse(_.fin._2)(dmultia_apply(inB,i)))
    lazy val body: Block[R] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(fin._1, fin._2)))
  }

  /**
   * Parallel reduction of a multidimensional array
   * @param in   - input multi-array
   * @param func - reduction function (must be associative)
   * @param zero - "empty" value
   */
  case class DeliteMultiArrayReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) extends FloatingLoop[A,A] {
    type OpType <: DeliteMultiArrayReduce[A]
    lazy val i2: Sym[Indices] = copyTransformedOrElse(_.i2)(fresh[Indices]).asInstanceOf[Sym[Indices]]
    lazy val fin: (Exp[A],Exp[A]) = (copyTransformedOrElse(_.fin._1)(dmultia_apply(in,i)), copyTransformedOrElse(_.fin._2)(dmultia_apply(in,i2)))
    lazy val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(fin._1, fin._2)))
  }
  
  /** 
   * Parallel foreach over elements of a multidimensional array
   * @param in   - input multi-array
   * @param func - foreach function
   */ 
  case class DeliteMultiArrayForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) extends FloatingLoop[A,Unit] {
    type OpType <: DeliteMultiArrayForeach[A]
    lazy val fin: Exp[A] = copyTransformedOrElse(_.fin)(dmultia_apply(in,i))
    lazy val body: Block[Unit] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(fin)))
  }

  /** 
   * Parallel loop over indices in multidimensional array's dimension(s)
   * @param dims - set of dimensions
   * @param func - function on set of indices
   */ 
  case class DeliteMultiArrayForIndices[A:Manifest](dims: Exp[Indices], func: Exp[Indices] => Exp[Unit])(implicit ctx: SourceContext) extends FloatingLoop[A,Unit] {
    type OpType <: DeliteMultiArrayForIndices[A]
    lazy val body: Block[Unit] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(i)))
  }

  /**
   * Parallel N-Dimensional Flat Map on multi-dimensional array
   * @param in    - input multi-array
   * @param mdims - map dimensions 
   * @param func  - flat map function
   */
  case class DeliteMultiArrayNDMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], mdims: Seq[Int], func: Exp[DeliteMultiArray[A]] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) extends FloatingLoop2[A,B,DeliteMultiArray[B]] {
    type OpType <: DeliteMultiArrayNDMap[A,B]
    lazy val fin: Sym[DeliteMultiArray[A]] = copyTransformedOrElse(_.fin)(fresh[DeliteMultiArray[A]]).asInstanceOf[Sym[DeliteMultiArray[A]]]
    lazy val body: Block[DeliteMultiArray[B]] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(fin)))
  }

  /**
   * Parallel Group-By on multi-dimensional array
   * @param in  - input multi-array 
   * @param key - key (hash) function
   */
  case class DeliteMultiArrayGroupBy[A:Manifest,K:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K])(implicit ctx: SourceContext) extends FloatingLoop2[A,K,DeliteHashMap[K,DeliteArray1D[A]]] {
    type OpType <: DeliteMultiArrayGroupBy[A,K]
    lazy val fin: Exp[A] = copyTransformedOrElse(_.fin)(dmultia_apply(in,i))
    lazy val keyFunc: Block[K] = copyTransformedBlockOrElse(_.keyFunc)(reifyEffects(key(fin)))
  }

  /**
   * Parallel Group-By-Reduce on multidimensional array
   * @param in     - input multi-array
   * @param key    - key (hash) function
   * @param value  - mapping function from elements of in to HashMap values
   * @param reduce - reduction on values (with same key)
   */
  case class DeliteMultiArrayGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) extends FloatingLoop3[A,K,V,DeliteHashMap[K,V]] {
    type OpType <: DeliteMultiArrayGroupByReduce[A,K,V]
    lazy val v: (Sym[V],Sym[V]) = copyOrElse(_.v)((fresh[V],fresh[V]))
    lazy val fin: Exp[A] = copyTransformedOrElse(_.fin)(dmultia_apply(in,i))
    lazy val keyFunc: Block[K] = copyTransformedBlockOrElse(_.keyFunc)(reifyEffects(key(fin)))
    lazy val valFunc: Block[V] = copyTransformedBlockOrElse(_.valFunc)(reifyEffects(value(fin)))
    lazy val redFunc: Block[V] = copyTransformedBlockOrElse(_.redFunc)(reifyEffects(reduce(v._1,v._2)))
  }

  // --- 1D Parallel Ops
  // TODO: The MultiArray ops here may be able to be changed to DeliteArray ops using pins later?
  /** 
   * Parallel Map-Filter on a 1D array
   * @param in   - input array
   * @param func - mapping function
   * @param cond - filter function
   */
  case class DeliteMultiArrayMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) extends FloatingLoop2[A,B,DeliteArray1D[B]] {
    type OpType <: DeliteMultiArrayMapFilter[A,B]
    lazy val fin: Sym[A] = copyTransformedOrElse(_.fin)(dmultia_apply(in,i))
    lazy val mapFunc: Block[B] = copyTransformedBlockOrElse(_.mapFunc)(reifyEffects(func(fin)))
    lazy val filtFunc: Block[Boolean] = copyTransformedBlockOrElse(_.filtFunc)(reifyEffects(cond(fin)))
  }

  /**
   * Parallel Flat Map on a 1D array
   * @param in   - input array
   * @param func - flat map function
   */
  case class DeliteMultiArrayFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], func: Exp[A] => Exp[DeliteArray1D[B]])(implicit ctx: SourceContext) extends FloatingLoop2[A,B,DeliteArray1D[B]] {
    type OpType <: DeliteMultiArrayFlatMap[A,B]
    lazy val fin: Sym[A] = copyTransformedOrElse(_.fin)(dmultia_apply(in,i))
    lazy val body: Block[DeliteArray1D[B]] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(fin)))
  }

  // --- Buffer operations
  case class DeliteMultiArrayInsert[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A], index: Exp[Int])(implicit ctx: SourceContext) extends AtomicWrite {
    def externalFields = List(index, x)
  }
  case class DeliteMultiArrayInsertAll[T:Manifest](ma: Exp[DeliteMultiArray[T]], rhs: Exp[DeliteMultiArray[T]], axis: Int, index: Exp[Int])(implicit ctx: SourceContext) extends AtomicWrite {
    def externalFields = List(rhs, axis, index)
  }
  case class DeliteMultiArrayRemove[T:Manifest](ma: Exp[DeliteMultiArray[T]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) extends AtomicWrite {
    def externalFields = List(axis, start, len)
  }

  // --- 1D Ops
  /**
   * General sort function using Java comparator interface
   * From Java doc: Comparator "returns a negative integer, zero, or a positive integer as the 
   * first argument is less than, equal to, or greater than the second."
   * @param len        - length of array to be created
   * @param comparator - comparator function on indices
   */
  case class DeliteMultiArraySortIndices(len: Exp[Int], comparator: (Exp[Int],Exp[Int]) => Exp[Int]) extends Def[DeliteArray1D[Int]] {
    lazy val i: (Sym[Int],Sym[Int]) = copyOrElse(_.i)((fresh[Int],fresh[Int]))
    lazy val body: Block[Int] = copyTransformedBlockOrElse(_.body)(reifyEffects(comparator(i._1,i._2)))
  }
  case class DeliteMultiArrayMkString[T:Manifest](ma: Exp[DeliteMultiArray[T]], dels: Seq[Exp[String]]) extends DefWithManifest[T,String]
  case class DeliteStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int]) extends DefWithManifest[String,DeliteArray1D[String]]

  // --- 2D Ops
  case class DeliteMatrixMultiply[A:Manifest:Numeric](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]])(implicit ctx: SourceContext) extends DefWithManifest[A,DeliteArray2D[A]] {
    val nA = implicitly[Numeric[A]]
  }
  case class DeliteMatrixVectorMultiply[A:Manifest:Numeric](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext) extends DefWithManifest[A,DeliteArray1D[A]] {
    val nA = implicitly[Numeric[A]]
  }

  // --- Atomic Write (MultiArray)
  case class MultiArrayAtomicWrite(ma: Exp[DeliteMultiArray[Any]], i: Exp[Indices], override val d: AtomicWrite) extends NestedAtomicWrite(d) {
    def externalFields = List(i, d)
  }

  override def recurseLookup[T:Manifest](target: Exp[Any], d: AtomicWrite): (Exp[Any], AtomicWrite) = target match {
    case Def(DeliteMultiArrayApply(ma,i)) => recurseLookup(ma, MultiArrayAtomicWrite(ma, i, d.asNested))
    case Def(Reflect(DeliteMultiArrayApply(ma,i))) => recurseLookup(ma, MultiArrayAtomicWrite(ma, i, d.asNested))
    case _ => super.recurseWrite(target, d)
  }

  // --- Data type pinning
  // Still TBD - are these IR nodes or just metadata functions?
  //case class DeliteMultiArrayPin[T:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[T]], layout: Layout[T,R]) extends DefWithManifest2[T,R,DeliteArray[R]]
  //case class DeliteMultiArrayUnpin[T:Manifest,R:Manifest](in: Exp[DeliteArray[R]], layout: Layout[T,R], shape: Seq[Exp[Int]]) extends DefWithManifest[T,R,DeliteMultiArray[T]]

  //////////////////////
  // Array IR Functions

  // --- Rank type casts
  // Note: Unsafe! Rank analysis will probably catch incorrect casts though..
  def dmultia_as_1D[T:Manifest](ma: Exp[DeliteMultiArray[T]]) = ma.asInstanceOf[Exp[DeliteArray1D[T]]]
  def dmultia_as_2D[T:Manifest](ma: Exp[DeliteMultiArray[T]]) = ma.asInstanceOf[Exp[DeliteArray2D[T]]]
  def dmultia_as_3D[T:Manifest](ma: Exp[DeliteMultiArray[T]]) = ma.asInstanceOf[Exp[DeliteArray3D[T]]]
  def dmultia_as_4D[T:Manifest](ma: Exp[DeliteMultiArray[T]]) = ma.asInstanceOf[Exp[DeliteArray4D[T]]]
  def dmultia_as_5D[T:Manifest](ma: Exp[DeliteMultiArray[T]]) = ma.asInstanceOf[Exp[DeliteArray5D[T]]]

  // --- Array constructors
  def dmultia_new[A:Manifest](dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectMutable(DeliteMultiArrayNew[A](dims))
  def dmultia_new_immutable[A:Manifest](dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayNew[A](dims))
  def dmultia_view[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayView(ma,start,stride,dims))

  // --- Array properties
  def dmultia_rank[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayRank(ma))
  def dmultia_shape[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayShape(ma))
  def dmultia_size[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySize(ma))

  //def dmultia_view_target[A:Manifest](ma: Exp[DeliteMultiArray[A]]) = reflectPure(DeliteMultiArrayViewTarget(ma))
  def dmultia_view_start[A:Manifest](ma: Exp[DeliteMultiArray[A]]) = reflectPure(DeliteMultiArrayViewStart(ma))
  def dmultia_view_stride[A:Manifest](ma: Exp[DeliteMultiArray[A]]) = reflectPure(DeliteMultiArrayViewStride(ma))

  // --- Array single element ops
  def dmultia_apply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[Indices])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayApply(ma,i))
  def dmultia_update[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[Indices], x: Exp[A])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayUpdate(ma,i,x))

  // --- Array permute / reshaping
  def dmultia_permute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayPermute(ma,config))
  def dmultia_reshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayReshape(ma,dims))
  
  // --- Array copying
  // clone doubles as view's toDense operation
  // TODO: do we need a symbol hint for this?
  def dmultia_clone[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMap(ma,{(e: Exp[A])=>e}))
  def dmultia_mutable[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectMutable(DeliteMultiArrayMap(ma,{(e:Exp[A])=>e}))
  def dmultia_immutable[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMap(ma,{(e:Exp[A])=>e}))
  
  // --- Array parallel ops
  def dmultia_fromfunction[A:Manifest](dims: Seq[Exp[Int]], f: Exp[Indices] => Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayFromFunction(dims,f))
  def dmultia_map[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[B])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMap(ma,f))
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[A]], mb: Exp[DeliteMultiArray[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayZipWith(ma,mb,f))
  def dmultia_reduce[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayReduce(ma,f,zero))
  def dmultia_forindices[A:Manifest](dims: Exp[Indices], f: Exp[Indices] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForIndices(dims,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }
  def dmultia_foreach[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForeach(ma,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }
  def dmultia_NDmap[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], mdims: Seq[Int], func: Exp[DeliteMultiArray[A]] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayNDMap(ma,mdims,func))
  def dmultia_groupBy[A:Manifest,K:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayGroupBy(ma,key))
  def dmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayGroupByReduce(ma,key,value,reduce))
  
  // --- 1D Parallel Ops
  def dmultia_flatmap[A:Manifest,B:Manifest](ma: Exp[DeliteArray1D[A]], f: Exp[A] => Exp[DeliteArray1D[B]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayFlatMap(ma,f))
  def dmultia_filter[A:Manifest](ma: Exp[DeliteArray1D[A]], f: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = dmultia_mapfilter(ma, {(e:Exp[A]) => e}, f)
  def dmultia_mapfilter[A:Manifest,B:Manifest](ma: Exp[DeliteArray1D[A]], map: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMapFilter(ma,map,cond))
  
  // --- Buffer ops
  def dmultia_insert[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A], index: Exp[Int])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayInsert(ma,x,index))
  def dmultia_insertAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]], axis: Int, index: Exp[Int])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayInsertAll(ma,rhs,axis,index))
  def dmultia_remove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayRemove(ma,axis,start,len))

  def dmultia_append[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A])(implicit ctx: SourceContext) = dmultia_insert(ma, x, ma.length)
  def dmultia_appendAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]], axis: Int)(implicit ctx: SourceContext) = dmultia_insertAll(ma, rhs, axis, ma.dim(axis))

  // --- 1D Operations
  def dmultia_sortIndices(len: Exp[Int], comp: (Exp[Int],Exp[Int]) => Exp[Int])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArraySortIndices(len, comp))
  def dmultia_mkstring[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayMkString(ma,dels))
  def dmultia_string_split(str: Exp[String], pat: Exp[String], ofs: Exp[Int] = unit(0))(implicit ctx: SourceContext) = reflectPure(DeliteStringSplit(str,pat,ofs))
  
  // --- 2D Operations
  def dmultia_matmult[A:Manifest:Numeric](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMatrixMultiply(lhs,rhs))
  def dmultia_matvecmult[A:Manifest:Numeric](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext) = reflectPure(DeliteMatrixVectorMultiply(mat,vec))

  // --- Pinning
  // TBD
  //def dmultia_pin[T:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[T]], layout: Layout[T,R])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayPin[T,R](ma,layout))
  //def dmultia_unpin[T:Manifest,R:Manifest](in: Exp[DeliteArray[R]], layout: Layout[T,R], shape: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayUnpin[T,R](ma,layout,shape))

  ////////////////
  // dependencies

  // TODO: Some duplication with DeliteOps... can we reduce this somehow?
  // TODO: Is this all correct? Not sure how to write these...

  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => blocks(op.body)
    case op: DeliteMultiArrayMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayZipWith[_,_,_] => blocks(op.body)
    case op: DeliteMultiArrayReduce[_] => blocks(op.body)
    case op: DeliteMultiArrayForeach[_] => blocks(op.body)
    case op: DeliteMultiArrayForIndices[_] => blocks(op.body)
    case op: DeliteMultiArrayNDMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayGroupBy[_,_] => blocks(op.keyFunc)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.redFunc)
    case op: DeliteMultiArrayMapFilter[_,_] => blocks(op.mapFunc) ::: blocks(op.filtFunc)
    case op: DeliteMultiArrayFlatMap[_,_] => blocks(op.body)
    case op: DeliteMultiArraySortIndices => blocks(op.body)
    case _ => super.blocks(e)
  }

  // regular data dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => syms(op.body) ::: syms(op.dims)
    case op: DeliteMultiArrayMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => syms(op.body) ::: syms(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => syms(op.body) ::: syms(List(op.in, op.zero))
    case op: DeliteMultiArrayForeach[_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayForIndices[_] => syms(op.body) ::: syms(op.dims)
    case op: DeliteMultiArrayNDMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayGroupBy[_,_] => syms(op.keyFunc) ::: syms(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => syms(op.keyFunc) ::: syms(op.valFunc) ::: syms(op.redFunc) ::: syms(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => syms(op.mapFunc) ::: syms(op.filtFunc) ::: syms(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArraySortIndices => syms(op.body) ::: syms(op.length)
    case _ => super.syms(e)
  }

  // TODO: should readSyms include readSyms(blocks)?
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => readSyms(op.body) ::: readSyms(op.dims)
    case op: DeliteMultiArrayMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => readSyms(op.body) ::: readSyms(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => readSyms(op.body) ::: readSyms(List(op.in, op.zero))
    case op: DeliteMultiArrayForeach[_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayForIndices[_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayNDMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayGroupBy[_,_] => readSyms(op.keyFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => readSyms(op.keyFunc) ::: readSyms(op.valFunc) ::: readSyms(op.redFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => readSyms(op.mapFunc) ::: readSyms(op.filtFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArraySortIndices => readSyms(op.body) ::: readSyms(op.length)
    case _ => super.readSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case op: DeliteMultiArrayFromFunction[_] => freqHot(op.body) ::: freqNormal(op.dims)
    case op: DeliteMultiArrayMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => freqHot(op.body) ::: freqNormal(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => freqHot(op.body) ::: freqNormal(List(op.in, op.zero))
    case op: DeliteMultiArrayForeach[_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayForIndices[_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayNDMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayGroupBy[_,_] => freqHot(op.keyFunc) ::: freqNormal(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => freqHot(List(op.keyFunc, op.valFunc, op.redFunc)) ::: freqNormal(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => freqHot(List(op.mapFunc, op.filtFunc)) ::: freqNormal(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArraySortIndices => freqHot(op.body) ::: freqNormal(op.length)
    case _ => super.symsFreq(e)
  }

 // symbols which are bound in a definition
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => List(op.i) ::: effectSyms(op.body)
    case op: DeliteMultiArrayMap[_,_] => List(op.i) ::: effectSyms(op.body)
    case op: DeliteMultiArrayZipWith[_,_,_] => List(op.i) ::: effectSyms(op.body)
    case op: DeliteMultiArrayReduce[_] => List(op.i, op.i2) ::: effectSyms(op.body)
    case op: DeliteMultiArrayForeach[_] => List(op.i) ::: effectSyms(op.body)
    case op: DeliteMultiArrayForIndices[_] => List(op.i) ::: effectSyms(op.body) 
    case op: DeliteMultiArrayNDMap[_,_] => List(op.fin) ::: effectSyms(op.body)
    case op: DeliteMultiArrayGroupBy[_,_] => List(op.i) ::: effectSyms(op.keyFunc)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => List(op.i,op.v._1,op.v._2) ::: effectSyms(op.keyFunc) ::: effectSyms(op.valFunc) ::: effectSyms(op.redFunc)
    case op: DeliteMultiArrayMapFilter[_,_] => List(op.i) ::: effectSyms(op.mapFunc) ::: effectSyms(op.filtFunc)
    case op: DeliteMultiArrayFlatMap[_,_] => List(op.i) ::: effectSyms(op.body)
    case op: DeliteMultiArraySortIndices => List(op.i._1,op.i._2) ::: effectSyms(op.body)
    case _ => super.boundSyms(e)
  }

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

  /////////////
  // mirroring

  // Needed for AtomicWrite mirroring
  // TODO: LHS of update may not be real here.. should we use fresh[..] in that case instead?
  override def mirrorDef[A:Manifest](e: Def[A], t: Transformer)(implicit ctx: SourceContext): Def[A] = e match {
    case DeliteMultiArrayUpdate(m,i,x) => DeliteMultiArrayUpdate(t(m),t(i),t(x))
    case DeliteMultiArrayInsert(m,x,i) => DeliteMultiArrayInsert(t(m),t(i),t(x))
    case DeliteMultiArrayInsertAll(m,x,a,i) => DeliteMultiArrayInsertAll(t(m),t(x),a,t(i))
    case DeliteMultiArrayRemove(m,a,s,l) => DeliteMultiArrayRemove(t(m),a,t(s),t(l))
    case MultiArrayAtomicWrite(m,i) => MultiArrayAtomicWrite(t(m),)
    case _ => super.mirrorDef(e,t)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteMultiArrayNew(d) => dmultia_new_immutable(f(d))(e.mA,ctx)
    case e@DeliteMultiArrayView(m,o,s,d) => dmultia_view(f(m),f(o),f(s),f(d))(e.mA,ctx)
    case e@DeliteMultiArrayRank(m) => dmultia_rank(f(m))(e.mA,ctx)
    case e@DeliteMultiArrayShape(m) => dmultia_shape(f(m))(e.mA,ctx)
    case e@DeliteMultiArraySize(m) => dmultia_size(f(m))(e.mA,ctx)
    case e@DeliteMultiArrayViewStart(m) => dmultia_view_start(f(m))(e.mA)
    case e@DeliteMultiArrayViewStride(m) => dmultia_view_stride(f(m))(e.mA)
    case e@DeliteMultiArrayApply(m,i) => dmultia_apply(f(m),f(i))(e.mA,ctx)
    
    case e@DeliteMultiArrayPermute(m,c) => dmultia_permute(f(m),c)(e.mA,ctx)
    case e@DeliteMultiArrayReshape(m,d) => dmultia_reshape(f(m),f(d))(e.mA,ctx)

    case e@DeliteMultiArrayFromFunction(d,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayFromFunction(f(d),g)(e.mA))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayMap(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayMap(f(m),g)(e.mA,e.mB,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayZipWith(ma,mb,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayZipWith(f(ma),f(mb),g)(e.mA,e.mB,e.mT,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayReduce(m,g,z) => 
      e.asInstanceOf[DeliteMultiArrayReduce[A]] match {   // scalac typer bug (same as in DeliteArray)
        case e@DeliteMultiArrayReduce(m,g,z) =>
          reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayReduce(f(m),g,f(z))(e.mA,ctx))(mtype(manifest[A]),ctx)
      }

    case e@DeliteMultiArrayForeach(m,g) => reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayForeach(f(m),g)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayForIndices(d,g) => reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayForIndices(f(d),g)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayNDMap(m,d,g) => reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayNDMap(f(m),d,g)(e.mA,e.mB,ctx))(mtype(manifest[A]),ctx)

    case e@DeliteMultiArrayUpdate(m,i,x) => toAtom(DeliteMultiArrayUpdate(f(m),f(i),f(x))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayInsert(m,x,i) => toAtom(DeliteMultiArrayInsert(f(m),f(x),f(i))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayInsertAll(m,r,a,i) => toAtom(DeliteMultiArrayInsertAll(f(m),f(r),a,f(i))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayRemove(m,a,s,l) => toAtom(DeliteMultiArrayRemove(f(m),a,f(s),f(l))(e.mA,ctx))(mtype(manifest[A]), ctx)
    
    case e@DeliteMultiArrayMkString(m,d) => dmultia_mkstring(f(m),f(d))(e.mA,ctx)
    case DeliteStringSplit(s,p,l) => dmultia_string_split(f(s),f(p),f(l))(ctx)

    case e@DeliteMultiArraySortIndices(l,c) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArraySortIndices(f(l),c))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DeliteMultiArrayMapFilter(m,g,c) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayMapFilter(f(m),g,c)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayFlatMap(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayFlatMap(f(m),g)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayGroupBy(m,k) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayGroupBy(f(m),k)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayGroupByReduce(m,k,v,r) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayGroupByReduce(f(m),k,v,r)(e.mA,e.mB,e.mT,ctx))(mtype(manifest[A]), ctx)

    case e@DeliteMatrixMultiply(l,r) => dmultia_matmult(f(l),f(r))(e.mA,e.nA,ctx)
    case e@DeliteMatrixVectorMultiply(m,v) => dmultia_matvecmult(f(m),f(v))(e.mA,e.nA,ctx)

    //case e@DeliteMultiArrayPin(m,l) => reflectPure(DeliteMultiArrayPin(f(m),l)(e.mA,e.mB))(mtype(manifest[A]), ctx)
    //case e@DeliteMultiArrayUnpin(in,l,s) => reflectPure(DeliteMultiArrayUnpin(f(in),l,f(s))(e.mA,e.mB))(mtype(manifest[A]),ctx)
   
    case Reflect(e@DeliteMultiArrayNew(d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayNew(f(d))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayView(m,o,s,d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayView(f(m),f(o),f(s),f(d))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayRank(m), u, es) => reflectMirrored(Reflect(DeliteMultiArrayRank(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayShape(m), u, es) => reflectMirrored(Reflect(DeliteMultiArrayShape(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArraySize(m), u, es) => reflectMirrored(Reflect(DeliteMultiArraySize(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayViewStart(m), u, es) => reflectMirrored(Reflect(DeliteMultiArrayViewStart(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayViewStride(m), u, es) => reflectMirrored(Reflect(DeliteMultiArrayViewStride(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayApply(m,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayApply(f(m),f(i))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayUpdate(m,i,x), u, es) => reflectMirrored(Reflect(DeliteMultiArrayUpdate(f(m),f(i),f(x))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayPermute(m,c), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPermute(f(m),c)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayReshape(m,d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayReshape(f(m),f(d))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayFromFunction(d,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayFromFunction(f(d),g)(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayMap(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayMap(f(m),g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayZipWith(ma,mb,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayZipWith(f(ma),f(mb),g)(e.mA,e.mB,e.mT,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayReduce(m,g,z), u, es) => 
      e.asInstanceOf[DeliteMultiArrayReduce[A]] match {  // scalac typer bug (same as in DeliteArray)
        case e@DeliteMultiArrayReduce(m,g,z) =>
          reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayReduce(f(m),g,f(z))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      }
    case Reflect(e@DeliteMultiArrayForeach(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayForeach(f(m),g)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayForIndices(d,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayForIndices(f(d),g)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayMutableMap(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayMutableMap(f(m),g)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayMutableZipWith(ma,mb,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayMutableZipWith(f(ma),f(mb),g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
    case Reflect(e@DeliteMultiArrayNDMap(m,d,g), u, es) => reflectMirrored(Reflect(DeliteMultiArrayNDMap(f(m),d,g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayInsert(m,x,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayInsert(f(m),f(x),f(i))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayInsertAll(m,r,a,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayInsertAll(f(m),f(r),a,f(i))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayRemove(m,a,s,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayRemove(f(m),a,f(s),f(l))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayMkString(m,d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayMkString(f(m),f(d))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(DeliteStringSplit(s,p,l), u, es) => reflectMirrored(Reflect(DeliteStringSplit(f(s),f(p),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArraySortIndices(l,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArraySortIndices(f(l),c), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayMapFilter(m,g,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayMapFilter(f(m),g,c)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayFlatMap(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayFlatMap(f(m),g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayGroupBy(m,k), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayGroupBy(f(m),k)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)        
    case Reflect(e@DeliteMultiArrayGroupByReduce(m,k,v,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayGroupByReduce(f(m),k,v,r)(e.mA,e.mB,e.mT,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMatrixMultiply(l,r), u, es) => reflectMirrored(Reflect(DeliteMatrixMultiply(f(l),f(r))(e.mA,e.nA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMatrixVectorMultiply(m,v), u, es) => reflectMirrored(Reflect(DeliteMatrixVectorMultiply(f(m),f(v))(e.mA,e.nA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    //case Reflect(e@DeliteMultiArrayPin(m,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPin(f(m),l)(e.mA,e.mB), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    //case Reflect(e@DeliteMultiArrayUnpin(a,l,s), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPin(f(a),l,f(s))(e.mA,e.mB), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}