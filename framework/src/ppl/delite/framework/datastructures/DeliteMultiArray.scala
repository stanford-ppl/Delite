package ppl.delite.framework.datastructures

import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}

//import ppl.delite.framework.visit.DeliteMetadata
import ppl.delite.framework.ops._

// Abstract, n-dimensional, multi-purpose array
// Intended for use at frontend w/ transformer to concrete data layouts
trait DeliteMultiArray[T]
trait DeliteArray1D[T] extends DeliteMultiArray[T]
trait DeliteArray2D[T] extends DeliteMultiArray[T]
trait DeliteArray3D[T] extends DeliteMultiArray[T]
trait DeliteArray4D[T] extends DeliteMultiArray[T]
trait DeliteArray5D[T] extends DeliteMultiArray[T]

trait DeliteMultiArrayOps extends DeliteAbstractOps {
  // --- Rank type casts
  def dmultia_as_1D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray1D[T]]
  def dmultia_as_2D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray2D[T]]
  def dmultia_as_3D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray3D[T]]
  def dmultia_as_4D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray4D[T]]
  def dmultia_as_5D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray5D[T]]

  object MultiArray {
    def apply[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = dmultia_new[T](dims)
    def imm[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext) = dmultia_new_immutable[T](dims)
    def fromFunction[T:Manifest](dims: Rep[Int]*)(func: Rep[LoopIndices] => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(dims, func)
  }

  implicit def repMultiAtoMultiAOps[T:Manifest](ma: Rep[DeliteMultiArray[T]])(implicit ctx: SourceContext) = new DeliteMultiArrayOpsCls(ma)
  class DeliteMultiArrayOpsCls[T:Manifest](ma: Rep[DeliteMultiArray[T]])(implicit ctx: SourceContext) {
    // --- rank casts
    def as1D: Rep[DeliteArray1D[T]] = dmultia_as_1D(ma)
    def as2D: Rep[DeliteArray2D[T]] = dmultia_as_2D(ma)
    def as3D: Rep[DeliteArray3D[T]] = dmultia_as_3D(ma)
    def as4D: Rep[DeliteArray4D[T]] = dmultia_as_4D(ma)
    def as5D: Rep[DeliteArray5D[T]] = dmultia_as_5D(ma)

    // --- properties
    def rank: Rep[Int] = dmultia_rank(ma)
    def size: Rep[Int] = dmultia_size(ma)
    def dim(i: Int): Rep[Int] = dmultia_dim(ma, i)

    // --- copies
    def mutable: Rep[DeliteMultiArray[T]] = dmultia_mutable(ma)
    def immutable: Rep[DeliteMultiArray[T]] = dmultia_immutable(ma)
    
    // --- Reshaping
    def reshape(d0: Rep[Int]): Rep[DeliteArray1D[T]] = dmultia_reshape(ma, Seq(d0)).as1D
    def reshape(d0: Rep[Int], d1: Rep[Int]): Rep[DeliteArray2D[T]] = dmultia_reshape(ma, Seq(d0, d1)).as2D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int]): Rep[DeliteArray3D[T]] = dmultia_reshape(ma, Seq(d0, d1, d2)).as3D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int]): Rep[DeliteArray4D[T]] = dmultia_reshape(ma, Seq(d0, d1, d2, d3)).as4D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int]): Rep[DeliteArray5D[T]] = dmultia_reshape(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int], d5: Rep[Int]*): Rep[DeliteMultiArray[T]] = dmultia_reshape(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    // --- Permuting
    def permute(d0: Int) = dmultia_permute(ma, Seq(d0)).as1D  // not really needed - does nothing
    def permute(d0: Int, d1: Int) = dmultia_permute(ma, Seq(d0, d1)).as2D // not really needed - special cases are matrix transpose, matrix copy
    def permute(d0: Int, d1: Int, d2: Int) = dmultia_permute(ma, Seq(d0, d1, d2)).as3D
    def permute(d0: Int, d1: Int, d2: Int, d3: Int) = dmultia_permute(ma, Seq(d0, d1, d2, d3)).as4D
    def permute(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int) = dmultia_permute(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def permute(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int, d5: Int*) = dmultia_permute(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    // --- single element
    def apply(i: Rep[Int]*): Rep[T] = dmultia_apply(ma,indices_new(i.toList))
    def update(i: Seq[Rep[Int]], x: Rep[T]): Rep[Unit] = dmultia_update(ma,indices_new(i),x)

    def apply(i: Rep[AbstractIndices]): Rep[T] = dmultia_apply(ma,i)
    def update(i: Rep[AbstractIndices], x: Rep[T]): Rep[Unit] = dmultia_update(ma,i,x)
    
    // --- parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteMultiArray[B]] = dmultia_map(ma,f)
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteMultiArray[R]] = dmultia_zipwith(ma,y,f)
    def reduce(zero: Rep[T])(f: (Rep[T],Rep[T]) => Rep[T]): Rep[T] = dmultia_reduce(ma,f,zero)
    def foreach(f: Rep[T] => Rep[Unit]): Rep[Unit] = dmultia_foreach(ma,{x: Rep[T] => f(x) })
    def forIndices(f: Rep[LoopIndices] => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma,{i: Rep[LoopIndices] => f(i) })
    def groupBy[K:Manifest](key: Rep[T] => Rep[K]) = dmultia_groupBy(ma,key)
    def groupByReduce[K:Manifest, V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]) = dmultia_groupByReduce(ma,key,value,reduce)

    def mmap(f: Rep[T] => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i))}
    def mzip[B:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i),y(i))}
  
    def mkString(dels: Rep[String]*) = dmultia_mkstring(ma,dels)
  }

  object Array1D {
    def apply[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext) = dmultia_new[T](List(len)).as1D
    def imm[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext) = dmultia_new_immutable[T](List(len)).as1D
    def fromFunction[T:Manifest](len: Rep[Int])(func: Rep[Int] => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(List(len), {x: Rep[LoopIndices] => func(x.flat)}).as1D
  
    def sortIndices(length: Rep[Int])(comparator: (Rep[Int], Rep[Int]) => Rep[Int])(implicit ctx: SourceContext) = dmultia_sortIndices(length, comparator)
    def splitString(str: Rep[String],pat: Rep[String],ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext) = dmultia_string_split(str,pat,ofs)

    def fromFile[T:Manifest](path: Rep[String])(func: Rep[String] => Rep[T])(implicit ctx: SourceContext) = dmultia_readfile(path, Seq(unit("\n")), func)
  }

  implicit def repArray1DtoArray1DOps[T:Manifest](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) = new DeliteArray1DOpsCls(ma)
  class DeliteArray1DOpsCls[T:Manifest](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def length: Rep[Int] = dmultia_size(ma)

    // --- copies
    def mutable: Rep[DeliteArray1D[T]] = dmultia_mutable(ma).as1D
    def immutable: Rep[DeliteArray1D[T]] = dmultia_immutable(ma).as1D

    // --- slicing
    // NOTE: len here is the length of the array view (including stride). should be calculated as floor((prev_len - start)/stride)
    def slice(start: Rep[Int], stride: Rep[Int], len: Rep[Int]) = dmultia_view(ma, Seq(start), Seq(stride), Seq(len)).as1D
    def slice(start: Rep[Int], len: Rep[Int]) = dmultia_view(ma, Seq(start), Seq(unit(1)), Seq(len)).as1D

    // --- mutability / buffering
    def update(i: Rep[Int], x: Rep[T]): Rep[Unit] = dmultia_update(ma,Indices(i),x)
    def insert(i: Rep[Int], x: Rep[T]): Rep[Unit] = dmultia_insert(ma,x,i)
    def append(x: Rep[T]): Rep[Unit] = dmultia_append(ma,x)
    def remove(start: Rep[Int], len: Rep[Int]): Rep[Unit] = dmultia_remove(ma, 0, start, len)
    def remove(i: Rep[Int]): Rep[Unit] = this.remove(i, unit(1))
  
    def insertAll(i: Rep[Int], rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_insertAll(ma, rhs, 0, i)
    def appendAll(rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 0)
    
    def :=(rhs: Rep[T]): Rep[Unit] = dmultia_append(ma, rhs)
    def ::=(rhs: Rep[DeliteArray1D[T]]): Rep[Unit] = dmultia_appendAll(ma, rhs, 0)

    def sortWith(func: (Rep[T], Rep[T]) => Rep[Int]): Rep[DeliteArray1D[T]] = dmultia_sortWith(ma, func)

    // --- 1D parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteArray1D[B]] = dmultia_map(ma,f).as1D
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteArray1D[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteArray1D[R]] = dmultia_zipwith(ma,y,f).as1D

    def forIndices(f: Rep[Int] => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma, {i: Rep[LoopIndices] => f(i.flat) })
    def filter(f: Rep[T] => Rep[Boolean]): Rep[DeliteArray1D[T]] = dmultia_filter(ma,f)
    def flatMap[B:Manifest](f: Rep[T] => Rep[DeliteArray1D[B]])(implicit ctx: SourceContext) = dmultia_flatmap(ma,f)  
  
    def mkString(del: Rep[String] = unit(",")) = dmultia_mkstring(ma,Seq(del))
    def writeFile(path: Rep[String])(func: Rep[T] => Rep[String])(implicit ctx: SourceContext) = dmultia_writefile(ma, Seq(unit("\n")), path, func)
  }

  implicit def repArray1DToOrderedArray1DOpsCls[T:Manifest:Ordering](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) = new DeliteArray1DOrderedOpsCls(ma)
  class DeliteArray1DOrderedOpsCls[T:Manifest:Ordering](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) {
    def sort = dmultia_sort(ma)
  }

  object Array2D {
    def apply[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext) = dmultia_new[T](List(rows,cols)).as2D
    def imm[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext) = dmultia_new_immutable[T](List(rows,cols)).as2D
    def fromFunction[T:Manifest](rows: Rep[Int], cols: Rep[Int])(func: (Rep[Int], Rep[Int]) => Rep[T])(implicit ctx: SourceContext) = dmultia_fromfunction(List(rows,cols), {x: Rep[LoopIndices] => func(x(0),x(1))}).as2D
    def fromFile[T:Manifest](path: Rep[String], cdel: Rep[String] = unit("\\s+"))(func: Rep[String] => Rep[T])(implicit ctx: SourceContext) = dmultia_readfile(path, Seq(unit("\n"), cdel), func)
  }

  implicit def repArray2DToArray2DOps[T:Manifest](ma: Rep[DeliteArray2D[T]])(implicit ctx: SourceContext) = new DeliteArray2DOpsCls(ma)
  class DeliteArray2DOpsCls[T:Manifest](ma: Rep[DeliteArray2D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def rows = dmultia_dim(ma, 0)
    def cols = dmultia_dim(ma, 1)

    // --- copies
    def mutable: Rep[DeliteArray2D[T]] = dmultia_mutable(ma).as2D
    def immutable: Rep[DeliteArray2D[T]] = dmultia_immutable(ma).as2D

    // --- Permuting
    def t: Rep[DeliteArray2D[T]] = dmultia_permute(ma, Seq(1,0)).as2D

    // --- Slicing
    def slice(startRow: Rep[Int], rowStride: Rep[Int], rows: Rep[Int], startCol: Rep[Int], colStride: Rep[Int], cols: Rep[Int]) = dmultia_view(ma, Seq(startRow, startCol), Seq(rowStride, colStride), Seq(rows, cols)).as2D
    def sliceRow(start: Rep[Int]) = dmultia_view(ma, Seq(start, unit(0)), Seq(unit(1)), Seq(cols), Seq(0)).as1D
    def sliceCol(start: Rep[Int]) = dmultia_view(ma, Seq(unit(0), start), Seq(cols), Seq(rows), Seq(1)).as1D

    def sliceRows(start: Rep[Int], num: Rep[Int]) = this.slice(start, unit(1), num, unit(0), unit(1), cols).as2D
    def sliceCols(start: Rep[Int], num: Rep[Int]) = this.slice(unit(0), unit(1), rows, start, unit(1), num).as2D

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
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteArray2D[B]] = dmultia_map(ma,f).as2D
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteArray2D[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteArray2D[R]] = dmultia_zipwith(ma,y,f).as2D

    def forIndices(f: (Rep[Int], Rep[Int]) => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma, {i: Rep[LoopIndices] => f(i(0), i(1)) })
    def mapRows[R:Manifest](f: Rep[DeliteArray1D[T]] => Rep[DeliteArray1D[R]]) = dmultia_NDmap(ma,List(0),{r: Rep[DeliteMultiArray[T]] => f(r.as1D) }).as2D
    def mapCols[R:Manifest](f: Rep[DeliteArray1D[T]] => Rep[DeliteArray1D[R]]) = dmultia_NDmap(ma,List(1),{c: Rep[DeliteMultiArray[T]] => f(c.as1D) }).as2D
  
    def mkString(rowDel: Rep[String] = unit("\n"), colDel: Rep[String] = unit(",")) = dmultia_mkstring(ma,Seq(rowDel,colDel))
    def writeFile(path: Rep[String], cdel: Rep[String] = unit("    "))(func: Rep[T] => Rep[String])(implicit ctx: SourceContext) = dmultia_writefile(ma, Seq(unit("\n"),cdel) path, func)
  }

  // --- File reading/writing
  def dmultia_readfile[A:Manifest](path: Rep[String], dels: Seq[Rep[String]], func: Rep[String] => Rep[A])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_writefile[A:Manifest](ma: Rep[DeliteMultiArray[A]], dels: Seq[Rep[String]], path: Rep[String], func: Rep[A] => Rep[String])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array constructors
  def dmultia_new[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_new_immutable[A:Manifest](dims: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_view[A:Manifest](ma: Rep[DeliteMultiArray[A]], start: Seq[Rep[Int]], stride: Seq[Rep[Int]], dims: Seq[Rep[Int]], unitDims: Seq[Int] = Nil)(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Array properties
  def dmultia_rank[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]
  def dmultia_dim[A:Manifest](ma: Rep[DeliteMultiArray[A]], n: Int)(implicit ctx: SourceContext): Rep[Int]
  def dmultia_size[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[Int]

  // --- Array single element
  def dmultia_apply[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Rep[AbstractIndices])(implicit ctx: SourceContext): Rep[A]
  def dmultia_update[A:Manifest](ma: Rep[DeliteMultiArray[A]], i: Rep[AbstractIndices], x: Rep[A])(implicit ctx: SourceContext): Rep[Unit]

  // --- Array copies / reshaping
  def dmultia_mutable[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_immutable[A:Manifest](ma: Rep[DeliteMultiArray[A]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_permute[A:Manifest](ma: Rep[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_reshape[A:Manifest](ma: Rep[DeliteMultiArray[A]], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Parallel Ops
  def dmultia_fromfunction[A:Manifest](dims: Seq[Rep[Int]], f: Rep[LoopIndices] => Rep[A])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_map[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Rep[DeliteMultiArray[A]], mb: Rep[DeliteMultiArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[DeliteMultiArray[R]]
  def dmultia_reduce[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def dmultia_forindices[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[LoopIndices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
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

  // --- Misc.
  def dmultia_mkstring[A:Manifest](ma: Rep[DeliteMultiArray[A]], dels: Seq[Rep[String]])(implicit ctx: SourceContext): Rep[String]

  // --- 1D Ops
  def dmultia_sortIndices(len: Rep[Int], comp: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[Int]]
  def dmultia_sort[A:Manifest:Ordering](ma: Rep[DeliteArray1D[A]])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]
  def dmultia_sortWith[A:Manifest](ma: Rep[DeliteArray1D[A]], func: (Rep[A], Rep[A]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]
  def dmultia_string_split(str: Rep[String],pat: Rep[String],ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[DeliteArray1D[String]]

  // --- 2D Ops
  def dmultia_matmult[A:Manifest:Numeric](lhs: Rep[DeliteArray2D[A]], rhs: Rep[DeliteArray2D[A]])(implicit ctx: SourceContext): Rep[DeliteArray2D[A]]
  def dmultia_matvecmult[A:Manifest:Numeric](mat: Rep[DeliteArray2D[A]], vec: Rep[DeliteArray1D[A]])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]

  // --- Pinning
  //def dmultia_pin[T:Manifest,R:Manifest](ma: Rep[DeliteMultiArray[T]], layout: Layout[T,R])(implicit ctx: SourceContext): Rep[DeliteArray[R]]
  //def dmultia_unpin[T:Manifest,R:Manifest](in: Rep[DeliteArray[R]], layout: Layout[T,R], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[T]]
}

trait DeliteMultiArrayOpsExp extends DeliteMultiArrayOps with DeliteAbstractOpsExp with EffectExp {
  this: DeliteOpsExp => // Needed for DeliteStructsExp

  // Abstract def family identifier
  private implicit val fc = AbstractFamily("multiarray")

  /////////////////////
  // Abstract IR Nodes

  // --- Array constructors
  case class DeliteMultiArrayNew[A:Manifest](dims: Seq[Exp[Int]]) extends AbstractDefWithManifest[A,DeliteMultiArray[A]]
  case class DeliteMultiArrayView[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]], unitDims: Seq[Int])(implicit ctx: SourceContext) extends AbstractDefWithManifest[A,DeliteMultiArray[A]]

  // --- Array properties
  case class DeliteMultiArrayRank[A:Manifest](ma: Exp[DeliteMultiArray[A]]) extends AbstractDefWithManifest[A,Int]
  case class DeliteMultiArrayDim[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Int) extends AbstractDefWithManifest[A, Int]
  case class DeliteMultiArraySize[A:Manifest](ma: Exp[DeliteMultiArray[A]]) extends AbstractDefWithManifest[A,Int]

  // --- Array single element ops
  case class DeliteMultiArrayApply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices]) extends AbstractDefWithManifest[A,A]
  case class DeliteMultiArrayUpdate[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices], x: Exp[A])(implicit ctx: SourceContext) extends AbstractAtomicWriteDef[A] {
    def externalFields = List(i, x)
  }

  // --- Array permute / reshaping
  case class DeliteMultiArrayPermute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) extends AbstractDefWithManifest[A,DeliteMultiArray[A]]
  case class DeliteMultiArrayReshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) extends AbstractDefWithManifest[A,DeliteMultiArray[A]]

  // --- Parallel Ops
  /** 
   * Read multidimensional array from file
   * Output MultiArray's rank is equivalent to the number of delimeters given
   * @param path - relative (?) path to file
   * @param dels - file delimeters corresponding to dimensions. first is expected to be newline. 
   * @param func - mapping function from input element string to element
   */
  case class DeliteMultiArrayReadFile[A:Manifest](path: Exp[String], dels: Seq[Exp[String]], func: Exp[String] => Exp[A]) extends DeliteAbstractLoop[A,DeliteMultiArray[A]] {
    type OpType <: DeliteMultiArrayReadFile[A]
    lazy val rV: Sym[String] = copyOrElse(_.rV)(fresh[String])
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(rV)))
  }

  /** 
   * Parallel creation of a multidimensional array
   * @param dims - a list of dimensions for the array (length is equal to array rank)
   * @param func - function from multidimensional index (e.g. (d1, d2, ... dn)) to element
   */
  case class DeliteMultiArrayFromFunction[A:Manifest](dims: Seq[Exp[Int]], func: Exp[LoopIndices] => Exp[A]) extends DeliteAbstractLoop[A,DeliteMultiArray[A]] {
    type OpType <: DeliteMultiArrayFromFunction[A]
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(i)))
  }

  /** 
   * Parallel map over all elements in multidimensional array
   * @param in   - input multi-array
   * @param func - lambda mapping function 
   */
  case class DeliteMultiArrayMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[R])(implicit ctx: SourceContext) extends DeliteAbstractLoop2[A,R,DeliteMultiArray[R]] {
    type OpType <: DeliteMultiArrayMap[A,R]
    val body: Block[R] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(in,i))))
  }

  /**
   * Parallel zip over two multidimensional-arrays
   * Multidimensional arrays must be of the same rank, and are assumed to be of the same dimension(s)
   * @param inA  - first input multi-array
   * @param inB  - second input multi-array
   * @param func - zipWith function
   */
  case class DeliteMultiArrayZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], func: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) extends DeliteAbstractLoop3[A,B,R,DeliteMultiArray[R]] {
    type OpType <: DeliteMultiArrayZipWith[A,B,R]
    val body: Block[R] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(inA,i), dmultia_apply(inB,i))))
  }

  /**
   * Parallel reduction of a multidimensional array
   * @param in   - input multi-array
   * @param func - reduction function (must be associative)
   * @param zero - "empty" value
   */
  case class DeliteMultiArrayReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) extends DeliteAbstractLoop[A,A] {
    type OpType <: DeliteMultiArrayReduce[A]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)((fresh[A], fresh[A]))
    val lookup: Block[A] = copyTransformedBlockOrElse(_.lookup)(reifyEffects(dmultia_apply(in, i)))
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(rV._1, rV._2)))
  }
  
  /** 
   * Parallel foreach over elements of a multidimensional array
   * @param in   - input multi-array
   * @param func - foreach function
   */ 
  case class DeliteMultiArrayForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) extends DeliteAbstractLoop[A,Unit] {
    type OpType <: DeliteMultiArrayForeach[A]
    val body: Block[Unit] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(in,i))))
  }

  /** 
   * Parallel loop over indices in multidimensional array's dimension(s)
   * @param in   - input multi-array
   * @param func - function on set of indices
   */ 
  case class DeliteMultiArrayForIndices[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[LoopIndices] => Exp[Unit])(implicit ctx: SourceContext) extends DeliteAbstractLoop[A,Unit] {
    type OpType <: DeliteMultiArrayForIndices[A]
    val body: Block[Unit] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(i)))
  }

  /**
   * Parallel N-Dimensional Flat Map on multi-dimensional array
   * @param in    - input multi-array
   * @param mdims - map dimensions 
   * @param func  - flat map function
   */
  case class DeliteMultiArrayNDMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], mdims: Seq[Int], func: Exp[DeliteMultiArray[A]] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) extends DeliteAbstractLoop2[A,B,DeliteMultiArray[B]] {
    type OpType <: DeliteMultiArrayNDMap[A,B]
    lazy val rV: Sym[DeliteMultiArray[A]] = copyTransformedOrElse(_.rV)(fresh[DeliteMultiArray[A]]).asInstanceOf[Sym[DeliteMultiArray[A]]]
    val body: Block[DeliteMultiArray[B]] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(rV)))
  }

  /**
   * Parallel Group-By on multi-dimensional array
   * @param in  - input multi-array 
   * @param key - key (hash) function
   */
  case class DeliteMultiArrayGroupBy[A:Manifest,K:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K])(implicit ctx: SourceContext) extends DeliteAbstractLoop2[A,K,DeliteHashMap[K,DeliteArray1D[A]]] {
    type OpType <: DeliteMultiArrayGroupBy[A,K]
    val keyFunc: Block[K] = copyTransformedBlockOrElse(_.keyFunc)(reifyEffects(key(dmultia_apply(in,i))))
  }

  /**
   * Parallel Group-By-Reduce on multidimensional array
   * @param in     - input multi-array
   * @param key    - key (hash) function
   * @param value  - mapping function from elements of in to HashMap values
   * @param reduce - reduction on values (with same key)
   */
  case class DeliteMultiArrayGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) extends DeliteAbstractLoop3[A,K,V,DeliteHashMap[K,V]] {
    type OpType <: DeliteMultiArrayGroupByReduce[A,K,V]
    lazy val rV: (Sym[V],Sym[V]) = copyOrElse(_.rV)((fresh[V],fresh[V]))
    val keyFunc: Block[K] = copyTransformedBlockOrElse(_.keyFunc)(reifyEffects(key(dmultia_apply(in,i))))
    val valFunc: Block[V] = copyTransformedBlockOrElse(_.valFunc)(reifyEffects(value(dmultia_apply(in,i))))
    val redFunc: Block[V] = copyTransformedBlockOrElse(_.redFunc)(reifyEffects(reduce(rV._1,rV._2)))
  }

  // --- 1D Parallel Ops
  /** 
   * Parallel Map-Filter on a 1D array
   * @param in   - input array
   * @param func - mapping function
   * @param cond - filter function
   */
  case class DeliteMultiArrayMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) extends DeliteAbstractLoop2[A,B,DeliteArray1D[B]] {
    type OpType <: DeliteMultiArrayMapFilter[A,B]
    val mapFunc: Block[B] = copyTransformedBlockOrElse(_.mapFunc)(reifyEffects(func(dmultia_apply(in,i))))
    val filtFunc: Block[Boolean] = copyTransformedBlockOrElse(_.filtFunc)(reifyEffects(cond(dmultia_apply(in,i))))
  }

  /**
   * Parallel Flat Map on a 1D array
   * @param in   - input array
   * @param func - flat map function
   */
  case class DeliteMultiArrayFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], func: Exp[A] => Exp[DeliteArray1D[B]])(implicit ctx: SourceContext) extends DeliteAbstractLoop2[A,B,DeliteArray1D[B]] {
    type OpType <: DeliteMultiArrayFlatMap[A,B]
    val body: Block[DeliteArray1D[B]] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(in,i))))
  }

  // --- Buffer operations
  case class DeliteMultiArrayInsert[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A], index: Exp[Int])(implicit ctx: SourceContext) extends AbstractAtomicWriteDef[A] {
    def externalFields = List(index, x)
  }
  case class DeliteMultiArrayAppend[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A])(implicit ctx: SourceContext) extends AbstractAtomicWriteDef[A] {
    def externalFields = List(x)
  }
  case class DeliteMultiArrayInsertAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]], axis: Int, index: Exp[Int])(implicit ctx: SourceContext) extends AbstractAtomicWriteDef[A] {
    def externalFields = List(rhs, axis, index)
  }
  case class DeliteMultiArrayRemove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) extends AbstractAtomicWriteDef[A] {
    def externalFields = List(axis, start, len)
  }

  // --- Misc Ops.
  /**
   * Creation of a string representation of a MultiArray
   * @param ma   - input MultiArray
   * @param dels - delimeters across dimensions. Number of delimeters should equal rank of input MultiArray 
   */ 
  case class DeliteMultiArrayMkString[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]]) extends AbstractDefWithManifest[A,String]
    
  /** 
   * Abstract node for writing a MultiArray to a file
   * @param in   - input MultiArray
   * @param dels - delimeters across dimensions. Number of delimeters should equal rank of input MultiArray
   * @param path - relative path (?) to output file
   * @param func - mapping from element of MultiArray to string
   */
  case class DeliteMultiArrayWriteFile[A:Manifest](in: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], path: Exp[String], func: Exp[A] => Exp[String]) extends DeliteAbstractLoop[A,Unit] {
    type OpType <: DeliteMultiArrayWriteFile[A]
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(ma,i))))
  }

  // --- 1D Ops
  /**
   * General sort function using Java comparator interface
   * From Java doc: Comparator "returns a negative integer, zero, or a positive integer as the 
   * first argument is less than, equal to, or greater than the second."
   * @param len        - length of array to be created
   * @param comparator - comparator function on indices
   */
  case class DeliteMultiArraySortIndices(len: Exp[Int], i: (Sym[Int], Sym[Int]), body: Block[Int]) extends AbstractDef[DeliteArray1D[Int]]
  case class DeliteMultiArrayStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int]) extends AbstractDefWithManifest[String,DeliteArray1D[String]]

  // --- 2D Ops
  case class DeliteMatrixMultiply[A:Manifest:Numeric](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]])(implicit ctx: SourceContext) extends AbstractDefWithManifest[A,DeliteArray2D[A]] {
    val nA = implicitly[Numeric[A]]
  }
  case class DeliteMatrixVectorMultiply[A:Manifest:Numeric](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext) extends AbstractDefWithManifest[A,DeliteArray1D[A]] {
    val nA = implicitly[Numeric[A]]
  }

  // --- Atomic Write (MultiArray)
  case class MultiArrayTracer(i: Exp[AbstractIndices]) extends AbstractAtomicTracer

  override def mirrorTrace(t: AtomicTracer, f: Transformer)(implicit pos: SourceContext): AtomicTracer = t match {
    case MultiArrayTracer(i) => MultiArrayTracer(f(i))
    case _ => super.mirrorTrace(t,f)
  }
  override def recurseLookup[T:Manifest](target: Exp[Any], trace: List[AtomicTracer]): (Exp[Any], List[AtomicTracer]) = target match {
    case Def(DeliteMultiArrayApply(ma,i)) => recurseLookup(ma, MultiArrayTracer(i) +: trace)
    case Def(Reflect(DeliteMultiArrayApply(ma,i),_,_)) => recurseLookup(ma, MultiArrayTracer(i) +: trace)
    case _ => super.recurseLookup(target, trace)
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
  def dmultia_new_immutable[A:Manifest](dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = DeliteMultiArrayNew[A](dims)
  def dmultia_view[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]], unitDims: Seq[Int] = Nil)(implicit ctx: SourceContext) = DeliteMultiArrayView(ma,start,stride,dims,unitDims)

  // --- Array properties
  def dmultia_rank[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = DeliteMultiArrayRank(ma)
  def dmultia_dim[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Int)(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayDim(ma, i))
  def dmultia_size[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = DeliteMultiArraySize(ma)

  // --- Array single element ops
  def dmultia_apply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices])(implicit ctx: SourceContext) = DeliteMultiArrayApply(ma,i)
  def dmultia_update[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices], x: Exp[A])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayUpdate(ma,i,x))

  // --- Array permute / reshaping
  def dmultia_permute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) = DeliteMultiArrayPermute(ma,config)
  def dmultia_reshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) = DeliteMultiArrayReshape(ma,dims)
  
  // --- Array copying
  // clone doubles as view's toDense operation
  // TODO: do we need a symbol hint for this?
  def dmultia_clone[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = DeliteMultiArrayMap(ma,{(e: Exp[A])=>e})
  def dmultia_mutable[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = reflectMutable(DeliteMultiArrayMap(ma,{(e:Exp[A])=>e}))
  def dmultia_immutable[A:Manifest](ma: Exp[DeliteMultiArray[A]])(implicit ctx: SourceContext) = DeliteMultiArrayMap(ma,{(e:Exp[A])=>e})
  
  // --- Array parallel ops
  def dmultia_fromfunction[A:Manifest](dims: Seq[Exp[Int]], f: Exp[LoopIndices] => Exp[A])(implicit ctx: SourceContext) = DeliteMultiArrayFromFunction(dims,f)
  def dmultia_map[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[B])(implicit ctx: SourceContext) = DeliteMultiArrayMap(ma,f)
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[A]], mb: Exp[DeliteMultiArray[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) = DeliteMultiArrayZipWith(ma,mb,f)
  def dmultia_reduce[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) = DeliteMultiArrayReduce(ma,f,zero)
  def dmultia_forindices[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[LoopIndices] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForIndices[A](ma,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }
  def dmultia_foreach[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForeach(ma,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }
  def dmultia_NDmap[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], mdims: Seq[Int], func: Exp[DeliteMultiArray[A]] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) = DeliteMultiArrayNDMap(ma,mdims,func)
  def dmultia_groupBy[A:Manifest,K:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K])(implicit ctx: SourceContext) = DeliteMultiArrayGroupBy(ma,key)
  def dmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) = DeliteMultiArrayGroupByReduce(ma,key,value,reduce)
  
  // --- 1D Parallel Ops
  def dmultia_flatmap[A:Manifest,B:Manifest](ma: Exp[DeliteArray1D[A]], f: Exp[A] => Exp[DeliteArray1D[B]])(implicit ctx: SourceContext) = DeliteMultiArrayFlatMap(ma,f)
  def dmultia_filter[A:Manifest](ma: Exp[DeliteArray1D[A]], f: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = dmultia_mapfilter(ma, {(e:Exp[A]) => e}, f)
  def dmultia_mapfilter[A:Manifest,B:Manifest](ma: Exp[DeliteArray1D[A]], map: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = DeliteMultiArrayMapFilter(ma,map,cond)
  
  // --- Buffer ops
  def dmultia_insert[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A], index: Exp[Int])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayInsert(ma,x,index))
  def dmultia_append[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayAppend(ma,x))
  def dmultia_insertAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]], axis: Int, index: Exp[Int])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayInsertAll(ma,rhs,axis,index))
  def dmultia_remove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) = reflectAtomicWrite(ma)(DeliteMultiArrayRemove(ma,axis,start,len))

  def dmultia_appendAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]], axis: Int)(implicit ctx: SourceContext) = dmultia_insertAll(ma, rhs, axis, ma.dim(axis))

  // --- Misc.
  def dmultia_mkstring[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]])(implicit ctx: SourceContext) = DeliteMultiArrayMkString(ma,dels)
  def dmultia_readfile[A:Manifest](path: Exp[String], dels: Seq[Exp[String]], func: Exp[String] => Exp[A])(implicit ctx: SourceContext): Exp[DeliteMultiArray[A]] = DeliteMultiArrayReadFile(path, dels, func)
  def dmultia_writefile[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], path: Exp[String], func: Exp[A] => Exp[String])(implicit ctx: SourceContext): Exp[Unit] = reflectEffect(DeliteMultiArrayWriteFile(ma, dels, path, func))

  // --- 1D Operations
  def dmultia_sortIndices(len: Exp[Int], comp: (Exp[Int],Exp[Int]) => Exp[Int])(implicit ctx: SourceContext) = {
    val i = (fresh[Int],fresh[Int])
    DeliteMultiArraySortIndices(len, i, reifyEffects(comp(i._1,i._2)) )
  }

  // TODO: both of these implementations use sortIndices + a gather operation. Better way?
  def dmultia_sort[A:Manifest:Ordering](ma: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext) = dmultia_sortIndices(dmultia_size(ma), {(i,j) => 
    val aV = ma(i)
    val bV = ma(j)
    // Must have 3 conditions and then a default to conform to Java runtime comparator contract.
    if (delite_less_than(aV, bV)) unit(-1)
    else if (delite_equals(aV, bV)) unit(0)
    else if (delite_greater_than(aV, bV)) unit(1)
    else unit(0)
  }).map{x => ma(x)}

  def dmultia_sortWith[A:Manifest](ma: Exp[DeliteArray1D[A]], func: (Exp[A], Exp[A]) => Exp[Int])(implicit ctx: SourceContext) = dmultia_sortIndices(dmultia_size(ma), {(i,j) =>
    val aV = ma(i)
    val bV = ma(j)
    val r = func(aV, bV)
    // Must have 3 conditions and then a default to conform to Java runtime comparator contract.
    if (delite_less_than(r, unit(0))) unit(-1)
    else if (delite_equals(r, unit(0))) unit(0)
    else if (delite_greater_than(r, unit(0))) unit(1)
    else unit(0)
  }).map{x => ma(x)}

  def dmultia_string_split(str: Exp[String], pat: Exp[String], ofs: Exp[Int] = unit(0))(implicit ctx: SourceContext) = DeliteMultiArrayStringSplit(str,pat,ofs)

  // --- 2D Operations
  def dmultia_matmult[A:Manifest:Numeric](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]])(implicit ctx: SourceContext) = DeliteMatrixMultiply(lhs,rhs)
  def dmultia_matvecmult[A:Manifest:Numeric](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext) = DeliteMatrixVectorMultiply(mat,vec)

  // --- Pinning
  // TBD
  //def dmultia_pin[T:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[T]], layout: Layout[T,R])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayPin[T,R](ma,layout))
  //def dmultia_unpin[T:Manifest,R:Manifest](in: Exp[DeliteArray[R]], layout: Layout[T,R], shape: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayUnpin[T,R](ma,layout,shape))

  ////////////////
  // dependencies

  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => blocks(op.body)
    case op: DeliteMultiArrayMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayZipWith[_,_,_] => blocks(op.body)
    case op: DeliteMultiArrayReduce[_] => blocks(op.body) ::: blocks(op.lookup)
    case op: DeliteMultiArrayForeach[_] => blocks(op.body)
    case op: DeliteMultiArrayForIndices[_] => blocks(op.body)
    case op: DeliteMultiArrayNDMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayGroupBy[_,_] => blocks(op.keyFunc)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.redFunc)
    case op: DeliteMultiArrayMapFilter[_,_] => blocks(op.mapFunc) ::: blocks(op.filtFunc)
    case op: DeliteMultiArrayFlatMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayReadFile[_] => blocks(op.body)
    case op: DeliteMultiArrayWriteFile[_] => blocks(op.body)
    case _ => super.blocks(e)
  }

  // regular data dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => syms(op.body) ::: syms(op.dims)
    case op: DeliteMultiArrayMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => syms(op.body) ::: syms(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => syms(op.body) ::: syms(op.lookup) ::: syms(List(op.in, op.zero))
    case op: DeliteMultiArrayForeach[_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayForIndices[_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayNDMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayGroupBy[_,_] => syms(op.keyFunc) ::: syms(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => syms(op.keyFunc) ::: syms(op.valFunc) ::: syms(op.redFunc) ::: syms(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => syms(op.mapFunc) ::: syms(op.filtFunc) ::: syms(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayReadFile[_] => syms(op.body) ::: syms(op.path) ::: syms(op.dels)
    case op: DeliteMultiArrayWriteFile[_] => syms(op.body) ::: syms(op.path) ::: syms(op.dels) ::: syms(op.in)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => readSyms(op.body) ::: readSyms(op.dims)
    case op: DeliteMultiArrayMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => readSyms(op.body) ::: readSyms(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => readSyms(op.body) ::: readSyms(op.lookup) ::: readSyms(List(op.in, op.zero))
    case op: DeliteMultiArrayForeach[_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayForIndices[_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayNDMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayGroupBy[_,_] => readSyms(op.keyFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => readSyms(op.keyFunc) ::: readSyms(op.valFunc) ::: readSyms(op.redFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => readSyms(op.mapFunc) ::: readSyms(op.filtFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayReadFile[_] => readSyms(op.body) ::: readSyms(op.path) ::: readSyms(op.dels)
    case op: DeliteMultiArrayWriteFile[_] => readSyms(op.body) ::: readSyms(op.path) ::: readSyms(op.dels) ::: readSyms(op.in)
    case _ => super.readSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case op: DeliteMultiArrayFromFunction[_] => freqHot(op.body) ::: freqNormal(op.dims)
    case op: DeliteMultiArrayMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => freqHot(op.body) ::: freqNormal(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => freqHot(op.body) ::: freqHot(op.lookup) ::: freqNormal(List(op.in, op.zero))
    case op: DeliteMultiArrayForeach[_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayForIndices[_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayNDMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayGroupBy[_,_] => freqHot(op.keyFunc) ::: freqNormal(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => freqHot(List(op.keyFunc, op.valFunc, op.redFunc)) ::: freqNormal(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => freqHot(List(op.mapFunc, op.filtFunc)) ::: freqNormal(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayReadFile[_] => freqHot(op.body) ::: freqNormal(op.path) ::: freqNormal(op.dels)
    case op: DeliteMultiArrayWriteFile[_] => freqHot(op.body) ::: freqNormal(op.path) ::: freqNormal(op.dels) ::: freqNormal(op.in)
    case _ => super.symsFreq(e)
  }

 // symbols which are bound in a definition
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayMap[_,_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayZipWith[_,_,_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayReduce[_] => List(op.v) ::: List(op.rV._1, op.rV._2) ::: effectSyms(op.body) ::: effectSyms(op.lookup)
    case op: DeliteMultiArrayForeach[_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayForIndices[_] => List(op.v) ::: effectSyms(op.body) 
    case op: DeliteMultiArrayNDMap[_,_] => List(op.v) ::: List(op.rV) ::: effectSyms(op.body)
    case op: DeliteMultiArrayGroupBy[_,_] => List(op.v) ::: effectSyms(op.keyFunc)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => List(op.v,op.rV._1,op.rV._2) ::: effectSyms(op.keyFunc) ::: effectSyms(op.valFunc) ::: effectSyms(op.redFunc)
    case op: DeliteMultiArrayMapFilter[_,_] => List(op.v) ::: effectSyms(op.mapFunc) ::: effectSyms(op.filtFunc)
    case op: DeliteMultiArrayFlatMap[_,_] => List(op.v) ::: effectSyms(op.body)
    case DeliteMultiArraySortIndices(_,i,body) => syms(i) ::: effectSyms(body)
    case op: DeliteMultiArrayReadFile[_] => List(op.v) ::: List(op.rV) ::: effectSyms(op.body)
    case op: DeliteMultiArrayWriteFile[_] => List(op.v) ::: effectSyms(op.body)
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
  // LHS symbol is not real here - ignored in mirroring
  override def mirrorNestedAtomic[A:Manifest](d: AtomicWrite[A], f: Transformer)(implicit ctx: SourceContext): AtomicWrite[A] = d match {
    case op@DeliteMultiArrayUpdate(m,i,x) => DeliteMultiArrayUpdate(m,f(i),f(x))(op.mA, ctx)
    case op@DeliteMultiArrayInsert(m,x,i) => DeliteMultiArrayInsert(m,f(x),f(i))(op.mA, ctx)
    case op@DeliteMultiArrayAppend(m,x) => DeliteMultiArrayAppend(m,f(x))(op.mA,ctx)
    case op@DeliteMultiArrayInsertAll(m,x,a,i) => DeliteMultiArrayInsertAll(m,f(x),a,f(i))(op.mA, ctx)
    case op@DeliteMultiArrayRemove(m,a,s,l) => DeliteMultiArrayRemove(m,a,f(s),f(l))(op.mA, ctx)
    case _ => super.mirrorNestedAtomic(d,f)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    // --- properties
    case e@DeliteMultiArrayRank(m) => dmultia_rank(f(m))(e.mA,ctx)
    case e@DeliteMultiArrayDim(m,i) => dmultia_dim(f(m),i)(e.mA,ctx)
    case e@DeliteMultiArraySize(m) => dmultia_size(f(m))(e.mA,ctx)

    // --- creation
    case e@DeliteMultiArrayNew(d) => dmultia_new_immutable(f(d))(e.mA,ctx)
    case e@DeliteMultiArrayView(m,o,s,d,ud) => dmultia_view(f(m),f(o),f(s),f(d),ud)(e.mA,ctx)
    
    case e@DeliteMultiArrayApply(m,i) => dmultia_apply(f(m),f(i))(e.mA,ctx)
    case e@DeliteMultiArrayUpdate(m,i,x) => toAtom(DeliteMultiArrayUpdate(f(m),f(i),f(x))(e.mA,ctx))(mtype(manifest[A]), ctx)

    case e@DeliteMultiArrayPermute(m,c) => dmultia_permute(f(m),c)(e.mA,ctx)
    case e@DeliteMultiArrayReshape(m,d) => dmultia_reshape(f(m),f(d))(e.mA,ctx)

    // --- Parallel ops
    case e@DeliteMultiArrayFromFunction(d,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayFromFunction(f(d),g)(e.mA))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayMap(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayMap(f(m),g)(e.mA,e.mB,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayZipWith(ma,mb,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayZipWith(f(ma),f(mb),g)(e.mA,e.mB,e.mT,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayReduce(m,g,z) => 
      e.asInstanceOf[DeliteMultiArrayReduce[A]] match {   // scalac typer bug (same as in DeliteArray)
        case e@DeliteMultiArrayReduce(m,g,z) =>
          reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayReduce(f(m),g,f(z))(e.mA,ctx))(mtype(manifest[A]),ctx)
      }
    case e@DeliteMultiArrayForeach(m,g) => reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayForeach(f(m),g)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayForIndices(m,g) => reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayForIndices(f(m),g)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayNDMap(m,d,g) => reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayNDMap(f(m),d,g)(e.mA,e.mB,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayGroupBy(m,k) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayGroupBy(f(m),k)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayGroupByReduce(m,k,v,r) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayGroupByReduce(f(m),k,v,r)(e.mA,e.mB,e.mT,ctx))(mtype(manifest[A]), ctx)

    // --- mutations (TODO: do we need these?)
    case e@DeliteMultiArrayInsert(m,x,i) => toAtom(DeliteMultiArrayInsert(f(m),f(x),f(i))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayAppend(m,x) => toAtom(DeliteMultiArrayAppend(f(m),f(x))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayInsertAll(m,r,a,i) => toAtom(DeliteMultiArrayInsertAll(f(m),f(r),a,f(i))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayRemove(m,a,s,l) => toAtom(DeliteMultiArrayRemove(f(m),a,f(s),f(l))(e.mA,ctx))(mtype(manifest[A]), ctx)

    // --- Misc
    case e@DeliteMultiArrayMkString(m,d) => dmultia_mkstring(f(m),f(d))(e.mA,ctx)
    case e@DeliteMultiArrayReadFile(p,d,k) => reflectPure(new {override val original = Some(f,e) } with DeliteMultiArrayReadFile(f(p),f(d),k)(e.mA,ctx))(mtype(manifest[A]), ctx)

    // --- 1D Ops
    case DeliteMultiArrayStringSplit(s,p,l) => dmultia_string_split(f(s),f(p),f(l))(ctx)
    case e@DeliteMultiArraySortIndices(l,v,c) => reflectPure(DeliteMultiArraySortIndices(f(l),(f(v._1).asInstanceOf[Sym[Int]],f(v._2).asInstanceOf[Sym[Int]]),f(c)))

    // --- 1D Parallel
    case e@DeliteMultiArrayMapFilter(m,g,c) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayMapFilter(f(m),g,c)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayFlatMap(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayFlatMap(f(m),g)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)

    // --- 2D Parallel 
    case e@DeliteMatrixMultiply(l,r) => dmultia_matmult(f(l),f(r))(e.mA,e.nA,ctx)
    case e@DeliteMatrixVectorMultiply(m,v) => dmultia_matvecmult(f(m),f(v))(e.mA,e.nA,ctx)

    //case e@DeliteMultiArrayPin(m,l) => reflectPure(DeliteMultiArrayPin(f(m),l)(e.mA,e.mB))(mtype(manifest[A]), ctx)
    //case e@DeliteMultiArrayUnpin(in,l,s) => reflectPure(DeliteMultiArrayUnpin(f(in),l,f(s))(e.mA,e.mB))(mtype(manifest[A]),ctx)
   
    // --- reflect of properties
    case Reflect(e@DeliteMultiArrayRank(m), u, es) => reflectMirrored(Reflect(DeliteMultiArrayRank(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayDim(m,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayDim(f(m),i)(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArraySize(m), u, es) => reflectMirrored(Reflect(DeliteMultiArraySize(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx) 

    // --- creation
    case Reflect(e@DeliteMultiArrayNew(d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayNew(f(d))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayView(m,o,s,d,ud), u, es) => reflectMirrored(Reflect(DeliteMultiArrayView(f(m),f(o),f(s),f(d),ud)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

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
    case Reflect(e@DeliteMultiArrayForIndices(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayForIndices(f(m),g)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayNDMap(m,d,g), u, es) => reflectMirrored(Reflect(DeliteMultiArrayNDMap(f(m),d,g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayGroupBy(m,k), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayGroupBy(f(m),k)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)        
    case Reflect(e@DeliteMultiArrayGroupByReduce(m,k,v,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayGroupByReduce(f(m),k,v,r)(e.mA,e.mB,e.mT,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayInsert(m,x,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayInsert(f(m),f(x),f(i))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayAppend(m,x), u, es) => reflectMirrored(Reflect(DeliteMultiArrayAppend(f(m),f(x))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayInsertAll(m,r,a,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayInsertAll(f(m),f(r),a,f(i))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayRemove(m,a,s,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayRemove(f(m),a,f(s),f(l))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayMkString(m,d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayMkString(f(m),f(d))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayReadFile(p,d,k), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with DeliteMultiArrayReadFile(f(p),f(d),k)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayWriteFile(i,d,p,k), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with DeliteMultiArrayWriteFile(f(i),f(d),f(p),k)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
 
    case Reflect(DeliteMultiArrayStringSplit(s,p,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayStringSplit(f(s),f(p),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArraySortIndices(l,v,c), u, es) => reflectMirrored(Reflect(DeliteMultiArraySortIndices(f(l),(f(v._1).asInstanceOf[Sym[Int]],f(v._2).asInstanceOf[Sym[Int]]),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    
    case Reflect(e@DeliteMultiArrayMapFilter(m,g,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayMapFilter(f(m),g,c)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayFlatMap(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayFlatMap(f(m),g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
 
    case Reflect(e@DeliteMatrixMultiply(l,r), u, es) => reflectMirrored(Reflect(DeliteMatrixMultiply(f(l),f(r))(e.mA,e.nA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMatrixVectorMultiply(m,v), u, es) => reflectMirrored(Reflect(DeliteMatrixVectorMultiply(f(m),f(v))(e.mA,e.nA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    //case Reflect(e@DeliteMultiArrayPin(m,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPin(f(m),l)(e.mA,e.mB), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    //case Reflect(e@DeliteMultiArrayUnpin(a,l,s), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPin(f(a),l,f(s))(e.mA,e.mB), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}