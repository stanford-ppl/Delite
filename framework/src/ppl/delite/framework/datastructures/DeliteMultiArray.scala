/*package ppl.delite.framework.datastructures
import scala.reflect.SourceContext

import ppl.delite.framework.ops._
import ppl.delite.framework.Util._

// Abstract, n-dimensional, multi-purpose array
// Intended for use at frontend w/ transformer to concrete data layouts
trait DeliteMultiArray[T]
trait DeliteArray1D[T] extends DeliteMultiArray[T]
trait DeliteArray2D[T] extends DeliteMultiArray[T]
trait DeliteArray3D[T] extends DeliteMultiArray[T]
trait DeliteArray4D[T] extends DeliteMultiArray[T]
trait DeliteArray5D[T] extends DeliteMultiArray[T]

// Abstract map intended for use with MultiArrays
trait DeliteMultiMap[K,V]

trait DeliteMultiArrayOps extends AbstractIndicesOps {
  object MultiArray {
    def apply[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext): Rep[DeliteMultiArray[T]] = dmultia_new[T](dims)
    def imm[T:Manifest](dims: Rep[Int]*)(implicit ctx: SourceContext): Rep[DeliteMultiArray[T]] = dmultia_new_immutable[T](dims)
    def fromFunction[T:Manifest](dims: Rep[Int]*)(func: Rep[LoopIndices] => Rep[T])(implicit ctx: SourceContext): Rep[DeliteMultiArray[T]] = dmultia_fromfunction(dims, func)
  }

  // Continuation of brief infix ops in DeliteMultiArray.scala
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

    // --- single element
    def apply(i: Rep[Int]*): Rep[T] = dmultia_apply(ma,indices_new(i.toList))
    def apply(i: Rep[AbstractIndices]): Rep[T] = dmultia_apply(ma,i)

    // --- Reshaping
    def reshape(d0: Rep[Int]): Rep[DeliteArray1D[T]] = dmultia_reshape(ma, Seq(d0)).as1D
    def reshape(d0: Rep[Int], d1: Rep[Int]): Rep[DeliteArray2D[T]] = dmultia_reshape(ma, Seq(d0, d1)).as2D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int]): Rep[DeliteArray3D[T]] = dmultia_reshape(ma, Seq(d0, d1, d2)).as3D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int]): Rep[DeliteArray4D[T]] = dmultia_reshape(ma, Seq(d0, d1, d2, d3)).as4D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int]): Rep[DeliteArray5D[T]] = dmultia_reshape(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def reshape(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int], d5: Rep[Int]*): Rep[DeliteMultiArray[T]] = dmultia_reshape(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    def reshapeView(d0: Rep[Int]): Rep[DeliteArray1D[T]] = dmultia_reshape_view(ma, Seq(d0)).as1D
    def reshapeView(d0: Rep[Int], d1: Rep[Int]): Rep[DeliteArray2D[T]] = dmultia_reshape_view(ma, Seq(d0, d1)).as2D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int]): Rep[DeliteArray3D[T]] = dmultia_reshape_view(ma, Seq(d0, d1, d2)).as3D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int]): Rep[DeliteArray4D[T]] = dmultia_reshape_view(ma, Seq(d0, d1, d2, d3)).as4D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int]): Rep[DeliteArray5D[T]] = dmultia_reshape_view(ma, Seq(d0, d1, d2, d3, d4)).as5D
    def reshapeView(d0: Rep[Int], d1: Rep[Int], d2: Rep[Int], d3: Rep[Int], d4: Rep[Int], d5: Rep[Int]*): Rep[DeliteMultiArray[T]] = dmultia_reshape_view(ma, Seq(d0, d1, d2, d3, d4) ++ d5.toSeq)

    // --- single element
    def update(i: List[Rep[Int]], x: Rep[T]): Rep[Unit] = dmultia_update(ma,indices_new(i),x)
    def update(i: Rep[AbstractIndices], x: Rep[T]): Rep[Unit] = dmultia_update(ma,i,x)

    // --- parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteMultiArray[B]] = dmultia_map(ma,f)
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteMultiArray[R]] = dmultia_zipwith(ma,y,f)
    def fold(zero: Rep[T])(f: (Rep[T],Rep[T]) => Rep[T]): Rep[T] = dmultia_fold(ma,f,zero)
    def reduce(zero: Rep[T])(f: (Rep[T], Rep[T]) => Rep[T]): Rep[T] = dmultia_reduce(ma,f,zero)
    def foreach(f: Rep[T] => Rep[Unit]): Rep[Unit] = dmultia_foreach(ma,f)
    def forIndices(f: Rep[LoopIndices] => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma,f)
    def groupBy[K:Manifest,V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V]): Rep[DeliteMultiMap[K,DeliteArray1D[V]]] = dmultia_groupBy(ma,key,value)
    def groupByReduce[K:Manifest, V:Manifest](key: Rep[T] => Rep[K], value: Rep[T] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V]): Rep[DeliteMultiMap[K,V]] = dmultia_groupByReduce(ma,key,value,reduce)

    def mmap(f: Rep[T] => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i))}
    def mzip[B:Manifest](y: Rep[DeliteMultiArray[B]])(f: (Rep[T],Rep[B]) => Rep[T]): Rep[Unit] = this.forIndices{i => ma(i) = f(ma(i),y(i))}

    def mkString(dels: Rep[String]*): Rep[String] = dmultia_mkstring(ma,dels,None)
    def mkStringFunc(dels: Rep[String]*)(func: Rep[T] => Rep[String]): Rep[String] = dmultia_mkstring(ma,dels,Some(func))
  }

  object Array1D {
    def apply[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[T]]
      = dmultia_new[T](List(len)).as1D
    def imm[T:Manifest](len: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[T]]
      = dmultia_new_immutable[T](List(len)).as1D
    def fromFunction[T:Manifest](len: Rep[Int])(func: Rep[Int] => Rep[T])(implicit ctx: SourceContext): Rep[DeliteArray1D[T]]
      = dmultia_fromfunction(List(len), {x: Rep[LoopIndices] => func(x.flat)}).as1D
    def sortIndices(length: Rep[Int])(comparator: (Rep[Int], Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[Int]]
      = dmultia_sortIndices(length, comparator)
    def splitString(str: Rep[String],pat: Rep[String],ofs: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[DeliteArray1D[String]]
      = dmultia_string_split(str,pat,ofs)
    def fromFile[T:Manifest](path: Rep[String])(func: Rep[String] => Rep[T])(implicit ctx: SourceContext): Rep[DeliteArray1D[T]]
      = dmultia_readfile(path, Seq(unit("\n")), func).as1D
  }

  implicit def repArray1DtoArray1DOps[T:Manifest](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) = new DeliteArray1DOpsCls(ma)
  class DeliteArray1DOpsCls[T:Manifest](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def length: Rep[Int] = dmultia_size(ma)

    // --- copies
    def mutable: Rep[DeliteArray1D[T]] = dmultia_mutable(ma).as1D
    def immutable: Rep[DeliteArray1D[T]] = dmultia_immutable(ma).as1D
    def permute(d0: Int): Rep[DeliteArray1D[T]] = dmultia_permute(ma, Seq(d0)).as1D  // not really needed - just a copy

    // --- slicing
    // NOTE: len here is the length of the array view (including stride). should be calculated as floor((prev_len - start)/stride)
    def slice(start: Rep[Int], stride: Rep[Int], len: Rep[Int]): Rep[DeliteArray1D[T]] = dmultia_view(ma, Seq(start), Seq(stride), Seq(len)).as1D
    def slice(start: Rep[Int], len: Rep[Int]): Rep[DeliteArray1D[T]] = dmultia_view(ma, Seq(start), Seq(unit(1)), Seq(len)).as1D

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
    def flatMap[B:Manifest](f: Rep[T] => Rep[DeliteArray1D[B]])(implicit ctx: SourceContext): Rep[DeliteArray1D[B]] = dmultia_flatmap(ma,f)

    def mkString(del: Rep[String] = unit(",")): Rep[String] = dmultia_mkstring(ma,Seq(del),None)
    def writeFile(path: Rep[String])(func: Rep[T] => Rep[String])(implicit ctx: SourceContext): Rep[Unit] = dmultia_writefile(ma, Seq(unit("\n")), path, func)
  }

  implicit def repArray1DToOrderedArray1DOpsCls[T:Manifest:Ordering](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) = new DeliteArray1DOrderedOpsCls(ma)
  class DeliteArray1DOrderedOpsCls[T:Manifest:Ordering](ma: Rep[DeliteArray1D[T]])(implicit ctx: SourceContext) {
    def sort: Rep[DeliteArray1D[T]] = dmultia_sort(ma)
  }

  object Array2D {
    def apply[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray2D[T]]
      = dmultia_new[T](List(rows,cols)).as2D
    def imm[T:Manifest](rows: Rep[Int], cols: Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray2D[T]]
      = dmultia_new_immutable[T](List(rows,cols)).as2D
    def fromFunction[T:Manifest](rows: Rep[Int], cols: Rep[Int])(func: (Rep[Int], Rep[Int]) => Rep[T])(implicit ctx: SourceContext): Rep[DeliteArray2D[T]]
      = dmultia_fromfunction(List(rows,cols), {x: Rep[LoopIndices] => func(x(0),x(1))}).as2D

    def fromFile[T:Manifest](path: Rep[String], cdel: Rep[String] = unit("\\s+"))(func: Rep[String] => Rep[T])(implicit ctx: SourceContext): Rep[DeliteArray2D[T]] = {
      // FIXME: Not using the flat array implementation right now because flatmap fusion is broken
      // dmultia_readfile(path, Seq(unit("\n"), cdel), func).as2D

      val vec = Array1D.fromFile(path){s => Array1D.splitString(s, cdel).map(func) } // array of arrays
      Array2D.fromFunction(vec.length, vec(unit(0)).length){(i,j) => vec(i).apply(j) }
    }
  }

  implicit def repArray2DToArray2DOps[T:Manifest](ma: Rep[DeliteArray2D[T]])(implicit ctx: SourceContext) = new DeliteArray2DOpsCls(ma)
  class DeliteArray2DOpsCls[T:Manifest](ma: Rep[DeliteArray2D[T]])(implicit ctx: SourceContext) {
    // --- properties
    def nRows: Rep[Int] = dmultia_dim(ma, 0)
    def nCols: Rep[Int] = dmultia_dim(ma, 1)

    // --- copies
    def mutable: Rep[DeliteArray2D[T]] = dmultia_mutable(ma).as2D
    def immutable: Rep[DeliteArray2D[T]] = dmultia_immutable(ma).as2D

    // --- Permuting
    def t: Rep[DeliteArray2D[T]] = dmultia_permute(ma, Seq(1,0)).as2D
    def permute(d0: Int, d1: Int): Rep[DeliteArray2D[T]] = dmultia_permute(ma, Seq(d0, d1)).as2D // not really needed - special cases are matrix transpose, matrix copy

    // --- Slicing
    def slice(startRow: Rep[Int], rowStride: Rep[Int], nrows: Rep[Int], startCol: Rep[Int], colStride: Rep[Int], ncols: Rep[Int]): Rep[DeliteArray2D[T]]
      = dmultia_view(ma, Seq(startRow, startCol), Seq(rowStride, colStride), Seq(nrows, ncols)).as2D
    def slice(startRow: Rep[Int], nrows: Rep[Int], startCol: Rep[Int], ncols: Rep[Int]): Rep[DeliteArray2D[T]]
      = dmultia_view(ma, Seq(startRow, startCol), Seq(unit(1), unit(1)), Seq(nrows, ncols)).as2D
    def sliceRow(start: Rep[Int]): Rep[DeliteArray1D[T]]
      = dmultia_view(ma, Seq(start, unit(0)), Seq(unit(1)), Seq(nCols), Seq(0)).as1D  // Getting multiple cols, drops 1st dimension
    def sliceCol(start: Rep[Int]): Rep[DeliteArray1D[T]]
      = dmultia_view(ma, Seq(unit(0), start), Seq(unit(1)), Seq(nRows), Seq(1)).as1D  // Getting multiple rows, drops 2nd dimension

    def sliceRows(start: Rep[Int], num: Rep[Int]): Rep[DeliteArray2D[T]] = this.slice(start, unit(1), num, unit(0), unit(1), nCols).as2D
    def sliceCols(start: Rep[Int], num: Rep[Int]): Rep[DeliteArray2D[T]] = this.slice(unit(0), unit(1), nRows, start, unit(1), num).as2D

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

    def removeRows(start: Rep[Int], len: Rep[Int]): Rep[Unit] = dmultia_remove(ma, 0, start, len)
    def removeCols(start: Rep[Int], len: Rep[Int]): Rep[Unit] = dmultia_remove(ma, 1, start, len)
    def removeRow(i: Rep[Int]): Rep[Unit] = this.removeRows(i, unit(1))
    def removeCol(j: Rep[Int]): Rep[Unit] = this.removeCols(j, unit(1))

    // --- 2D parallel ops
    def map[B:Manifest](f: Rep[T] => Rep[B]): Rep[DeliteArray2D[B]] = dmultia_map(ma,f).as2D
    def zip[B:Manifest,R:Manifest](y: Rep[DeliteArray2D[B]])(f: (Rep[T],Rep[B]) => Rep[R]): Rep[DeliteArray2D[R]] = dmultia_zipwith(ma,y,f).as2D

    def forIndices(f: (Rep[Int], Rep[Int]) => Rep[Unit]): Rep[Unit] = dmultia_forindices(ma, {i: Rep[LoopIndices] => f(i(0), i(1)) })
    def mapRows[R:Manifest](f: Rep[DeliteArray1D[T]] => Rep[DeliteArray1D[R]]): Rep[DeliteArray2D[R]]
      = dmultia_NDmap(ma,Seq(0),{r: Rep[DeliteMultiArray[T]] => f(r.as1D) }).as2D
    def mapCols[R:Manifest](f: Rep[DeliteArray1D[T]] => Rep[DeliteArray1D[R]]): Rep[DeliteArray2D[R]]
      = dmultia_NDmap(ma,Seq(1),{c: Rep[DeliteMultiArray[T]] => f(c.as1D) }).as2D

    def mkString(rowDel: Rep[String] = unit("\n"), colDel: Rep[String] = unit(",")): Rep[String]
      = dmultia_mkstring(ma,Seq(rowDel,colDel),None)
    def writeFile(path: Rep[String], cdel: Rep[String] = unit("    "))(func: Rep[T] => Rep[String])(implicit ctx: SourceContext): Rep[Unit]
      = dmultia_writefile(ma, Seq(unit("\n"),cdel), path, func)
  }


  // --- HashMap stuff
  def dmulmap_size[K:Manifest,V:Manifest](dm: Rep[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Rep[Int]
  def dmulmap_get[K:Manifest,V:Manifest](dm: Rep[DeliteMultiMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[V]
  def dmulmap_contains[K:Manifest,V:Manifest](dm: Rep[DeliteMultiMap[K,V]], key: Rep[K])(implicit ctx: SourceContext): Rep[Boolean]
  def dmulmap_keys[K:Manifest,V:Manifest](dm: Rep[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray1D[K]]
  def dmulmap_values[K:Manifest,V:Manifest](dm: Rep[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Rep[DeliteArray1D[V]]
  def dmulmap_from_1d_arrays[K:Manifest,V:Manifest](keys: Rep[DeliteArray1D[K]], vals: Rep[DeliteArray1D[V]])(implicit ctx: SourceContext): Rep[DeliteMultiMap[K,V]]

  // --- Rank type casts
  def dmultia_as_1D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray1D[T]]
  def dmultia_as_2D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray2D[T]]
  def dmultia_as_3D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray3D[T]]
  def dmultia_as_4D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray4D[T]]
  def dmultia_as_5D[T:Manifest](ma: Rep[DeliteMultiArray[T]]): Rep[DeliteArray5D[T]]

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
  def dmultia_permute_view[A:Manifest](ma: Rep[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_reshape_view[A:Manifest](ma: Rep[DeliteMultiArray[A]], shape: Seq[Rep[Int]], UNSAFE: Boolean = false)(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]

  // --- Parallel Ops
  def dmultia_fromfunction[A:Manifest](dims: Seq[Rep[Int]], f: Rep[LoopIndices] => Rep[A])(implicit ctx: SourceContext): Rep[DeliteMultiArray[A]]
  def dmultia_map[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]
  def dmultia_zipwith[A:Manifest,B:Manifest,R:Manifest](ma: Rep[DeliteMultiArray[A]], mb: Rep[DeliteMultiArray[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[DeliteMultiArray[R]]
  def dmultia_reduce[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def dmultia_fold[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: (Rep[A],Rep[A]) => Rep[A], zero: Rep[A])(implicit ctx: SourceContext): Rep[A]
  def dmultia_foreach[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_forindices[A:Manifest](ma: Rep[DeliteMultiArray[A]], f: Rep[LoopIndices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_forshapeindices(shape: Seq[Rep[Int]], f: Rep[LoopIndices] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def dmultia_NDmap[A:Manifest,B:Manifest](ma: Rep[DeliteMultiArray[A]], mdims: Seq[Int], func: Rep[DeliteMultiArray[A]] => Rep[DeliteMultiArray[B]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[B]]

  def dmultia_groupBy[A:Manifest,K:Manifest,V:Manifest](ma: Rep[DeliteMultiArray[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMultiMap[K,DeliteArray1D[V]]]
  def dmultia_groupByReduce[A:Manifest,K:Manifest,V:Manifest](ma: Rep[DeliteMultiArray[A]], key: Rep[A] => Rep[K], value: Rep[A] => Rep[V], reduce: (Rep[V],Rep[V]) => Rep[V])(implicit ctx: SourceContext): Rep[DeliteMultiMap[K,V]]

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
  def dmultia_mkstring[A:Manifest](ma: Rep[DeliteMultiArray[A]], dels: Seq[Rep[String]], func: Option[Rep[A] => Rep[String]])(implicit ctx: SourceContext): Rep[String]

  // --- 1D Ops
  def dmultia_sortIndices(len: Rep[Int], comp: (Rep[Int],Rep[Int]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[Int]]
  def dmultia_sort[A:Manifest:Ordering](ma: Rep[DeliteArray1D[A]])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]
  def dmultia_sortWith[A:Manifest](ma: Rep[DeliteArray1D[A]], func: (Rep[A], Rep[A]) => Rep[Int])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]
  def dmultia_string_split(str: Rep[String],pat: Rep[String],lim: Rep[Int] = unit(0))(implicit ctx: SourceContext): Rep[DeliteArray1D[String]]

  // --- 2D Ops
  def dmultia_matmult[A:Manifest](lhs: Rep[DeliteArray2D[A]], rhs: Rep[DeliteArray2D[A]], default: (Rep[DeliteArray2D[A]], Rep[DeliteArray2D[A]]) => Rep[DeliteArray2D[A]])(implicit ctx: SourceContext): Rep[DeliteArray2D[A]]
  def dmultia_matvecmult[A:Manifest](mat: Rep[DeliteArray2D[A]], vec: Rep[DeliteArray1D[A]], default: (Rep[DeliteArray2D[A]], Rep[DeliteArray1D[A]]) => Rep[DeliteArray1D[A]])(implicit ctx: SourceContext): Rep[DeliteArray1D[A]]

  // --- Pinning
  //def dmultia_pin[T:Manifest,R:Manifest](ma: Rep[DeliteMultiArray[T]], layout: Layout[T,R])(implicit ctx: SourceContext): Rep[DeliteArray[R]]
  //def dmultia_unpin[T:Manifest,R:Manifest](in: Rep[DeliteArray[R]], layout: Layout[T,R], shape: Seq[Rep[Int]])(implicit ctx: SourceContext): Rep[DeliteMultiArray[T]]
}

trait DeliteMultiArrayOpsExp extends DeliteMultiArrayOps with DeliteFigmentOpsExp { this: DeliteOpsExp =>

  // --- HashMap nodes
  case class DeliteMultiMapSize[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext) extends Fig3[K,V,Int]
  case class DeliteMultiMapGet[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]], key: Exp[K])(implicit ctx: SourceContext) extends Fig3[K,V,V]
  case class DeliteMultiMapContains[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]], key: Exp[K])(implicit ctx: SourceContext) extends Fig3[K,V,Boolean]
  case class DeliteMultiMapKeys[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext) extends Fig3[K,V,DeliteArray1D[K]]
  case class DeliteMultiMapVals[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext) extends Fig3[K,V,DeliteArray1D[V]]
  case class DeliteMultiMapFromArrays[K:Manifest,V:Manifest](keys: Exp[DeliteArray1D[K]], vals: Exp[DeliteArray1D[V]])(implicit ctx: SourceContext) extends Fig3[K,V,DeliteMultiMap[K,V]]

  def dmulmap_size[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Exp[Int] = DeliteMultiMapSize(dm)
  def dmulmap_get[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]], key: Exp[K])(implicit ctx: SourceContext): Exp[V] = DeliteMultiMapGet(dm,key)
  def dmulmap_contains[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]], key: Exp[K])(implicit ctx: SourceContext): Exp[Boolean] = DeliteMultiMapContains(dm,key)
  def dmulmap_keys[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Exp[DeliteArray1D[K]] = DeliteMultiMapKeys(dm)
  def dmulmap_values[K:Manifest,V:Manifest](dm: Exp[DeliteMultiMap[K,V]])(implicit ctx: SourceContext): Exp[DeliteArray1D[V]] = DeliteMultiMapVals(dm)
  def dmulmap_from_1d_arrays[K:Manifest,V:Manifest](keys: Exp[DeliteArray1D[K]], vals: Exp[DeliteArray1D[V]])(implicit ctx: SourceContext): Exp[DeliteMultiMap[K,V]] = DeliteMultiMapFromArrays(keys,vals)

  override def unapplyStructType[T:Manifest]: Option[(StructTag[T], List[(String,Manifest[_])])] = manifest[T] match {
    case t if t.erasure == classOf[DeliteMultiMap[_,_]] => Some((classTag(t), List("keys" -> darray1dManifest(t.typeArguments(0)), "vals" -> darray1dManifest(t.typeArguments(1)), "size" -> manifest[Int])))
    case _ => super.unapplyStructType
  }

  /////////////////////
  // IR Figments

  // --- Array constructors
  case class DeliteMultiArrayNew[A:Manifest](dims: Seq[Exp[Int]]) extends Fig2[A,DeliteMultiArray[A]]
  case class DeliteMultiArrayView[A:Manifest](ma: Exp[DeliteMultiArray[A]], start: Seq[Exp[Int]], stride: Seq[Exp[Int]], dims: Seq[Exp[Int]], unitDims: Seq[Int])(implicit ctx: SourceContext) extends Fig2[A,DeliteMultiArray[A]]

  // --- Array properties
  case class DeliteMultiArrayRank[A:Manifest](ma: Exp[DeliteMultiArray[A]]) extends Fig2[A,Int]
  case class DeliteMultiArrayDim[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Int) extends Fig2[A, Int]
  case class DeliteMultiArraySize[A:Manifest](ma: Exp[DeliteMultiArray[A]]) extends Fig2[A,Int]

  // --- Array single element ops
  case class DeliteMultiArrayApply[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices]) extends Fig[A]
  case class DeliteMultiArrayUpdate[A:Manifest](ma: Exp[DeliteMultiArray[A]], i: Exp[AbstractIndices], x: Exp[A])(implicit ctx: SourceContext) extends AtomicWriteFig[A] {
    def externalFields = List(i, x)
  }

  // --- Array permute / reshaping
  case class DeliteMultiArrayPermute[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) extends Fig2[A,DeliteMultiArray[A]]
  case class DeliteMultiArrayReshape[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]])(implicit ctx: SourceContext) extends Fig2[A,DeliteMultiArray[A]]

  case class DeliteMultiArrayPermuteView[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) extends Fig2[A,DeliteMultiArray[A]]
  case class DeliteMultiArrayReshapeView[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], UNSAFE: Boolean)(implicit ctx: SourceContext) extends Fig2[A,DeliteMultiArray[A]]

  // --- Parallel Ops
  /**
   * Read multidimensional array from file
   * Output MultiArray's rank is equivalent to the number of delimeters given
   * @param path - relative (?) path to file
   * @param dels - file delimeters corresponding to dimensions. first is expected to be newline.
   * @param func - mapping function from input element string to element
   */
  case class DeliteMultiArrayReadFile[A:Manifest](path: Exp[String], dels: Seq[Exp[String]], func: Exp[String] => Exp[A])(implicit ctx: SourceContext) extends NestedLoopFig2[A,DeliteMultiArray[A]] {
    type OpType <: DeliteMultiArrayReadFile[A]
    lazy val rV: Sym[String] = copyOrElse(_.rV)(fresh[String])
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(rV)))

    override def toString = s"DeliteMultiArrayReadFile($path, dels: " + dels.mkString(",") + s", $rV => $body)"
  }

  /**
   * Parallel creation of a MD array
   * @param dims - a list of dimensions for the array (length is equal to array rank)
   * @param func - function from multidimensional index (e.g. (d1, d2, ... dn)) to element
   */
  case class DeliteMultiArrayFromFunction[A:Manifest](dims: Seq[Exp[Int]], func: Exp[LoopIndices] => Exp[A])(implicit ctx: SourceContext) extends NestedLoopFig2[A,DeliteMultiArray[A]] {
    type OpType <: DeliteMultiArrayFromFunction[A]
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(i)))

    override def toString = "DeliteMultiArrayFromFunction(" + dims.mkString(",") + ", " + body + ")"
  }

  /**
   * Parallel map over all elements in MD array
   * @param in   - input multi-array
   * @param func - lambda mapping function
   */
  case class DeliteMultiArrayMap[A:Manifest,R:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[R])(implicit ctx: SourceContext) extends NestedLoopFig3[A,R,DeliteMultiArray[R]] {
    type OpType <: DeliteMultiArrayMap[A,R]
    val body: Block[R] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(in,i))))

    override def toString = s"DeliteMultiArrayMap($in, $body)"
  }

  /**
   * Parallel zip over two MD arrays
   * Multidimensional arrays must be of the same rank, and are assumed to be of the same dimension(s)
   * @param inA  - first input multi-array
   * @param inB  - second input multi-array
   * @param func - zipWith function
   */
  case class DeliteMultiArrayZipWith[A:Manifest,B:Manifest,R:Manifest](inA: Exp[DeliteMultiArray[A]], inB: Exp[DeliteMultiArray[B]], func: (Exp[A],Exp[B]) => Exp[R])(implicit ctx: SourceContext) extends NestedLoopFig4[A,B,R,DeliteMultiArray[R]] {
    type OpType <: DeliteMultiArrayZipWith[A,B,R]
    val body: Block[R] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(inA,i), dmultia_apply(inB,i))))

    override def toString = s"DeliteMultiArrayZipWith($inA, $inB, $body)"
  }

  /**
   * Parallel reduction of an MD array (has no zero value)
   * @param in   - input multi-array
   * @param func - reduction function (must be associative)
   */
  case class DeliteMultiArrayReduce[A:Manifest](in: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) extends NestedLoopFig[A] {
    type OpType <: DeliteMultiArrayReduce[A]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)((fresh[A], fresh[A]))
    val lookup: Block[A] = copyTransformedBlockOrElse(_.lookup)(reifyEffects(dmultia_apply(in, i)))
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(rV._1, rV._2)))

    override def toString = s"DeliteMultiArrayReduce($in, $zero, $lookup, $rV => $body)"
  }
  /**
   * Parallel fold of an MD array (has initial zero value)
   * @param in   - input multi-array
   * @param func - reduction function (must be associative)
   * @param zero - "empty" value
   */
  case class DeliteMultiArrayFold[A:Manifest](in: Exp[DeliteMultiArray[A]], func: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) extends NestedLoopFig[A] {
    type OpType <: DeliteMultiArrayFold[A]
    lazy val rV: (Sym[A],Sym[A]) = copyOrElse(_.rV)((fresh[A], fresh[A]))
    val lookup: Block[A] = copyTransformedBlockOrElse(_.lookup)(reifyEffects(dmultia_apply(in, i)))
    val body: Block[A] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(rV._1, rV._2)))

    override def toString = s"DeliteMultiArrayFold($in, $zero, $lookup, $rV => $body)"
  }

  /**
   * Effectful parallel foreach over elements of a MD array
   * @param in   - input multi-array
   * @param func - foreach function
   */
  case class DeliteMultiArrayForeach[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) extends NestedLoopFig2[A,Unit] {
    type OpType <: DeliteMultiArrayForeach[A]
    val body: Block[Unit] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(in,i))))

    override def toString = s"DeliteMultiArrayForeach($in, $body)"
  }

  /**
   * Effectful parallel loop over indices in MD array's dimension(s)
   * Note: This is not implemented as ForShapeIndices of the array's shape because the
   * shape may not entirely be known before rank analysis runs
   * @param in   - input multi-array
   * @param func - function on set of indices
   */
  case class DeliteMultiArrayForIndices[A:Manifest](in: Exp[DeliteMultiArray[A]], func: Exp[LoopIndices] => Exp[Unit])(implicit ctx: SourceContext) extends NestedLoopFig2[A,Unit] {
    type OpType <: DeliteMultiArrayForIndices[A]
    val body: Block[Unit] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(i)))

    override def toString = s"DeliteMultiArrayForIndices($in, $body)"
  }

  /**
   * Effectful loop over multidimensional space
   * @param shape - dimensions of space
   * @param func  - function on set of indices
   */
  case class DeliteMultiArrayForShapeIndices(shape: Seq[Exp[Int]], func: Exp[LoopIndices] => Exp[Unit])(implicit ctx: SourceContext) extends NestedLoopFig[Unit] {
    type OpType <: DeliteMultiArrayForShapeIndices
    val body: Block[Unit] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(i)))

    override def toString = "DeliteMultiArrayForShapeIndices(" + shape.mkString(",") + ", " + body + ")"
  }

  /**
   * Parallel N-Dimensional Flat Map on MD array
   * @param in    - input multi-array
   * @param mdims - map dimensions
   * @param func  - flat map function
   */
  case class DeliteMultiArrayNDMap[A:Manifest,B:Manifest](in: Exp[DeliteMultiArray[A]], mdims: Seq[Int], func: Exp[DeliteMultiArray[A]] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) extends NestedLoopFig3[A,B,DeliteMultiArray[B]] {
    type OpType <: DeliteMultiArrayNDMap[A,B]
    lazy val rV: Sym[DeliteMultiArray[A]] = copyTransformedOrElse(_.rV)(fresh[DeliteMultiArray[A]]).asInstanceOf[Sym[DeliteMultiArray[A]]]
    val body: Block[DeliteMultiArray[B]] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(rV)))

    override def toString = s"DeliteMultiArrayNDMap($in, " + mdims.mkString(",") + s"$rV => $body)"
  }

  /**
   * Parallel Group-By on MD array
   * @param in  - input multi-array
   * @param key - key (hash) function
   */
  case class DeliteMultiArrayGroupBy[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V])(implicit ctx: SourceContext) extends NestedLoopFig4[A,K,V,DeliteMultiMap[K,DeliteArray1D[V]]] {
    type OpType <: DeliteMultiArrayGroupBy[A,K,V]
    val keyFunc: Block[K] = copyTransformedBlockOrElse(_.keyFunc)(reifyEffects(key(dmultia_apply(in,i))))
    val valFunc: Block[V] = copyTransformedBlockOrElse(_.valFunc)(reifyEffects(value(dmultia_apply(in,i))))

    override def toString = s"DeliteMultiArrayGroupBy($in, $keyFunc, $valFunc)"
  }

  /**
   * Parallel Group-By-Reduce on MD array
   * @param in     - input multi-array
   * @param key    - key (hash) function
   * @param value  - mapping function from elements of in to HashMap values
   * @param reduce - reduction on values (with same key)
   */
  case class DeliteMultiArrayGroupByReduce[A:Manifest,K:Manifest,V:Manifest](in: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K], value: Exp[A] => Exp[V], reduce: (Exp[V],Exp[V]) => Exp[V])(implicit ctx: SourceContext) extends NestedLoopFig4[A,K,V,DeliteMultiMap[K,V]] {
    type OpType <: DeliteMultiArrayGroupByReduce[A,K,V]
    lazy val rV: (Sym[V],Sym[V]) = copyOrElse(_.rV)((fresh[V],fresh[V]))
    val keyFunc: Block[K] = copyTransformedBlockOrElse(_.keyFunc)(reifyEffects(key(dmultia_apply(in,i))))
    val valFunc: Block[V] = copyTransformedBlockOrElse(_.valFunc)(reifyEffects(value(dmultia_apply(in,i))))
    val redFunc: Block[V] = copyTransformedBlockOrElse(_.redFunc)(reifyEffects(reduce(rV._1,rV._2)))

    override def toString = s"DeliteMultiArrayGroupByReduce($in, $keyFunc, $valFunc, $rV => $redFunc)"
  }

  // --- 1D Parallel Ops
  /**
   * Parallel Map-Filter on a 1D array
   * @param in   - input array
   * @param func - mapping function
   * @param cond - filter function
   */
  case class DeliteMultiArrayMapFilter[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], func: Exp[A] => Exp[B], cond: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) extends NestedLoopFig3[A,B,DeliteArray1D[B]] {
    type OpType <: DeliteMultiArrayMapFilter[A,B]
    val mapFunc: Block[B] = copyTransformedBlockOrElse(_.mapFunc)(reifyEffects(func(dmultia_apply(in,i))))
    val filtFunc: Block[Boolean] = copyTransformedBlockOrElse(_.filtFunc)(reifyEffects(cond(dmultia_apply(in,i))))

    override def toString = s"DeliteMultiArrayMapFilter($in, $mapFunc, $filtFunc)"
  }

  /**
   * Parallel Flat Map on a 1D array
   * @param in   - input array
   * @param func - flat map function
   */
  case class DeliteMultiArrayFlatMap[A:Manifest,B:Manifest](in: Exp[DeliteArray1D[A]], func: Exp[A] => Exp[DeliteArray1D[B]])(implicit ctx: SourceContext) extends NestedLoopFig3[A,B,DeliteArray1D[B]] {
    type OpType <: DeliteMultiArrayFlatMap[A,B]
    val body: Block[DeliteArray1D[B]] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(in,i))))

    override def toString = s"DeliteMultiArrayFlatMap($in, $body)"
  }

  // --- Buffer operations
  /**
   * Single element insert into a 1D array at position 'index'
   * Elements at and above index are shifted to the right by one position
   * NOTE: Appends should use specialized function DeliteMultiArrayAppend
   * @param ma    - 1D array
   * @param x     - element to insert
   * @param index - position to insert element at
   */
  case class DeliteMultiArrayInsert[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A], index: Exp[Int])(implicit ctx: SourceContext) extends AtomicWriteFig[A] {
    def externalFields = List(index, x)
  }

  /**
   * Single element append into a 1D array
   * Special case of insert, corresponds to small optimization in ArrayBuffer
   * @param ma - 1D array
   * @param x  - element to append
   */
  case class DeliteMultiArrayAppend[A:Manifest](ma: Exp[DeliteArray1D[A]], x: Exp[A])(implicit ctx: SourceContext) extends AtomicWriteFig[A] {
    def externalFields = List(x)
  }

  /**
   * Insert ND array into a MD array at position 'index'
   * Elements at and above index are shifted to the right
   * N must be equal to either M or M - 1.
   * @param ma    - MD array (lhs)
   * @param rhs   - ND array (to insert)
   * @param axis  - dimension axis along which to insert
   * @param index - position to insert element at
   */
  case class DeliteMultiArrayInsertAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]], axis: Int, index: Exp[Int])(implicit ctx: SourceContext) extends AtomicWriteFig[A] {
    def externalFields = List(rhs, axis, index)
  }

  /**
   * Remove elements from a MD array
   * Data is rearranged to fill blanks. No trimming guarantees
   * @param ma    - MD array (lhs)
   * @param axis  - dimension axis along which to remove
   * @param start - position of first element / slice to remove
   * @param len   - number of elements / slices to remove
   */
  case class DeliteMultiArrayRemove[A:Manifest](ma: Exp[DeliteMultiArray[A]], axis: Int, start: Exp[Int], len: Exp[Int])(implicit ctx: SourceContext) extends AtomicWriteFig[A] {
    def externalFields = List(axis, start, len)
  }

  // --- Misc Ops.
  /**
   * Creation of a string representation of a MultiArray
   * Note that func is a restricted function - cannot have multiple paths or effects
   * @param ma   - input MultiArray
   * @param dels - delimeters across dimensions. Number of delimeters should equal rank of input MultiArray
   * @param func - optional function to convert elements to string (delite_stringify used by default)
   */
  case class DeliteMultiArrayMkString[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], func: Option[Exp[A] => Exp[String]])(implicit ctx: SourceContext) extends NestedLoopFig2[A,String] {
    type OpType <: DeliteMultiArrayMkString[A]
    lazy val rV: Sym[A] = copyTransformedOrElse(_.rV)(fresh[A]).asInstanceOf[Sym[A]]
    val body: Option[Block[String]] = func match {
      case Some(f) => Some(copyTransformedBlockOrElse(_.body.get)(reifyEffects(f(rV))))
      case _ => None
    }

    override def toString = s"DeliteMultiArrayMkString($ma, dels: " + dels.mkString(",") + s", $rV => $body)"
  }

  /**
   * Abstract node for writing a MD array to a file
   * @param in   - input MD array
   * @param dels - delimeters across dimensions. Number of delimeters should equal rank of input MultiArray
   * @param path - relative path (?) to output file
   * @param func - mapping from element of array to string
   */
  case class DeliteMultiArrayWriteFile[A:Manifest](in: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], path: Exp[String], func: Exp[A] => Exp[String])(implicit ctx: SourceContext) extends NestedLoopFig2[A,Unit] {
    type OpType <: DeliteMultiArrayWriteFile[A]
    val body: Block[String] = copyTransformedBlockOrElse(_.body)(reifyEffects(func(dmultia_apply(in,i))))

    override def toString = s"DeliteMultiArrayWriteFile($in, dels: " + dels.mkString(",") + s", $path, $body)"
  }

  // --- 1D Ops
  /**
   * General sort function using Java comparator interface
   * From Java doc: Comparator "returns a negative integer, zero, or a positive integer as the
   * first argument is less than, equal to, or greater than the second."
   * @param len        - length of array to be created
   * @param comparator - comparator function on indices
   */
  case class DeliteMultiArraySortIndices(len: Exp[Int], i: (Sym[Int], Sym[Int]), body: Block[Int]) extends Fig[DeliteArray1D[Int]]

  /**
   * Splits a given string by delimeters, creating a maximum of 'lim' elements
   * Results of split are returned as a 1D array
   * @param str   - input string
   * @param split - string pattern to split on
   * @param lim   - maximum number of elements (or 0 or less for no limit)
   */
  case class DeliteMultiArrayStringSplit(str: Exp[String], split: Exp[String], lim: Exp[Int]) extends Fig[DeliteArray1D[String]]

  // --- 2D Ops
  case class DeliteMatrixMultiply[A:Manifest](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]], default: (Exp[DeliteArray2D[A]], Exp[DeliteArray2D[A]]) => Exp[DeliteArray2D[A]])(implicit ctx: SourceContext) extends OpLoopFig2[A,DeliteArray2D[A]] {
    type OpType <: DeliteMatrixMultiply[A]
    lazy val rM1: Sym[DeliteArray2D[A]] = copyOrElse(_.rM1)(fresh[DeliteArray2D[A]])
    lazy val rM2: Sym[DeliteArray2D[A]] = copyOrElse(_.rM2)(fresh[DeliteArray2D[A]])
    val defFunc: Block[DeliteArray2D[A]] = copyTransformedBlockOrElse(_.defFunc)(reifyEffects(default(rM1, rM2)))
  }
  case class DeliteMatrixVectorMultiply[A:Manifest](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]],  default: (Exp[DeliteArray2D[A]], Exp[DeliteArray1D[A]]) => Exp[DeliteArray1D[A]])(implicit ctx: SourceContext) extends OpLoopFig2[A,DeliteArray1D[A]] {
    type OpType <: DeliteMatrixVectorMultiply[A]
    lazy val rM: Sym[DeliteArray2D[A]] = copyOrElse(_.rM)(fresh[DeliteArray2D[A]])
    lazy val rV: Sym[DeliteArray1D[A]] = copyOrElse(_.rV)(fresh[DeliteArray1D[A]])
    val defFunc: Block[DeliteArray1D[A]] = copyTransformedBlockOrElse(_.defFunc)(reifyEffects(default(rM, rV)))
  }

  // --- Atomic Write (MultiArray)
  case class MultiArrayTracer(i: Exp[AbstractIndices]) extends AtomicTracerFig

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
  def dmultia_permute_view[A:Manifest](ma: Exp[DeliteMultiArray[A]], config: Seq[Int])(implicit ctx: SourceContext) = DeliteMultiArrayPermuteView(ma,config)
  def dmultia_reshape_view[A:Manifest](ma: Exp[DeliteMultiArray[A]], dims: Seq[Exp[Int]], UNSAFE: Boolean = false)(implicit ctx: SourceContext) = DeliteMultiArrayReshapeView(ma,dims,UNSAFE)

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
  def dmultia_fold[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: (Exp[A],Exp[A]) => Exp[A], zero: Exp[A])(implicit ctx: SourceContext) = DeliteMultiArrayFold(ma,f,zero)
  def dmultia_forindices[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[LoopIndices] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForIndices[A](ma,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }
  def dmultia_foreach[A:Manifest](ma: Exp[DeliteMultiArray[A]], f: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForeach(ma,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }
  def dmultia_forshapeindices(shape: Seq[Exp[Int]], f: Exp[LoopIndices] => Exp[Unit])(implicit ctx: SourceContext) = {
    val df = DeliteMultiArrayForShapeIndices(shape,f)
    reflectEffect(df, summarizeEffects(df.body).star andAlso Simple())
  }

  def dmultia_NDmap[A:Manifest,B:Manifest](ma: Exp[DeliteMultiArray[A]], mdims: Seq[Int], func: Exp[DeliteMultiArray[A]] => Exp[DeliteMultiArray[B]])(implicit ctx: SourceContext) = DeliteMultiArrayNDMap(ma,mdims,func)
  def dmultia_groupBy[A:Manifest,K:Manifest,V:Manifest](ma: Exp[DeliteMultiArray[A]], key: Exp[A] => Exp[K],value: Exp[A] => Exp[V])(implicit ctx: SourceContext) = DeliteMultiArrayGroupBy(ma,key,value)
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

  def dmultia_appendAll[A:Manifest](ma: Exp[DeliteMultiArray[A]], rhs: Exp[DeliteMultiArray[A]], axis: Int)(implicit ctx: SourceContext) = dmultia_insertAll(ma, rhs, axis, dmultia_dim(ma, axis))

  // --- Misc.
  def dmultia_mkstring[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], func: Option[Exp[A] => Exp[String]])(implicit ctx: SourceContext) = DeliteMultiArrayMkString(ma,dels,func)
  def dmultia_readfile[A:Manifest](path: Exp[String], dels: Seq[Exp[String]], func: Exp[String] => Exp[A])(implicit ctx: SourceContext): Exp[DeliteMultiArray[A]] = DeliteMultiArrayReadFile(path, dels, func)
  def dmultia_writefile[A:Manifest](ma: Exp[DeliteMultiArray[A]], dels: Seq[Exp[String]], path: Exp[String], func: Exp[A] => Exp[String])(implicit ctx: SourceContext): Exp[Unit] = reflectEffect(DeliteMultiArrayWriteFile(ma, dels, path, func))

  // --- 1D Operations
  def dmultia_sortIndices(len: Exp[Int], comp: (Exp[Int],Exp[Int]) => Exp[Int])(implicit ctx: SourceContext) = {
    val i = (fresh[Int],fresh[Int])
    DeliteMultiArraySortIndices(len, i, reifyEffects(comp(i._1,i._2)) )
  }

  // TODO: both of these implementations use sortIndices + a gather operation. Better way?
  def dmultia_sort[A:Manifest:Ordering](ma: Exp[DeliteArray1D[A]])(implicit ctx: SourceContext) = dmultia_sortIndices(dmultia_size(ma), {(i,j) =>
    val aV = dmultia_apply(ma,Indices(i))
    val bV = dmultia_apply(ma,Indices(j))
    // Must have 3 conditions and then a default to conform to Java runtime comparator contract.
    if (delite_less_than(aV, bV)) unit(-1)
    else if (delite_equals(aV, bV)) unit(0)
    else if (delite_greater_than(aV, bV)) unit(1)
    else unit(0)
  }).map{x => dmultia_apply(ma,Indices(x))}.as1D

  def dmultia_sortWith[A:Manifest](ma: Exp[DeliteArray1D[A]], func: (Exp[A], Exp[A]) => Exp[Int])(implicit ctx: SourceContext) = dmultia_sortIndices(dmultia_size(ma), {(i,j) =>
    val aV = dmultia_apply(ma,Indices(i))
    val bV = dmultia_apply(ma,Indices(j))
    val r = func(aV, bV)
    // Must have 3 conditions and then a default to conform to Java runtime comparator contract.
    if (delite_less_than(r, unit(0))) unit(-1)
    else if (delite_equals(r, unit(0))) unit(0)
    else if (delite_greater_than(r, unit(0))) unit(1)
    else unit(0)
  }).map{x => dmultia_apply(ma,Indices(x))}.as1D

  def dmultia_string_split(str: Exp[String], pat: Exp[String], lim: Exp[Int] = unit(0))(implicit ctx: SourceContext) = DeliteMultiArrayStringSplit(str,pat,lim)

  // --- 2D Operations
  def dmultia_matmult[A:Manifest](lhs: Exp[DeliteArray2D[A]], rhs: Exp[DeliteArray2D[A]], default: (Exp[DeliteArray2D[A]], Exp[DeliteArray2D[A]]) => Exp[DeliteArray2D[A]] )(implicit ctx: SourceContext)
    = DeliteMatrixMultiply(lhs, rhs, default)
  def dmultia_matvecmult[A:Manifest](mat: Exp[DeliteArray2D[A]], vec: Exp[DeliteArray1D[A]], default: (Exp[DeliteArray2D[A]], Exp[DeliteArray1D[A]]) => Exp[DeliteArray1D[A]] )(implicit ctx: SourceContext)
    = DeliteMatrixVectorMultiply(mat, vec, default)

  // --- Pinning
  // TBD
  //def dmultia_pin[T:Manifest,R:Manifest](ma: Exp[DeliteMultiArray[T]], layout: Layout[T,R])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayPin[T,R](ma,layout))
  //def dmultia_unpin[T:Manifest,R:Manifest](in: Exp[DeliteArray[R]], layout: Layout[T,R], shape: Seq[Exp[Int]])(implicit ctx: SourceContext) = reflectPure(DeliteMultiArrayUnpin[T,R](ma,layout,shape))

  def darray1dManifest(typeArg: Manifest[_]): Manifest[DeliteArray1D[_]] = makeManifest(classOf[DeliteArray1D[_]], List(typeArg))

  def isMultiArrayType(x: Manifest[_]): Boolean = isSubtype(x.erasure, classOf[DeliteMultiArray[_]])

  def hasMultiArrayType[T](tp: Manifest[T]): Boolean = tp match {
    case tp if isMultiArrayType(tp) => true
    case StructType(_,elems) => elems.map{f => hasMultiArrayType(f._2)}.fold(false){_||_}
    case tp => tp.typeArguments.map{f => hasMultiArrayType(f)}.fold(false){_||_}
  }

  override def isDataStructureType[T](tp: Manifest[T]): Boolean = tp match {
    case tp if isMultiArrayType(tp) => true
    case _ => super.isDataStructureType(tp)
  }

  override def initProps[A](tp: Manifest[A], symData: PropMap[Datakey[_],Metadata], child: Option[SymbolProperties], index: Option[String])(implicit ctx: SourceContext) = tp match {
    case t if isMultiArrayType(t) =>
      val typeChild = initType(t.typeArguments.head)
      val symChild = meet(MetaTypeInit, child, Some(typeChild))
      ArrayProperties(symChild, symData)
    case _ => super.initProps(tp, symData, child, index)
  }

  ////////////////
  // dependencies

  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => blocks(op.body)
    case op: DeliteMultiArrayMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayZipWith[_,_,_] => blocks(op.body)
    case op: DeliteMultiArrayReduce[_] => blocks(op.body) ::: blocks(op.lookup)
    case op: DeliteMultiArrayFold[_] => blocks(op.body) ::: blocks(op.lookup)
    case op: DeliteMultiArrayForeach[_] => blocks(op.body)
    case op: DeliteMultiArrayForIndices[_] => blocks(op.body)
    case op: DeliteMultiArrayForShapeIndices => blocks(op.body)
    case op: DeliteMultiArrayNDMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayGroupBy[_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => blocks(op.keyFunc) ::: blocks(op.valFunc) ::: blocks(op.redFunc)
    case op: DeliteMultiArrayMapFilter[_,_] => blocks(op.mapFunc) ::: blocks(op.filtFunc)
    case op: DeliteMultiArrayFlatMap[_,_] => blocks(op.body)
    case op: DeliteMultiArrayReadFile[_] => blocks(op.body)
    case op: DeliteMultiArrayWriteFile[_] => blocks(op.body)
    case op: DeliteMultiArrayMkString[_] => blocks(op.body.toList)
    case op: DeliteMatrixMultiply[_] => blocks(op.defFunc)
    case op: DeliteMatrixVectorMultiply[_] => blocks(op.defFunc)
    case _ => super.blocks(e)
  }

  // regular data dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => syms(op.body) ::: syms(op.dims)
    case op: DeliteMultiArrayMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => syms(op.body) ::: syms(op.inA) ::: syms(op.inB)
    case op: DeliteMultiArrayReduce[_] => syms(op.body) ::: syms(op.lookup) ::: syms(op.in) ::: syms(op.zero)
    case op: DeliteMultiArrayFold[_] => syms(op.body) ::: syms(op.lookup) ::: syms(op.in) ::: syms(op.zero)
    case op: DeliteMultiArrayForeach[_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayForIndices[_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayForShapeIndices => syms(op.body) ::: syms(op.shape)
    case op: DeliteMultiArrayNDMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayGroupBy[_,_,_] => syms(op.keyFunc) ::: syms(op.valFunc) ::: syms(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => syms(op.keyFunc) ::: syms(op.valFunc) ::: syms(op.redFunc) ::: syms(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => syms(op.mapFunc) ::: syms(op.filtFunc) ::: syms(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => syms(op.body) ::: syms(op.in)
    case op: DeliteMultiArrayReadFile[_] => syms(op.body) ::: syms(op.path) ::: syms(op.dels)
    case op: DeliteMultiArrayWriteFile[_] => syms(op.body) ::: syms(op.path) ::: syms(op.dels) ::: syms(op.in)
    case op: DeliteMultiArrayMkString[_] => syms(op.body.toList) ::: syms(op.ma) ::: syms(op.dels)
    case op: DeliteMatrixMultiply[_] => syms(op.rhs) ::: syms(op.lhs) ::: syms(op.defFunc)
    case op: DeliteMatrixVectorMultiply[_] => syms(op.mat) ::: syms(op.vec) ::: syms(op.defFunc)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => readSyms(op.body) ::: readSyms(op.dims)
    case op: DeliteMultiArrayMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => readSyms(op.body) ::: readSyms(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => readSyms(op.body) ::: readSyms(op.lookup) ::: readSyms(op.in) ::: readSyms(op.zero)
    case op: DeliteMultiArrayFold[_] => readSyms(op.body) ::: readSyms(op.lookup) ::: readSyms(op.in) ::: readSyms(op.zero)
    case op: DeliteMultiArrayForeach[_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayForIndices[_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayForShapeIndices => readSyms(op.body) ::: readSyms(op.shape)
    case op: DeliteMultiArrayNDMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayGroupBy[_,_,_] => readSyms(op.keyFunc) ::: readSyms(op.valFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => readSyms(op.keyFunc) ::: readSyms(op.valFunc) ::: readSyms(op.redFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => readSyms(op.mapFunc) ::: readSyms(op.filtFunc) ::: readSyms(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => readSyms(op.body) ::: readSyms(op.in)
    case op: DeliteMultiArrayReadFile[_] => readSyms(op.body) ::: readSyms(op.path) ::: readSyms(op.dels)
    case op: DeliteMultiArrayWriteFile[_] => readSyms(op.body) ::: readSyms(op.path) ::: readSyms(op.dels) ::: readSyms(op.in)
    case op: DeliteMultiArrayMkString[_] => readSyms(op.body.toList) ::: readSyms(op.ma) ::: readSyms(op.dels)
    case op: DeliteMatrixMultiply[_] => readSyms(op.rhs) ::: readSyms(op.lhs) ::: readSyms(op.defFunc)
    case op: DeliteMatrixVectorMultiply[_] => readSyms(op.mat) ::: readSyms(op.vec) ::: readSyms(op.defFunc)
    case _ => super.readSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case op: DeliteMultiArrayFromFunction[_] => freqHot(op.body) ::: freqNormal(op.dims)
    case op: DeliteMultiArrayMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayZipWith[_,_,_] => freqHot(op.body) ::: freqNormal(List(op.inA, op.inB))
    case op: DeliteMultiArrayReduce[_] => freqHot(op.body) ::: freqHot(op.lookup) ::: freqNormal(op.in) ::: freqNormal(op.zero)
    case op: DeliteMultiArrayFold[_] => freqHot(op.body) ::: freqNormal(op.lookup) ::: freqNormal(op.in) ::: freqNormal(op.zero)
    case op: DeliteMultiArrayForeach[_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayForIndices[_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayForShapeIndices => freqHot(op.body) ::: freqNormal(op.shape)
    case op: DeliteMultiArrayNDMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayGroupBy[_,_,_] => freqHot(op.keyFunc) ::: freqHot(op.valFunc) ::: freqNormal(op.in)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => freqHot(List(op.keyFunc, op.valFunc, op.redFunc)) ::: freqNormal(op.in)
    case op: DeliteMultiArrayMapFilter[_,_] => freqHot(List(op.mapFunc, op.filtFunc)) ::: freqNormal(op.in)
    case op: DeliteMultiArrayFlatMap[_,_] => freqHot(op.body) ::: freqNormal(op.in)
    case op: DeliteMultiArrayReadFile[_] => freqHot(op.body) ::: freqNormal(op.path) ::: freqNormal(op.dels)
    case op: DeliteMultiArrayWriteFile[_] => freqHot(op.body) ::: freqNormal(op.path) ::: freqNormal(op.dels) ::: freqNormal(op.in)
    case op: DeliteMultiArrayMkString[_] => freqHot(op.body.toList) ::: freqNormal(op.dels) ::: freqNormal(op.ma)
    case op: DeliteMatrixMultiply[_] => freqNormal(op.rhs) ::: freqNormal(op.lhs) ::: freqNormal(op.defFunc)
    case op: DeliteMatrixVectorMultiply[_] => freqNormal(op.mat) ::: freqNormal(op.vec) ::: freqNormal(op.defFunc)
    case _ => super.symsFreq(e)
  }

 // symbols which are bound in a definition
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayFromFunction[_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayMap[_,_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayZipWith[_,_,_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayReduce[_] => List(op.v) ::: List(op.rV._1, op.rV._2) ::: effectSyms(op.body) ::: effectSyms(op.lookup)
    case op: DeliteMultiArrayFold[_] => List(op.v) ::: List(op.rV._1, op.rV._2) ::: effectSyms(op.body) ::: effectSyms(op.lookup)
    case op: DeliteMultiArrayForeach[_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayForIndices[_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayForShapeIndices => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayNDMap[_,_] => List(op.v) ::: List(op.rV) ::: effectSyms(op.body)
    case op: DeliteMultiArrayGroupBy[_,_,_] => List(op.v) ::: effectSyms(op.keyFunc) ::: effectSyms(op.valFunc)
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => List(op.v,op.rV._1,op.rV._2) ::: effectSyms(op.keyFunc) ::: effectSyms(op.valFunc) ::: effectSyms(op.redFunc)
    case op: DeliteMultiArrayMapFilter[_,_] => List(op.v) ::: effectSyms(op.mapFunc) ::: effectSyms(op.filtFunc)
    case op: DeliteMultiArrayFlatMap[_,_] => List(op.v) ::: effectSyms(op.body)
    case DeliteMultiArraySortIndices(_,i,body) => syms(i) ::: effectSyms(body)
    case op: DeliteMultiArrayReadFile[_] => List(op.v) ::: List(op.rV) ::: effectSyms(op.body)
    case op: DeliteMultiArrayWriteFile[_] => List(op.v) ::: effectSyms(op.body)
    case op: DeliteMultiArrayMkString[_] => List(op.rV) ::: effectSyms(op.body.toList)
    case op: DeliteMatrixMultiply[_] => List(op.rM1, op.rM2) ::: effectSyms(op.defFunc)
    case op: DeliteMatrixVectorMultiply[_] => List(op.rM, op.rV) ::: effectSyms(op.defFunc)
    case _ => super.boundSyms(e)
  }

  ///////////////////////
  // aliases and sharing

  // return x if e may be equal to x
  // These probably aren't all necessary, adding them just in case
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case op: DeliteMultiArrayInsert[_] => Nil
    case op: DeliteMultiArrayAppend[_] => Nil
    case op: DeliteMultiArrayRemove[_] => Nil
    case op: DeliteMultiArrayInsertAll[_] => Nil
    case op: DeliteMultiArrayMap[_,_] => Nil
    case op: DeliteMultiArrayZipWith[_,_,_] => Nil
    case op: DeliteMultiArrayReduce[_] => Nil
    case op: DeliteMultiArrayFold[_] => Nil
    case op: DeliteMultiArrayForeach[_] => Nil
    case op: DeliteMultiArrayForIndices[_] => Nil
    case op: DeliteMultiArrayForShapeIndices => Nil
    case op: DeliteMultiArrayNDMap[_,_] => Nil
    case op: DeliteMultiArrayGroupBy[_,_,_] => Nil
    case op: DeliteMultiArrayGroupByReduce[_,_,_] => Nil
    case op: DeliteMultiArrayMapFilter[_,_] => Nil
    case op: DeliteMultiArrayFlatMap[_,_] => Nil
    case op: DeliteMatrixMultiply[_] => Nil
    case op: DeliteMatrixVectorMultiply[_] => Nil
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
    case e@DeliteMultiMapSize(m) => dmulmap_size(f(m))(e.mA, e.mB, ctx)
    case e@DeliteMultiMapGet(m,k) => dmulmap_get(f(m),f(k))(e.mA, e.mB, ctx)
    case e@DeliteMultiMapContains(m,k) => dmulmap_contains(f(m),f(k))(e.mA, e.mB, ctx)
    case e@DeliteMultiMapKeys(m) => dmulmap_keys(f(m))(e.mA, e.mB, ctx)
    case e@DeliteMultiMapVals(m) => dmulmap_values(f(m))(e.mA, e.mB, ctx)
    case e@DeliteMultiMapFromArrays(k,v) => dmulmap_from_1d_arrays(f(k),f(v))(e.mA, e.mB, ctx)

    case Reflect(e@DeliteMultiMapSize(m),u,es) => reflectMirrored(Reflect(DeliteMultiMapSize(f(m))(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiMapGet(m,k),u,es) => reflectMirrored(Reflect(DeliteMultiMapGet(f(m),f(k))(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiMapContains(m,k),u,es) => reflectMirrored(Reflect(DeliteMultiMapContains(f(m),f(k))(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiMapKeys(m),u,es) => reflectMirrored(Reflect(DeliteMultiMapKeys(f(m))(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiMapVals(m),u,es) => reflectMirrored(Reflect(DeliteMultiMapVals(f(m))(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiMapFromArrays(k,v), u, es) => reflectMirrored(Reflect(DeliteMultiMapFromArrays(f(k),f(v))(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    // --- properties
    case e@DeliteMultiArrayRank(m) => dmultia_rank(f(m))(e.mA,ctx)
    case e@DeliteMultiArrayDim(m,i) => dmultia_dim(f(m),i)(e.mA,ctx)
    case e@DeliteMultiArraySize(m) => dmultia_size(f(m))(e.mA,ctx)

    // --- creation
    case e@DeliteMultiArrayNew(d) => dmultia_new_immutable(f(d))(e.mA,ctx)
    case e@DeliteMultiArrayView(m,o,s,d,ud) => dmultia_view(f(m),f(o),f(s),f(d),ud)(e.mA,ctx)

    case e@DeliteMultiArrayApply(m,i) => dmultia_apply(f(m),f(i))(e.mR,ctx)
    case e@DeliteMultiArrayUpdate(m,i,x) => toAtom(DeliteMultiArrayUpdate(f(m),f(i),f(x))(e.mA,ctx))(mtype(manifest[A]), ctx)

    case e@DeliteMultiArrayPermute(m,c) => dmultia_permute(f(m),c)(e.mA,ctx)
    case e@DeliteMultiArrayReshape(m,d) => dmultia_reshape(f(m),f(d))(e.mA,ctx)
    case e@DeliteMultiArrayPermuteView(m,c) => dmultia_permute_view(f(m),c)(e.mA,ctx)
    case e@DeliteMultiArrayReshapeView(m,d,u) => dmultia_reshape_view(f(m),f(d),u)(e.mA,ctx)

    // --- Parallel ops
    case e@DeliteMultiArrayReadFile(p,d,k) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayReadFile(f(p),f(d),k)(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayFromFunction(d,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayFromFunction(f(d),g)(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayMap(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayMap(f(m),g)(e.mA,e.mB,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayZipWith(ma,mb,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayZipWith(f(ma),f(mb),g)(e.mA,e.mB,e.mC,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayReduce(m,g,z) =>
      e.asInstanceOf[DeliteMultiArrayReduce[A]] match {   // scalac typer bug (same as in DeliteArray)
        case e@DeliteMultiArrayReduce(m,g,z) =>
          reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayReduce(f(m),g,f(z))(e.mR,ctx))(mtype(manifest[A]),ctx)
      }
    case e@DeliteMultiArrayFold(m,g,z) =>
      e.asInstanceOf[DeliteMultiArrayFold[A]] match {
        case e@DeliteMultiArrayFold(m,g,z) =>
          reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayFold(f(m),g,f(z))(e.mR,ctx))(mtype(manifest[A]),ctx)
      }
    case e@DeliteMultiArrayForeach(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayForeach(f(m),g)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayForIndices(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayForIndices(f(m),g)(e.mA,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayForShapeIndices(s,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayForShapeIndices(f(s),g)(ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayNDMap(m,d,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayNDMap(f(m),d,g)(e.mA,e.mB,ctx))(mtype(manifest[A]),ctx)
    case e@DeliteMultiArrayGroupBy(m,k,v) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayGroupBy(f(m),k,v)(e.mA,e.mB,e.mC,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayGroupByReduce(m,k,v,r) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayGroupByReduce(f(m),k,v,r)(e.mA,e.mB,e.mC,ctx))(mtype(manifest[A]), ctx)

    // --- mutations (TODO: do we need these? Should always have Reflect wrapper on these nodes)
    case e@DeliteMultiArrayInsert(m,x,i) => toAtom(DeliteMultiArrayInsert(f(m),f(x),f(i))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayAppend(m,x) => toAtom(DeliteMultiArrayAppend(f(m),f(x))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayInsertAll(m,r,a,i) => toAtom(DeliteMultiArrayInsertAll(f(m),f(r),a,f(i))(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayRemove(m,a,s,l) => toAtom(DeliteMultiArrayRemove(f(m),a,f(s),f(l))(e.mA,ctx))(mtype(manifest[A]), ctx)

    // --- Misc
    case e@DeliteMultiArrayMkString(m,d,k) => reflectPure(new { override val original = Some(f,e)} with DeliteMultiArrayMkString(f(m),f(d),k)(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayWriteFile(i,d,p,k) => reflectPure(new { override val original = Some(f,e)} with DeliteMultiArrayWriteFile(f(i),f(d),f(p),k)(e.mA,ctx))(mtype(manifest[A]), ctx)

    // --- 1D Ops
    case DeliteMultiArrayStringSplit(s,p,l) => dmultia_string_split(f(s),f(p),f(l))(ctx)
    case e@DeliteMultiArraySortIndices(l,v,c) => reflectPure(DeliteMultiArraySortIndices(f(l),(f(v._1).asInstanceOf[Sym[Int]],f(v._2).asInstanceOf[Sym[Int]]),f(c)))

    // --- 1D Parallel
    case e@DeliteMultiArrayMapFilter(m,g,c) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayMapFilter(f(m),g,c)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMultiArrayFlatMap(m,g) => reflectPure(new { override val original = Some(f,e) } with DeliteMultiArrayFlatMap(f(m),g)(e.mA,e.mB,ctx))(mtype(manifest[A]), ctx)

    // --- 2D Parallel
    case e@DeliteMatrixMultiply(l,r,d) => reflectPure(new {override val original = Some(f,e) } with DeliteMatrixMultiply(f(l),f(r),d)(e.mA,ctx))(mtype(manifest[A]), ctx)
    case e@DeliteMatrixVectorMultiply(m,v,d) => reflectPure(new {override val original = Some(f,e) } with DeliteMatrixVectorMultiply(f(m),f(v),d)(e.mA,ctx))(mtype(manifest[A]), ctx)

    //case e@DeliteMultiArrayPin(m,l) => reflectPure(DeliteMultiArrayPin(f(m),l)(e.mA,e.mB))(mtype(manifest[A]), ctx)
    //case e@DeliteMultiArrayUnpin(in,l,s) => reflectPure(DeliteMultiArrayUnpin(f(in),l,f(s))(e.mA,e.mB))(mtype(manifest[A]),ctx)

    // --- reflect of properties
    case Reflect(e@DeliteMultiArrayRank(m), u, es) => reflectMirrored(Reflect(DeliteMultiArrayRank(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayDim(m,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayDim(f(m),i)(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArraySize(m), u, es) => reflectMirrored(Reflect(DeliteMultiArraySize(f(m))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    // --- creation
    case Reflect(e@DeliteMultiArrayNew(d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayNew(f(d))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayView(m,o,s,d,ud), u, es) => reflectMirrored(Reflect(DeliteMultiArrayView(f(m),f(o),f(s),f(d),ud)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayApply(m,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayApply(f(m),f(i))(e.mR), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayUpdate(m,i,x), u, es) => reflectMirrored(Reflect(DeliteMultiArrayUpdate(f(m),f(i),f(x))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayPermute(m,c), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPermute(f(m),c)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayReshape(m,d), u, es) => reflectMirrored(Reflect(DeliteMultiArrayReshape(f(m),f(d))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayPermuteView(m,c), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPermuteView(f(m),c)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayReshapeView(m,d,s), u, es) => reflectMirrored(Reflect(DeliteMultiArrayReshapeView(f(m),f(d),s)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayReadFile(p,d,k), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with DeliteMultiArrayReadFile(f(p),f(d),k)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayFromFunction(d,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayFromFunction(f(d),g)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayMap(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayMap(f(m),g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayZipWith(ma,mb,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayZipWith(f(ma),f(mb),g)(e.mA,e.mB,e.mC,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayReduce(m,g,z), u, es) =>
      e.asInstanceOf[DeliteMultiArrayReduce[A]] match {  // scalac typer bug (same as in DeliteArray)
        case e@DeliteMultiArrayReduce(m,g,z) =>
          reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayReduce(f(m),g,f(z))(e.mR,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      }
    case Reflect(e@DeliteMultiArrayFold(m,g,z), u, es) =>
      e.asInstanceOf[DeliteMultiArrayFold[A]] match {
        case e@DeliteMultiArrayFold(m,g,z) =>
          reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayFold(f(m),g,f(z))(e.mR,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]),ctx)
      }
    case Reflect(e@DeliteMultiArrayForeach(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayForeach(f(m),g)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayForIndices(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayForIndices(f(m),g)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayForShapeIndices(s,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayForShapeIndices(f(s),g)(ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayNDMap(m,d,g), u, es) => reflectMirrored(Reflect(DeliteMultiArrayNDMap(f(m),d,g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayGroupBy(m,k,v), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayGroupBy(f(m),k,v)(e.mA,e.mB,e.mC,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayGroupByReduce(m,k,v,r), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayGroupByReduce(f(m),k,v,r)(e.mA,e.mB,e.mC,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayInsert(m,x,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayInsert(f(m),f(x),f(i))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayAppend(m,x), u, es) => reflectMirrored(Reflect(DeliteMultiArrayAppend(f(m),f(x))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayInsertAll(m,r,a,i), u, es) => reflectMirrored(Reflect(DeliteMultiArrayInsertAll(f(m),f(r),a,f(i))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayRemove(m,a,s,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayRemove(f(m),a,f(s),f(l))(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayMkString(m,d,k), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with DeliteMultiArrayMkString(f(m),f(d),k)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayWriteFile(i,d,p,k), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e)} with DeliteMultiArrayWriteFile(f(i),f(d),f(p),k)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(DeliteMultiArrayStringSplit(s,p,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayStringSplit(f(s),f(p),f(l)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArraySortIndices(l,v,c), u, es) => reflectMirrored(Reflect(DeliteMultiArraySortIndices(f(l),(f(v._1).asInstanceOf[Sym[Int]],f(v._2).asInstanceOf[Sym[Int]]),f(c)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMultiArrayMapFilter(m,g,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayMapFilter(f(m),g,c)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMultiArrayFlatMap(m,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteMultiArrayFlatMap(f(m),g)(e.mA,e.mB,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case Reflect(e@DeliteMatrixMultiply(l,r,d), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with DeliteMatrixMultiply(f(l),f(r),d)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@DeliteMatrixVectorMultiply(m,v,d), u, es) => reflectMirrored(Reflect(new {override val original = Some(f,e) } with DeliteMatrixVectorMultiply(f(m),f(v),d)(e.mA,ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    //case Reflect(e@DeliteMultiArrayPin(m,l), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPin(f(m),l)(e.mA,e.mB), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    //case Reflect(e@DeliteMultiArrayUnpin(a,l,s), u, es) => reflectMirrored(Reflect(DeliteMultiArrayPin(f(a),l,f(s))(e.mA,e.mB), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}*/