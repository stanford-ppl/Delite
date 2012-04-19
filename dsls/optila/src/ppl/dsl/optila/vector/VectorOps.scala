package ppl.dsl.optila.vector

import java.io.{PrintWriter}
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}

import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.dsl.optila._

trait VectorOps extends Variables {
  this: OptiLA =>

  abstract class VectorBuilder[Elem, To] {
    def alloc(length: Rep[Int], isRow: Rep[Boolean]): Rep[To]
    def toIntf(x: Rep[To]): Interface[Vector[Elem]]
  }  
  // def sparseVectorBuilder[A:Manifest] = new VectorBuilder[A,SparseVector[A]] {
  //     def alloc(length: Rep[Int], isRow: Rep[Boolean]) = Vector.sparse[A](length, isRow)
  //   }
  
  object Vector {
    def apply[A:Manifest](len: Int, isRow: Boolean)(implicit ctx: SourceContext) = densevector_obj_new(unit(len), unit(isRow)) // needed to resolve ambiguities
    def apply[A](len: Rep[Int], isRow: Rep[Boolean])(implicit mA: Manifest[A], o: Overloaded1, ctx: SourceContext) = densevector_obj_new(len, isRow)
    def apply[A:Manifest](xs: A*)(implicit ctx: SourceContext) = {
      val out = densevector_obj_new[A](unit(0),unit(true))
      // interpreted (not lifted)
      xs.foreach { out += unit(_) }
      out.unsafeImmutable
    }
    def apply[A](xs: Rep[A]*)(implicit mA: Manifest[A], o: Overloaded2, ctx: SourceContext) = {
      val out = densevector_obj_new[A](unit(0),unit(true))
      // interpreted (not lifted)
      xs.foreach { out += _ }
      out.unsafeImmutable // return immutable object
    }

    def dense[A:Manifest](len: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext) = densevector_obj_new(len, isRow)
    //def sparse[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) = sparsevector_obj_new(len, isRow)
        
    def ones(len: Rep[Int])(implicit ctx: SourceContext) = DenseVector.ones(len)
    def onesf(len: Rep[Int])(implicit ctx: SourceContext) = DenseVector.onesf(len)
    def zeros(len: Rep[Int])(implicit ctx: SourceContext) = DenseVector.zeros(len)
    def zerosf(len: Rep[Int])(implicit ctx: SourceContext) = DenseVector.zerosf(len)
    def rand(len: Rep[Int])(implicit ctx: SourceContext) = DenseVector.rand(len)
    def randf(len: Rep[Int])(implicit ctx: SourceContext) = DenseVector.randf(len)
    def range(start: Rep[Int], end: Rep[Int], stride: Rep[Int] = unit(1), isRow: Rep[Boolean] = unit(true))(implicit ctx: SourceContext) =
      vector_obj_range(start, end, stride, isRow)
    def uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean] = unit(true))(implicit ctx: SourceContext) =
      DenseVector.uniform(start, step_size, end, isRow)
  }

  // class OpInfo[A,That,Intf] {
  //     implicit val mR: Manifest[That]
  //     def toIntf(x: Rep[That]): Interface[Intf]    
  //     def builder: VectorBuilder[A,That]
  //   }
    
  trait VecOpsCls[A] extends DCInterfaceOps[Vector[A],A] {
    // type VA // self type    
    implicit def mA: Manifest[A] 
    // implicit def mVA: Manifest[VA]        
    // implicit def toOps(x: Rep[VA]): VecOpsCls[A]
    // implicit def toIntf(x: Rep[VA]): Interface[Vector[A]]        
    // implicit def builder: VectorBuilder[A,VA]        
    
    type V[X] // generic return type, unless overloaded for the op as below (TODO: use type classes to clean this up!)
    type M[X] 
    implicit def mV[B:Manifest]: Manifest[V[B]]         
    implicit def toOps[B:Manifest](x: Rep[V[B]]): VecOpsCls[B]
    implicit def toIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]]        
    implicit def builder[B:Manifest](implicit ctx: SourceContext): VectorBuilder[B,V[B]]    
    implicit def mM[B:Manifest]: Manifest[M[B]]         
    implicit def matToIntf[B:Manifest](x: Rep[M[B]]): Interface[Matrix[B]]        
    implicit def matBuilder[B:Manifest](implicit ctx: SourceContext): MatrixBuilder[B,M[B]]    
    
    type VA = V[A] // temporary for easy compatibility with old stuff
    type MA = M[A]

    type Self <: Vector[A]
    implicit def wrap(x: Rep[Self]): Interface[Vector[A]]
    val elem: Rep[Self] 
    val x = elem
    
    // DeliteCollection
    def dcSize(implicit ctx: SourceContext) = length
    def dcApply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] = apply(n)
    def dcUpdate(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = update(n,y)
    
    // conversions
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) =  map(e => conv(e))
    def toDouble(implicit conv: Rep[A] => Rep[Double]) =  map(e => conv(e))
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = map(e => conv(e))
    def toInt(implicit conv: Rep[A] => Rep[Int]) = map(e => conv(e))
    //def toLong(implicit conv: Rep[A] => Rep[Long]) = map(e => conv(e))
    
    // accessors
    def length(implicit ctx: SourceContext): Rep[Int] 
    def isRow(implicit ctx: SourceContext): Rep[Boolean] 
    def apply(n: Rep[Int])(implicit ctx: SourceContext): Rep[A] 
    def isEmpty(implicit ctx: SourceContext) = length == unit(0)
    def first(implicit ctx: SourceContext) = apply(unit(0))
    def last(implicit ctx: SourceContext) = apply(length - unit(1))
    def indices(implicit ctx: SourceContext) = (unit(0)::length)
    def drop(count: Rep[Int])(implicit ctx: SourceContext) = slice(count, length)
    def take(count: Rep[Int])(implicit ctx: SourceContext) = slice(unit(0), count)
    def slice(start: Rep[Int], end: Rep[Int])(implicit ctx: SourceContext): Rep[VA] = vector_slice[A,VA](x, start, end)
    def contains(y: Rep[A])(implicit ctx: SourceContext): Rep[Boolean] = vector_contains(x,y)
    def distinct(implicit ctx: SourceContext): Rep[VA] = vector_distinct[A,VA](x)  
    
    // general
    def t(implicit ctx: SourceContext): Rep[VA] // TODO: move to type-system
    def mt()(implicit ctx: SourceContext): Rep[VA]
    def Clone()(implicit ctx: SourceContext): Rep[VA] = vector_clone[A,VA](x) 
    def mutable()(implicit ctx: SourceContext): Rep[VA] = vector_mutable_clone[A,VA](x)
    def pprint()(implicit ctx: SourceContext): Rep[Unit] = vector_pprint(x)
    def replicate(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[MA] = vector_repmat[A,MA](x,i,j)
    //def replicate(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[A]] = vector_repmat[A](x,i,j)
    def mkString(sep: Rep[String] = unit(""))(implicit ctx: SourceContext): Rep[String] = vector_mkstring(x, sep)      
    
    // data operations
    // most of these these can't operate on Interfaces because they happen at runtime right now...
    // (would need to use a more generic setter field, but Struct should make this obsolete anyways)    
    
    // TODO: should these be moved to another interface, e.g. MutableVecInterface? how do we deal with immutable vectors that
    // don't implement these (e.g. RangeVector). Instead of Interface, would clients require a MutableInterface[...]? (yet another type constructor... :( )
    def update(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
    def :+(y: Rep[A])(implicit ctx: SourceContext): Rep[VA] = {
      val out = mutable()
      out += y
      out.unsafeImmutable
      // val out = builder[A].alloc(length+1, isRow)
      // for (i <- 0 until out.length) {
      //   out(i) = apply(i)
      // }
      // out(length) = y
      // out
    }
    def +=(y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] = insert(length,y)
    def ++(y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[VA] = vector_concatenate[A,VA](x,y)    
    def ++=(y: Rep[VA])(implicit ctx: SourceContext) = insertAll(length,y)
    def copyFrom(pos: Rep[Int], y: Rep[VA])(implicit ctx: SourceContext): Rep[Unit]
    def insert(pos: Rep[Int], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]
    def insertAll(pos: Rep[Int], y: Rep[VA])(implicit ctx: SourceContext): Rep[Unit]
    def remove(pos: Rep[Int])(implicit ctx: SourceContext) = removeAll(pos,unit(1))
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext): Rep[Unit]
    def trim()(implicit ctx: SourceContext): Rep[Unit]
    def clear()(implicit ctx: SourceContext): Rep[Unit]
    
    // arithmetic operations
    
    // we only need to go through this gymnastic hack when we have different return values for different ops;
    // usually this would just return Rep[VA] (i.e. the same as the lhs)
    // would be really nice if we could use a default value here (VA), but the overrides don't seem to work...
    // could overload for a more specific static type, but can't override +(y: Interface[Vector[A]]) in SparseVecOps because the return value Dense is not a subtype of VA
    type VPLUSR
    implicit val mVPLUSR: Manifest[VPLUSR]
    implicit def vplusBuilder(implicit ctx: SourceContext): VectorBuilder[A,VPLUSR]
    def vplusToIntf(x: Rep[VPLUSR]): Interface[Vector[A]]
    //val plusInfo: OpInfo[A,VPLUSR,Interface[Vector[A]]]    
    //def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_plus[A,VPLUSR](x,y)(manifest[A], implicitly[Arith[A]], plusInfo.mR, plusInfo.b)
    def +(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_plus[A,VPLUSR](x,y)
    def +[B:Manifest](y: Interface[Vector[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = vector_plus_withconvert[B,A,VPLUSR](y,x)
    def +(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext): Rep[VA] = vector_plus[A,VA](x,y) // needed for Arith        
    def +(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_plus_scalar[A,VPLUSR](x,y) 
    def +=(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = { vector_plusequals[A](x,y); elem }
    def +=(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext) = { vector_plusequals[A](x,y); elem }
    def :+=(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_plusequals_scalar[A](x,y) 
    
    type VMINUSR
    implicit val mVMINUSR: Manifest[VMINUSR]
    implicit def vminusBuilder(implicit ctx: SourceContext): VectorBuilder[A,VMINUSR]
    def vminusToIntf(x: Rep[VMINUSR]): Interface[Vector[A]]
    def -(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_minus[A,VMINUSR](x,y)
    def -[B:Manifest](y: Interface[Vector[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = vector_minus_withconvert[B,A,VMINUSR](y,x)
    def -(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext) = vector_minus[A,VA](x,y)
    def -(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_minus_scalar[A,VMINUSR](x,y)
    def -=(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = { vector_minusequals[A](x,y); x }
    def -=(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext) = { vector_minusequals[A](x,y); x }    
    def -=(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_minusequals_scalar[A](x,y)
    
    type VTIMESR
    implicit val mVTIMESR: Manifest[VTIMESR]
    implicit def vtimesBuilder(implicit ctx: SourceContext): VectorBuilder[A,VTIMESR]
    def vtimesToIntf(x: Rep[VTIMESR]): Interface[Vector[A]]    
    // TODO: need to extend Arith to support this using CanXX dispatch
    // Rep[DenseVector[Double]] * Rep[RangeVector] (Rep[DenseVector[Double]] * Interface[Vector[Int]])    
    def *(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_times[A,VTIMESR](x,y)        
    def *[B:Manifest](y: Interface[Vector[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = vector_times_withconvert[B,A,VTIMESR](y,x)
    def *(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext) = vector_times[A,VA](x,y)
    def *(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_times_scalar[A,VTIMESR](x,y)
    def *=(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_timesequals[A](x,y)    
    def *=(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext) = vector_timesequals[A](x,y)
    def *=(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_timesequals_scalar[A](x,y)    
    //def *(y: Rep[DenseMatrix[A]])(implicit a: Arith[A],o: Overloaded2) = vector_times_matrix[A,VTIMESR](x,y)
    def **(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_outer[A](x,y)
    //def **(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_outer[A,MA](x,y)
    def *:*(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_dot_product(x,y)
    def dot(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_dot_product(x,y)

    def /(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_divide[A,VA](x,y)    
    def /[B:Manifest](y: Interface[Vector[B]])(implicit a: Arith[A], c: Rep[B] => Rep[A], ctx: SourceContext) = vector_divide_withconvert[B,A,VA](y,x)
    def /(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext) = vector_divide[A,VA](x,y)
    def /(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_divide_scalar[A,VA](x,y)    
    def /=(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = vector_divideequals[A](x,y)    
    def /=(y: Rep[VA])(implicit a: Arith[A], ctx: SourceContext) = vector_divideequals[A](x,y)
    def /=(y: Rep[A])(implicit a: Arith[A], ctx: SourceContext, o: Overloaded1) = vector_divideequals_scalar[A](x,y)
    
    def sum(implicit a: Arith[A], ctx: SourceContext) = vector_sum(x)
    def abs(implicit a: Arith[A], ctx: SourceContext) = vector_abs[A,VA](x)
    def exp(implicit a: Arith[A], ctx: SourceContext) = vector_exp[A,VA](x)    
    
    def sort(implicit o: Ordering[A], ctx: SourceContext): Rep[VA]
    def min(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = vector_min(x)
    def minIndex(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = vector_minindex(x)
    def max(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = vector_max(x)
    def maxIndex(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = vector_maxindex(x)
    def median(implicit o: Ordering[A], ctx: SourceContext) = vector_median(x)
    def :>(y: Interface[Vector[A]])(implicit o: Ordering[A], ctx: SourceContext) = zip(y) { (a,b) => a > b }
    def :<(y: Interface[Vector[A]])(implicit o: Ordering[A], ctx: SourceContext) = zip(y) { (a,b) => a < b }    
    
    // bulk operations
    def map[B:Manifest](f: Rep[A] => Rep[B])(implicit ctx: SourceContext): Rep[V[B]] = vector_map[A,B,V[B]](x,f)
    def mmap(f: Rep[A] => Rep[A])(implicit ctx: SourceContext): Rep[Self] = { vector_mmap(x,f); elem }
    def foreach(block: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit] = vector_foreach(x, block)
    def zip[B:Manifest,R:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext): Rep[V[R]] = vector_zipwith[A,B,R,V[R]](x,y,f)
    def mzip[B:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[A])(implicit ctx: SourceContext): Rep[Self] = { vector_mzipwith(x,y,f); elem }
    def reduce(f: (Rep[A],Rep[A]) => Rep[A])(implicit a: Arith[A], ctx: SourceContext): Rep[A] = vector_reduce(x,f)
    def filter(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[VA] = vector_filter[A,VA](x,pred)
    
    // type VFINDR
    // implicit val mVFINDR: Manifest[VFINDR]
    // implicit val vfindBuilder: VectorBuilder[Int,VFINDR]    
    // def vfindToIntf(x: Rep[VFINDR]): Interface[Vector[Int]]        
    //def find(pred: Rep[A] => Rep[Boolean]): Rep[VFINDR] = vector_find[A,VFINDR](x,pred)
    def find(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[V[Int]] = vector_find[A,V[Int]](x,pred)    
    def count(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[Int] = vector_count(x, pred)
    // def flatMap[B:Manifest](f: Rep[A] => Rep[V[B]]): Rep[V[B]] = vector_flatmap[A,B,V[B]](x,f)
    // def partition(pred: Rep[A] => Rep[Boolean]): (Rep[VA], Rep[VA])  = vector_partition[A,VA](x,pred)
    // def groupBy[K:Manifest](pred: Rep[A] => Rep[K]): Rep[V[VA]] = vector_groupby[A,K,VA,V[VA]](x,pred)                        
  }
  
  def __equal[A:Manifest,V[X] <: Vector[X]](a: Rep[V[A]], b: Rep[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], mA: Manifest[V[A]], ctx: SourceContext, o: Overloaded1): Rep[Boolean] = vector_equals(a,b)
  def __equal[A:Manifest,V[X] <: Vector[X]](a: Rep[V[A]], b: Var[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], mA: Manifest[V[A]], ctx: SourceContext, o: Overloaded2): Rep[Boolean] = vector_equals(a,readVar(b))
  def __equal[A:Manifest,V[X] <: Vector[X]](a: Var[V[A]], b: Rep[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], mA: Manifest[V[A]], ctx: SourceContext, o: Overloaded3): Rep[Boolean] = vector_equals(readVar(a),b)
  def __equal[A:Manifest,V[X] <: Vector[X]](a: Var[V[A]], b: Var[V[A]])(implicit toIntf: Rep[V[A]] => Interface[Vector[A]], mA: Manifest[V[A]], ctx: SourceContext, o: Overloaded4): Rep[Boolean] = vector_equals(readVar(a),readVar(b))
  
  /**
   * Binary math operations on Vectors with unit conversions (precision widening). 
   */  
  
  // generic 
  def infix_+[L,R:Arith:Manifest,V[X] <: Vector[X]](lhs: L, rhs: Rep[V[R]])(implicit c: L => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded1): Rep[V[R]] = vector_plus_scalar[R,V[R]](toIntf(rhs),c(lhs))
  def infix_+[L:Arith:Manifest,R:Manifest,V[X] <: Vector[X]](lhs: Rep[L], rhs: Rep[V[R]])(implicit c: Rep[R] => Rep[L], vb: VectorBuilder[L,V[L]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[L]], ctx: SourceContext, o: Overloaded2): Rep[V[L]] = vector_plus_scalar_withconvert[R,L,V[L]](toIntf(rhs),lhs)
  def infix_+[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded3): Rep[V[R]] = vector_plus_scalar_withconvert[L,R,V[R]](toIntf(lhs),rhs)
  def infix_+[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: R)(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded4): Rep[V[R]] = vector_plus_scalar_withconvert[L,R,V[R]](toIntf(lhs),unit(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Interface[Vector[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded5): Rep[V[R]] = vector_plus_withconvert[L,R,V[R]](lhs,toIntf(rhs))
  def infix_+[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntfL: Rep[V[L]] => Interface[Vector[L]], toIntfR: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded6): Rep[V[R]] = vector_plus_withconvert[L,R,V[R]](toIntfL(lhs),toIntfR(rhs))
  
  // special cases to fill holes
  def infix_+[V[X] <: Vector[X]](lhs: Rep[Int], rhs: Rep[V[Double]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Double]] => Interface[Vector[Double]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded7): Rep[V[Double]] = vector_plus_scalar[Double,V[Double]](toIntf(rhs),repIntToRepDouble(lhs))
  def infix_+[V[X] <: Vector[X]](lhs: Rep[Int], rhs: Rep[V[Float]])(implicit vb: VectorBuilder[Float,V[Float]], toIntf: Rep[V[Float]] => Interface[Vector[Float]], m: Manifest[V[Float]], ctx: SourceContext, o: Overloaded8): Rep[V[Float]] = vector_plus_scalar[Float,V[Float]](toIntf(rhs),repIntToRepFloat(lhs))
  def infix_+[V[X] <: Vector[X]](lhs: Rep[Float], rhs: Rep[V[Double]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Double]] => Interface[Vector[Double]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded9): Rep[V[Double]] = vector_plus_scalar[Double,V[Double]](toIntf(rhs),repFloatToRepDouble(lhs))
  def infix_+[V[X] <: Vector[X]](lhs: Float, rhs: Rep[V[Int]])(implicit vb: VectorBuilder[Float,V[Float]], toIntf: Rep[V[Int]] => Interface[Vector[Int]], m: Manifest[V[Float]], ctx: SourceContext, o: Overloaded10): Rep[V[Float]] = vector_plus_scalar_withconvert[Int,Float,V[Float]](toIntf(rhs),unit(lhs))
  def infix_+[V[X] <: Vector[X]](lhs: Double, rhs: Rep[V[Int]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Int]] => Interface[Vector[Int]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded11): Rep[V[Double]] = vector_plus_scalar_withconvert[Int,Double,V[Double]](toIntf(rhs),unit(lhs))
  def infix_+[V[X] <: Vector[X]](lhs: Double, rhs: Rep[V[Float]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Float]] => Interface[Vector[Float]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded12): Rep[V[Double]] = vector_plus_scalar_withconvert[Float,Double,V[Double]](toIntf(rhs),unit(lhs))

  def infix_-[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded1): Rep[V[R]] = vector_minus_scalar_withconvert[L,R,V[R]](toIntf(lhs),rhs)
  def infix_-[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: R)(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded2): Rep[V[R]] = vector_minus_scalar_withconvert[L,R,V[R]](toIntf(lhs),unit(rhs))
  def infix_-[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Interface[Vector[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded3): Rep[V[R]] = vector_minus_withconvert[L,R,V[R]](lhs,toIntf(rhs))
  def infix_-[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntfL: Rep[V[L]] => Interface[Vector[L]], toIntfR: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded4): Rep[V[R]] = vector_minus_withconvert[L,R,V[R]](toIntfL(lhs),toIntfR(rhs))

  def infix_*[L,R:Arith:Manifest,V[X] <: Vector[X]](lhs: L, rhs: Rep[V[R]])(implicit c: L => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded1): Rep[V[R]] = vector_times_scalar[R,V[R]](toIntf(rhs),c(lhs))
  def infix_*[L:Arith:Manifest,R:Manifest,V[X] <: Vector[X]](lhs: Rep[L], rhs: Rep[V[R]])(implicit c: Rep[R] => Rep[L], vb: VectorBuilder[L,V[L]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[L]], ctx: SourceContext, o: Overloaded2): Rep[V[L]] = vector_times_scalar_withconvert[R,L,V[L]](toIntf(rhs),lhs)
  def infix_*[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded3): Rep[V[R]] = vector_times_scalar_withconvert[L,R,V[R]](toIntf(lhs),rhs)
  def infix_*[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: R)(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded4): Rep[V[R]] = vector_times_scalar_withconvert[L,R,V[R]](toIntf(lhs),unit(rhs))
  def infix_*[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Interface[Vector[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded5): Rep[V[R]] = vector_times_withconvert[L,R,V[R]](lhs,toIntf(rhs))
  def infix_*[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntfL: Rep[V[L]] => Interface[Vector[L]], toIntfR: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded6): Rep[V[R]] = vector_times_withconvert[L,R,V[R]](toIntfL(lhs),toIntfR(rhs))
  def infix_*[V[X] <: Vector[X]](lhs: Rep[Int], rhs: Rep[V[Double]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Double]] => Interface[Vector[Double]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded7): Rep[V[Double]] = vector_times_scalar[Double,V[Double]](toIntf(rhs),repIntToRepDouble(lhs))
  def infix_*[V[X] <: Vector[X]](lhs: Rep[Int], rhs: Rep[V[Float]])(implicit vb: VectorBuilder[Float,V[Float]], toIntf: Rep[V[Float]] => Interface[Vector[Float]], m: Manifest[V[Float]], ctx: SourceContext, o: Overloaded8): Rep[V[Float]] = vector_times_scalar[Float,V[Float]](toIntf(rhs),repIntToRepFloat(lhs))
  def infix_*[V[X] <: Vector[X]](lhs: Rep[Float], rhs: Rep[V[Double]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Double]] => Interface[Vector[Double]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded9): Rep[V[Double]] = vector_times_scalar[Double,V[Double]](toIntf(rhs),repFloatToRepDouble(lhs))
  def infix_*[V[X] <: Vector[X]](lhs: Float, rhs: Rep[V[Int]])(implicit vb: VectorBuilder[Float,V[Float]], toIntf: Rep[V[Int]] => Interface[Vector[Int]], m: Manifest[V[Float]], ctx: SourceContext, o: Overloaded10): Rep[V[Float]] = vector_times_scalar_withconvert[Int,Float,V[Float]](toIntf(rhs),unit(lhs))
  def infix_*[V[X] <: Vector[X]](lhs: Double, rhs: Rep[V[Int]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Int]] => Interface[Vector[Int]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded11): Rep[V[Double]] = vector_times_scalar_withconvert[Int,Double,V[Double]](toIntf(rhs),unit(lhs))
  def infix_*[V[X] <: Vector[X]](lhs: Double, rhs: Rep[V[Float]])(implicit vb: VectorBuilder[Double,V[Double]], toIntf: Rep[V[Float]] => Interface[Vector[Float]], m: Manifest[V[Double]], ctx: SourceContext, o: Overloaded12): Rep[V[Double]] = vector_times_scalar_withconvert[Float,Double,V[Double]](toIntf(rhs),unit(lhs))
  
  def infix_/[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[R])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded1): Rep[V[R]] = vector_divide_scalar_withconvert[L,R,V[R]](toIntf(lhs),rhs)
  def infix_/[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: R)(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[L]] => Interface[Vector[L]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded2): Rep[V[R]] = vector_divide_scalar_withconvert[L,R,V[R]](toIntf(lhs),unit(rhs))
  def infix_/[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Interface[Vector[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntf: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded3): Rep[V[R]] = vector_divide_withconvert[L,R,V[R]](lhs,toIntf(rhs))
  def infix_/[L:Manifest,R:Arith:Manifest,V[X] <: Vector[X]](lhs: Rep[V[L]], rhs: Rep[V[R]])(implicit c: Rep[L] => Rep[R], vb: VectorBuilder[R,V[R]], toIntfL: Rep[V[L]] => Interface[Vector[L]], toIntfR: Rep[V[R]] => Interface[Vector[R]], m: Manifest[V[R]], ctx: SourceContext, o: Overloaded4): Rep[V[R]] = vector_divide_withconvert[L,R,V[R]](toIntfL(lhs),toIntfR(rhs))
  
  
  /**
   * Interface[Vector] 
   */
   
  // clients that can handle multiple kinds of vector must accept an Interface[Vector[T]],  not a Rep[Vector[T]]
  class VInterface[A:Manifest](val ops: VecOpsCls[A]) extends DCInterface[Vector[A],A] {// clients use Interface[Vector]
    override def toString = "VInterface(" + ops.elem.toString + "  [manifest: " + ops.mA.toString + "])"
  }

  // then we convert from a Interface[Vector[T]] to an interfaceVecToOpsCls, providing all of the original vector methods  
  implicit def interfaceToVecOps[A:Manifest](intf: Interface[Vector[A]]): InterfaceVecOpsCls[A] = new InterfaceVecOpsCls(intf.asInstanceOf[VInterface[A]]) // all Interface[Vector] should be instances of VInterface, but can we enforce this?
  
  class InterfaceVecOpsCls[A:Manifest](val intf: VInterface[A]) {
    // would be nice - could short-circuit the operation if known statically!
    // asSparse, toSparse
    // asDense, toDense
    
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) = intf.ops.toIntf(intf.ops.toBoolean)
    def toDouble(implicit conv: Rep[A] => Rep[Double]) = intf.ops.toIntf(intf.ops.toDouble)
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = intf.ops.toIntf(intf.ops.toFloat)
    def toInt(implicit conv: Rep[A] => Rep[Int]) = intf.ops.toIntf(intf.ops.toInt)
    //def toLong(implicit conv: Rep[A] => Rep[Long]) = intf.ops.toIntf(intf.ops.toLong)
    
    def length(implicit ctx: SourceContext) = intf.ops.length
    def isRow(implicit ctx: SourceContext) = intf.ops.isRow    
    def apply(n: Rep[Int])(implicit ctx: SourceContext) = intf.ops.apply(n)
    def isEmpty(implicit ctx: SourceContext) = intf.ops.isEmpty
    def first(implicit ctx: SourceContext) = intf.ops.first
    def last(implicit ctx: SourceContext) = intf.ops.last
    def indices(implicit ctx: SourceContext) = intf.ops.indices
    def drop(count: Rep[Int])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.drop(count))
    def take(count: Rep[Int])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.take(count))
    def slice(start: Rep[Int], end: Rep[Int])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.slice(start,end))
    def contains(y: Rep[A])(implicit ctx: SourceContext): Rep[Boolean] = intf.ops.contains(y)
    def distinct(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.distinct)    
    
    def t(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.t)
    def mt()(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.mt)
    def Clone()(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.Clone)
    def mutable()(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.mutable)
    def pprint()(implicit ctx: SourceContext) = intf.ops.pprint
    //def replicate(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = intf.ops.matToIntf(intf.ops.replicate(i,j))
    def replicate(i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = intf.ops.replicate(i,j)
    def mkString(sep: Rep[String] = unit(""))(implicit ctx: SourceContext) = intf.ops.mkString(sep)
    
    def update(n: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = intf.ops.update(n,y)
    def +=(y: Rep[A])(implicit ctx: SourceContext) = intf.ops.+=(y)    
    def :+(y: Rep[A])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.:+(y))
    def ++(y: Interface[Vector[A]])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.++(y))
    //def ++=(y: Rep[intf.ops.V[A]]) = intf.ops.++=(y)
    //def copyFrom(pos: Rep[Int], y: Rep[intf.ops.V[A]]) = intf.ops.copyFrom(pos,y)
    def insert(pos: Rep[Int], y: Rep[A])(implicit ctx: SourceContext) = intf.ops.insert(pos,y)
    //def insertAll(pos: Rep[Int], y: Rep[intf.ops.V[A]]) = intf.ops.insertAll(pos,y)
    def remove(pos: Rep[Int])(implicit ctx: SourceContext) = intf.ops.remove(pos)
    def removeAll(pos: Rep[Int], len: Rep[Int])(implicit ctx: SourceContext) = intf.ops.removeAll(pos,len)
    def trim()(implicit ctx: SourceContext) = intf.ops.trim
    def clear()(implicit ctx: SourceContext) = intf.ops.clear    
    
    // //def +(y: Rep[intf.ops.V[A]])(implicit a: Arith[A]) = intf.ops.toIntf(intf.ops.+(y)) // doesn't work, would need dynamic type of ops
    def +(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.vplusToIntf(intf.ops.+(y))    
    def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded2, ctx: SourceContext) = intf.ops.vplusToIntf(intf.ops.+(y))
    def -(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.vminusToIntf(intf.ops.-(y))    
    def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded2, ctx: SourceContext) = intf.ops.vminusToIntf(intf.ops.-(y))
    def *(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.vtimesToIntf(intf.ops.*(y))    
    def *(y: Rep[A])(implicit a: Arith[A], o: Overloaded2, ctx: SourceContext) = intf.ops.vtimesToIntf(intf.ops.*(y))  
    //def *(y: Rep[Matrix[A]])(implicit a: Arith[A],o: Overloaded2) = intf.ops.vtimesToIntf(intf.ops.*(y))
    def **(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.**(y)
    def *:*(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.*:*(y)
    def dot(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.dot(y)
    def /(y: Interface[Vector[A]])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops./(y))
    def /(y: Rep[A])(implicit a: Arith[A], o: Overloaded2, ctx: SourceContext) = intf.ops.toIntf(intf.ops./(y))
    
    def sum(implicit a: Arith[A], ctx: SourceContext) = intf.ops.sum
    def abs(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.abs)
    def exp(implicit a: Arith[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.exp)        
    
    def sort(implicit o: Ordering[A], ctx: SourceContext) = intf.ops.toIntf(intf.ops.sort)
    def min(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.min
    def minIndex(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.minIndex
    def max(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.max
    def maxIndex(implicit o: Ordering[A], mx: HasMinMax[A], ctx: SourceContext) = intf.ops.maxIndex
    def median(implicit o: Ordering[A], ctx: SourceContext) = intf.ops.median
    def :>(y: Interface[Vector[A]])(implicit o: Ordering[A], ctx: SourceContext) = intf.ops.:>(y)
    def :<(y: Interface[Vector[A]])(implicit o: Ordering[A], ctx: SourceContext) = intf.ops.:<(y)
    
    def map[B:Manifest](f: Rep[A] => Rep[B])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.map(f))
    def mmap(f: Rep[A] => Rep[A])(implicit ctx: SourceContext) = intf.ops.wrap(intf.ops.mmap(f))
    def foreach(block: Rep[A] => Rep[Unit])(implicit ctx: SourceContext) = intf.ops.foreach(block)
    def zip[B:Manifest,R:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[R])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.zip(y)(f))
    def mzip[B:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[A])(implicit ctx: SourceContext) = intf.ops.wrap(intf.ops.mzip(y)(f))
    def reduce(f: (Rep[A],Rep[A]) => Rep[A])(implicit a: Arith[A], ctx: SourceContext) = intf.ops.reduce(f)
    def filter(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.filter(pred))
    //def find(pred: Rep[A] => Rep[Boolean]) = intf.ops.vfindToIntf(intf.ops.find(pred))
    def find(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = intf.ops.toIntf(intf.ops.find(pred))
    def count(pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext) = intf.ops.count(pred)
    // def flatMap[B:Manifest](f: Rep[A] => Rep[V[B]]) = intf.ops.toIntf(intf.ops.flatMap(f))
    // def partition(pred: Rep[A] => Rep[Boolean]) = { val a = intf.ops.partition(pred); (intf.ops.toIntf(a._1), intf.ops.toIntf(a._2)) }
    // def groupBy[K:Manifest](pred: Rep[A] => Rep[K]) = intf.ops.toIntf(intf.ops.groupBy(pred))
  }
  
  def EmptyVector[A](implicit mA: Manifest[A], ctx: SourceContext): Rep[DenseVector[A]] = (mA match {
    // these don't allocate any memory
    case Manifest.Double => densevector_empty_double
    case Manifest.Float => densevector_empty_float
    case Manifest.Int => densevector_empty_int
    // allocates a dummy polymorphic class
    case _ => densevector_empty[A]
  }).asInstanceOf[Rep[DenseVector[A]]]

  def ZeroVector[A](length: Rep[Int], isRow: Rep[Boolean] = unit(true))(implicit mA: Manifest[A], ctx: SourceContext): Rep[DenseVector[A]] = (mA match {
    case Manifest.Double => densevector_zero_double(length, isRow)
    case Manifest.Float => densevector_zero_float(length, isRow)
    case Manifest.Int => densevector_zero_int(length, isRow)
    case _ => throw new IllegalArgumentException("No ZeroVector exists of type " + mA)
  }).asInstanceOf[Rep[DenseVector[A]]]

  // object defs
  def vector_obj_range(start: Rep[Int], end: Rep[Int], stride: Rep[Int], isRow: Rep[Boolean])(implicit ctx: SourceContext): Rep[RangeVector]

  // class defs
  def vector_equals[A:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Boolean]
  def vector_slice[A:Manifest,VA:Manifest](x: Interface[Vector[A]], start: Rep[Int], end: Rep[Int])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_contains[A:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext): Rep[Boolean]
  def vector_distinct[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  
  def vector_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_mutable_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_pprint[A:Manifest](x: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def vector_repmat[A:Manifest,MA:Manifest](x: Interface[Vector[A]], i: Rep[Int], j: Rep[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  //def vector_repmat[A:Manifest](x: Interface[Vector[A]], i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]
  def vector_mkstring[A:Manifest](x: Interface[Vector[A]], sep: Rep[String])(implicit ctx: SourceContext): Rep[String]  
  
  def vector_concatenate[A:Manifest,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  
  def vector_plus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_plus_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_plus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA] 
  def vector_plus_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_plusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def vector_plusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit]   
  def vector_minus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_minus_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_minus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA] 
  def vector_minus_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_minusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def vector_minusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] 
  def vector_times[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_times_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_times_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA] 
  def vector_times_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_timesequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def vector_timesequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] 
  //def vector_times_matrix[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[DenseMatrix[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_dot_product[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  def vector_outer[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[DenseMatrix[A]]  
  //def vector_outer[A:Manifest:Arith,MA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext): Rep[MA]
  def vector_divide[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_divide_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_divide_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA] 
  def vector_divide_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Rep[B])(implicit conv: Rep[A] => Rep[B], b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_divideequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Unit]
  def vector_divideequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext): Rep[Unit] 
  
  def vector_sum[A:Manifest:Arith](x: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  def vector_abs[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_exp[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  
  def vector_min[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  def vector_minindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Int]
  def vector_max[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]
  def vector_maxindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[Int]
  def vector_median[A:Manifest:Ordering](x: Interface[Vector[A]])(implicit ctx: SourceContext): Rep[A]  
  
  def vector_map[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Rep[A] => Rep[B])(implicit b: VectorBuilder[B,VB], ctx: SourceContext): Rep[VB]
  def vector_mmap[A:Manifest](x: Interface[Vector[A]], f: Rep[A] => Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def vector_foreach[A:Manifest](x: Interface[Vector[A]], block: Rep[A] => Rep[Unit])(implicit ctx: SourceContext): Rep[Unit]
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest,VR:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit b: VectorBuilder[R,VR], ctx: SourceContext): Rep[VR]
  def vector_mzipwith[A:Manifest,B:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Rep[A],Rep[B]) => Rep[A])(implicit ctx: SourceContext): Rep[Unit]
  def vector_reduce[A:Manifest:Arith](x: Interface[Vector[A]], f: (Rep[A],Rep[A]) => Rep[A])(implicit ctx: SourceContext): Rep[A]
  def vector_filter[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean])(implicit b: VectorBuilder[A,VA], ctx: SourceContext): Rep[VA]
  def vector_find[A:Manifest,VFINDR:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean])(implicit b: VectorBuilder[Int,VFINDR], ctx: SourceContext): Rep[VFINDR]
  def vector_count[A:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean])(implicit ctx: SourceContext): Rep[Int]
  //def vector_flatmap[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Rep[A] => Rep[Vector[B]])(implicit b: VectorBuilder[B,VB]): Rep[VB]
  //def vector_partition[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean]): (Rep[VA], Rep[VA])
  //def vector_groupby[A:Manifest,K:Manifest,VA:Manifest,VVA:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[K])(implicit b: VectorBuilder[VA,VVA]): Rep[VVA]             
}

trait VectorOpsExp extends VectorOps with DeliteCollectionOpsExp with VariablesExp with BaseFatExp {

  this: VectorImplOps with OptiLAExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VectorObjectRange(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean])
    extends Def[RangeVector]

  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)

  case class VectorEquals[A:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])
    extends DeliteOpSingleWithManifest[A,Boolean](reifyEffectsHere(vector_equals_impl(x,y)))
  
  case class VectorSlice[A:Manifest,VA:Manifest](x: Interface[Vector[A]], start: Exp[Int], end: Exp[Int])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(vector_slice_impl[A,VA](x,start,end)))       

  case class VectorContains[A:Manifest](x: Interface[Vector[A]], y: Exp[A])
    extends DeliteOpSingleWithManifest[A,Boolean](reifyEffectsHere(vector_contains_impl[A](x, y)))

  case class VectorDistinct[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(vector_distinct_impl[A,VA](x)))

  case class VectorClone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(vector_clone_impl[A,VA](x)))

  case class VectorPPrint[A](x: Interface[Vector[A]])(block: Block[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)
    // reifyEffects(densevector_pprint_impl[A](x))

  case class VectorMkString[A:Manifest](x: Interface[Vector[A]], sep: Exp[String])
    extends DeliteOpSingleWithManifest[A,String](reifyEffectsHere(vector_mkstring_impl[A](x, sep)))

  case class VectorConcatenate[A:Manifest,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(vector_concatenate_impl[A,VA](x,y)))

  // case class VectorTimesMatrix[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Exp[Matrix[A]])(implicit val b: VectorBuilder[A,VA])
  //     extends DeliteOpSingleWithManifest[A,VA](reifyEffectsHere(vector_times_matrix_impl[A,VA](x,y)))
  
  case class VectorOuter[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])
    extends DeliteOpSingleWithManifest[A,DenseMatrix[A]](reifyEffectsHere(vector_outer_impl[A](x,y))) {
      //TODO: should mixin implicit accessors
      //def m = manifest[A]
      val a = implicitly[Arith[A]]
  }
    
  case class VectorMedian[A:Manifest:Ordering](x: Interface[Vector[A]])
    extends DeliteOpSingleWithManifest[A,A](reifyEffectsHere(vector_median_impl[A](x))) {
      val o = implicitly[Ordering[A]]
  }

  // case class VectorRepmat[A:Manifest](x: Interface[Vector[A]], i: Exp[Int], j: Exp[Int])
  //   extends DeliteOpSingleTask(reifyEffectsHere(vector_repmat_impl[A](x,i,j))) {
  //     
  //   val mA = manifest[A]
  // }
    
  ///////////////////////////////////////
  // implemented via parallel delite ops

  abstract class VectorArithmeticMap[A:Manifest:Arith,VA:Manifest](implicit val b: VectorBuilder[A,VA]) extends DeliteOpMap[A,A,VA] {
    val intf: Interface[Vector[A]]
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]    
    def alloc = b.alloc(intf.length, intf.isRow)
    val size = copyTransformedOrElse(_.size)(intf.length)
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
    val mVA = manifest[VA]   
  }

  abstract class VectorArithmeticZipWith[A:Manifest:Arith,VA:Manifest](implicit val b: VectorBuilder[A,VA]) extends DeliteOpZipWith[A,A,A,VA] {
    val intfA: Interface[Vector[A]]
    val intfB: Interface[Vector[A]]
    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[A]]]    
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)

    val mA = manifest[A]
    val a = implicitly[Arith[A]]
    val mVA = manifest[VA]
  }
  
  abstract class VectorArithmeticIndexedLoop[A:Manifest:Arith] extends DeliteOpIndexedLoop {
    val intf: Interface[Vector[A]]
    val size = copyTransformedOrElse(_.size)(intf.length)
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }
  
  abstract class VectorArithmeticReduce[A:Manifest:Arith] extends DeliteOpReduce[A] {
    val intf: Interface[Vector[A]]
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]    
    val size = copyTransformedOrElse(_.size)(intf.length)
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }
  
  case class VectorPlus[A:Manifest:Arith,VA:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticZipWith[A,VA] {

    def func = (a,b) => a + b
  }

  case class VectorPlusWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpZipWith[A,B,B,VB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[B]]]    
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)
    def func = (a,b) => conv(a) + b
      
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]  
    val mVB = manifest[VB]
  }
  
  case class VectorPlusScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e + y
  }
  
  case class VectorPlusScalarWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intf: Interface[Vector[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpMap[A,B,VB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    def alloc = b.alloc(intf.length, intf.isRow)
    val size = copyTransformedOrElse(_.size)(intf.length)  
    def func = e => conv(e) + y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mVB = manifest[VB]    
  }
  
  case class VectorPlusEquals[A:Manifest:Arith](intf: Interface[Vector[A]], intfB: Interface[Vector[A]])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) + intfB(i) } 
  } 
  
  case class VectorPlusEqualsScalar[A:Manifest:Arith](intf: Interface[Vector[A]], y: Exp[A])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) + y } 
  } 
  
  case class VectorMinus[A:Manifest:Arith,VA:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticZipWith[A,VA] {

    def func = (a,b) => a - b
  }
  
  case class VectorMinusWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpZipWith[A,B,B,VB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[B]]]    
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)  
    def func = (a,b) => conv(a) - b
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mVB = manifest[VB]
  }
  
  case class VectorMinusScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e - y
  }

  case class VectorMinusScalarWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intf: Interface[Vector[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpMap[A,B,VB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    def alloc = b.alloc(intf.length, intf.isRow)
    val size = copyTransformedOrElse(_.size)(intf.length)  
    def func = e => conv(e) - y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mVB = manifest[VB]
  }
  
  case class VectorMinusEquals[A:Manifest:Arith](intf: Interface[Vector[A]], intfB: Interface[Vector[A]])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) - intfB(i) } 
  } 
  
  case class VectorMinusEqualsScalar[A:Manifest:Arith](intf: Interface[Vector[A]], y: Exp[A])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) - y } 
  } 
  
  case class VectorTimes[A:Manifest:Arith,VA:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticZipWith[A,VA] {

    def func = (a,b) => a * b
  }
  
  case class VectorTimesWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpZipWith[A,B,B,VB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[B]]]    
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)  
    def func = (a,b) => conv(a) * b
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]
    val mVB = manifest[VB]    
  }
  
  case class VectorTimesScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e * y
  }
  
  case class VectorTimesScalarWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intf: Interface[Vector[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpMap[A,B,VB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    def alloc = b.alloc(intf.length, intf.isRow)
    val size = copyTransformedOrElse(_.size)(intf.length)  
    def func = e => conv(e) * y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mVB = manifest[VB]
  }
  
  case class VectorTimesEquals[A:Manifest:Arith](intf: Interface[Vector[A]], intfB: Interface[Vector[A]])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) * intfB(i) } 
  } 
  
  case class VectorTimesEqualsScalar[A:Manifest:Arith](intf: Interface[Vector[A]], y: Exp[A])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) * y } 
  } 
  
  case class VectorDotProduct[A:Manifest:Arith](intfA: Interface[Vector[A]], intfB: Interface[Vector[A]])
    extends DeliteOpZipWithReduce[A,A,A] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[A]]]    
    def zip = (a,b) => a*b
    def reduce = (a,b) => a + b
    val size = copyTransformedOrElse(_.size)(intfA.length)
    val zero = implicitly[Arith[A]].empty
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }

  
  case class VectorDivide[A:Manifest:Arith,VA:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticZipWith[A,VA] {

    def func = (a,b) => a / b
  }
  
  case class VectorDivideWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpZipWith[A,B,B,VB] {
    
    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[B]]]    
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)  
    def func = (a,b) => conv(a) / b
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mVB = manifest[VB]
  }

  case class VectorDivideScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e / y
  }
  
  case class VectorDivideScalarWithConvert[A:Manifest,B:Manifest:Arith,VB:Manifest](intf: Interface[Vector[A]], y: Exp[B])(implicit val conv: Exp[A] => Exp[B], val b: VectorBuilder[B,VB])
    extends DeliteOpMap[A,B,VB] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    def alloc = b.alloc(intf.length, intf.isRow)
    val size = copyTransformedOrElse(_.size)(intf.length)
    def func = e => conv(e) / y
    
    val mA = manifest[A]
    val mB = manifest[B]
    val a = implicitly[Arith[B]]    
    val mVB = manifest[VB]
  }  

  case class VectorDivideEquals[A:Manifest:Arith](intf: Interface[Vector[A]], intfB: Interface[Vector[A]])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) / intfB(i) } 
  } 
  
  case class VectorDivideEqualsScalar[A:Manifest:Arith](intf: Interface[Vector[A]], y: Exp[A])
    extends VectorArithmeticIndexedLoop { 

    def func = i => { intf(i) = intf(i) / y } 
  } 
  
  case class VectorSum[A:Manifest:Arith](intf: Interface[Vector[A]]) 
    extends VectorArithmeticReduce[A] {

    val zero = a.empty 
    def func = (a,b) => a + b
  }

  case class VectorAbs[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e.abs
  }

  case class VectorExp[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e.exp
  }
  
  case class VectorMin[A:Manifest:Ordering:HasMinMax](intf: Interface[Vector[A]]) 
    extends DeliteOpReduce[A] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val size = copyTransformedOrElse(_.size)(intf.length)
    val zero = implicitly[HasMinMax[A]].maxValue
    def func = (a,b) => if (a < b) a else b
    
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }

  case class VectorMax[A:Manifest:Ordering:HasMinMax](intf: Interface[Vector[A]]) 
    extends DeliteOpReduce[A] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val size = copyTransformedOrElse(_.size)(intf.length)
    val zero = implicitly[HasMinMax[A]].minValue
    def func = (a,b) => if (a > b) a else b
    
    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]    
  }

  case class VectorMinIndex[A:Manifest:Ordering:HasMinMax](intfB: Interface[Vector[A]]) 
    extends DeliteOpZipWithReduceTuple[Int,A,Int,A] {
    
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val inA = copyTransformedOrElse(_.inA)(unit(0)::intfB.length)
    val size = copyTransformedOrElse(_.size)(intfB.length)
    val zero = (copyTransformedBlockOrElse(_.zero._1)(reifyEffects(unit(0))),copyTransformedBlockOrElse(_.zero._2)(reifyEffects(implicitly[HasMinMax[A]].maxValue))) // 0 sensible? maybe -1?
    def zip = (a,b) => (reifyEffects(a),reifyEffects(b))
    def reduce = (a,b) => (reifyEffects(if (a._2 < b._2) a._1 else b._1), reifyEffects(if (a._2 < b._2) a._2 else b._2))

    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }
  
  case class VectorMaxIndex[A:Manifest:Ordering:HasMinMax](intfB: Interface[Vector[A]]) 
    extends DeliteOpZipWithReduceTuple[Int,A,Int,A] {

    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val inA = copyTransformedOrElse(_.inA)(unit(0)::intfB.length)
    val size = copyTransformedOrElse(_.size)(intfB.length)
    val zero = (copyTransformedBlockOrElse(_.zero._1)(reifyEffects(unit(0))),copyTransformedBlockOrElse(_.zero._2)(reifyEffects(implicitly[HasMinMax[A]].minValue))) // 0 sensible? maybe -1?
    def zip = (a,b) => (reifyEffects(a),reifyEffects(b))
    def reduce = (a,b) => (reifyEffects(if (a._2 > b._2) a._1 else b._1), reifyEffects(if (a._2 > b._2) a._2 else b._2))

    val mA = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }
  
  case class VectorMap[A:Manifest,B:Manifest,VB:Manifest](intf: Interface[Vector[A]], func: Exp[A] => Exp[B])(implicit val b: VectorBuilder[B,VB])
    extends DeliteOpMap[A,B,VB] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val size = copyTransformedOrElse(_.size)(intf.length)
    def alloc = b.alloc(intf.length, intf.isRow)

    val mA = manifest[A]
    val mB = manifest[B]
    val mVB = manifest[VB]
  }

  case class VectorMutableMap[A:Manifest](intf: Interface[Vector[A]], block: Exp[A] => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(intf.length)
    def func = i => intf(i) = block(intf(i))

    val mA = manifest[A]
  }

  case class VectorForeach[A:Manifest](intf: Interface[Vector[A]], func: Exp[A] => Exp[Unit])
    extends DeliteOpForeach[A] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    def sync = n => List()
    val size = copyTransformedOrElse(_.size)(intf.length)
    
    val mA = manifest[A]
  }

  case class VectorZipWith[A:Manifest,B:Manifest,R:Manifest,VR:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]],
                                                                         func: (Exp[A], Exp[B]) => Exp[R])(implicit val b: VectorBuilder[R,VR])
    extends DeliteOpZipWith[A,B,R,VR] {

    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[B]]]  
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)
    
    val mA = manifest[A]
    val mB = manifest[B]
    val mR = manifest[R]
    val mVR = manifest[VR]
  }

  case class VectorMutableZipWith[A:Manifest,B:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]],
                                                        block: (Exp[A], Exp[B]) => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(intfA.length)
    def func = i => intfA(i) = block(intfA(i),intfB(i))
    
    val mA = manifest[A]
    val mB = manifest[B]
  }

  // note: we may want to factor 'HasEmpty' out of 'Arith' to make this more general, if the need arises.
  case class VectorReduce[A:Manifest:Arith](intf: Interface[Vector[A]], func: (Exp[A], Exp[A]) => Exp[A])
    extends DeliteOpReduce[A] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val size = copyTransformedOrElse(_.size)(intf.length)
    val zero = implicitly[Arith[A]].empty
    
    val mA = manifest[A]
    val a = implicitly[Arith[A]]
  }

  case class VectorFilter[A:Manifest,VA:Manifest](intf: Interface[Vector[A]], cond: Exp[A] => Exp[Boolean])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpFilter[A,A,VA] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    def alloc = b.alloc(unit(0), intf.isRow)
    def func = e => e 
    val size = copyTransformedOrElse(_.size)(intf.length)

    val mA = manifest[A]  
    val mVA = manifest[VA]
  }

  case class VectorFind[A:Manifest,VFINDR:Manifest](intf: Interface[Vector[A]], cond: Exp[A] => Exp[Boolean])(implicit val b: VectorBuilder[Int,VFINDR])
    extends DeliteOpFilter[A,Int,VFINDR] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    def alloc = b.alloc(unit(0), unit(true))
    def func = e => v // should we make available and use a helper function like index(e)?
    val size = copyTransformedOrElse(_.size)(intf.length)

    val mA = manifest[A]  
    val mVA = manifest[VFINDR]
  }

  case class VectorCount[A:Manifest](intf: Interface[Vector[A]], cond: Exp[A] => Exp[Boolean]) 
    extends DeliteOpFilterReduce[A,Int] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val size = copyTransformedOrElse(_.size)(intf.length)
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b   

    val mA = manifest[A]
  }
  
  case class VectorRepmat[A:Manifest,MA:Manifest](x: Interface[Vector[A]], i: Exp[Int], j: Exp[Int])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpMap[Int,A,MA] {
  
    def alloc = b.alloc(i, x.length*j)
    val in = copyTransformedOrElse(_.in)(unit(0)::size)
    val size = copyTransformedOrElse(_.size)(x.length*i*j)
    def func = i => x(i%x.length)
  
    val mA = manifest[A]
    val mMA = manifest[MA]
  }

  /*
  case class VectorOuter[A:Manifest:Arith,MA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit val b: MatrixBuilder[A,MA])
    extends DeliteOpMap[Int,A,MA] {

    def alloc = b.alloc(x.length, y.length)
    val in = copyTransformedOrElse(_.in)(unit(0)::size)
    val size = copyTransformedOrElse(_.size)(x.length*y.length)
    def func = i => x(i/y.length) * y(i%y.length)

    val mA = manifest[A]
    val mMA = manifest[MA]
    val a = implicitly[Arith[A]]
  }
  */
  /*
  case class VectorFlatMap[A:Manifest,B:Manifest,VB:Manifest](in: Interface[Vector[A]], map: Exp[A] => Exp[VB])(implicit b: VectorBuilder[B,VB])
    extends DeliteOpMapReduce2[A,VB] {

    val size = in.length
    val zero = EmptyVector[B]
    def reduce = (a,b) => a ++ b
  }
  */
  
  /////////////////////
  // delite collection
    
  def isDenseVec[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.Type.erasure,classOf[DenseVector[A]])  
  def asDenseVec[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[DenseVector[A]]]
  
  def isRange[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.Type.erasure,classOf[RangeVector])  
  def asRange[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[RangeVector]]
  
  def isView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = isSubtype(x.Type.erasure,classOf[VectorView[A]])
  def asView[A](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = x.asInstanceOf[Exp[VectorView[A]]]  
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]])(implicit ctx: SourceContext) = { 
    if (isDenseVec(x)) asDenseVec(x).length
    else if (isRange(x)) asRange(x).length
    else if (isView(x)) asView(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int])(implicit ctx: SourceContext) = {
    if (isDenseVec(x)) asDenseVec(x).apply(n)
    else if (isRange(x)) (asRange(x).apply(n)).asInstanceOf[Exp[A]]
    else if (isView(x)) asView(x).apply(n)
    else {
      Predef.println("couldn't find dc_apply for " + x.Type.toString)
      Predef.println("isView: " + isView(x))
      super.dc_apply(x,n)    
    }
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A])(implicit ctx: SourceContext) = {
    if (isDenseVec(x)) asDenseVec(x).update(n,y)
    else if (isRange(x)) asRange(x).update(n,y.asInstanceOf[Exp[Int]])
    else if (isView(x)) asView(x).update(n,y)
    else super.dc_update(x,n,y)        
  }
  
  
  /////////////////////
  // object interface

  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(VectorObjectRange(start, end, stride, isRow))


  /////////////////////
  // class interface
  
  def vector_equals[A:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorEquals(x,y))
  def vector_slice[A:Manifest,VA:Manifest](x: Interface[Vector[A]], start: Exp[Int], end: Exp[Int])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorSlice[A,VA](x, start, end))
  def vector_contains[A:Manifest](x: Interface[Vector[A]], y: Exp[A])(implicit ctx: SourceContext) = reflectPure(VectorContains(x,y))
  def vector_distinct[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorDistinct[A,VA](x))
  
  def vector_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorClone[A,VA](x))
  def vector_mutable_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectMutable(VectorClone[A,VA](x))
  def vector_pprint[A:Manifest](x: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectEffect(VectorPPrint(x)(reifyEffectsHere(vector_pprint_impl[A](x))))  
  def vector_repmat[A:Manifest,MA:Manifest](x: Interface[Vector[A]], i: Rep[Int], j: Rep[Int])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(VectorRepmat[A,MA](x,i,j))
  //def vector_repmat[A:Manifest](x: Interface[Vector[A]], i: Rep[Int], j: Rep[Int])(implicit ctx: SourceContext) = reflectPure(VectorRepmat[A](x,i,j))
  def vector_mkstring[A:Manifest](x: Interface[Vector[A]], sep: Rep[String])(implicit ctx: SourceContext) = reflectPure(VectorMkString(x,sep))
  
  def vector_concatenate[A:Manifest,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorConcatenate[A,VA](x,y))
  
  def vector_plus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorPlus[A,VA](x,y))
  def vector_plus_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorPlusWithConvert[A,B,VB](x,y))
  def vector_plus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorPlusScalar[A,VA](x,y))
  def vector_plus_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorPlusScalarWithConvert[A,B,VB](x,y))
  def vector_plusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorPlusEquals(x,y))
  def vector_plusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorPlusEqualsScalar(x,y))
  def vector_minus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorMinus[A,VA](x,y))
  def vector_minus_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorMinusWithConvert[A,B,VB](x,y))
  def vector_minus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorMinusScalar[A,VA](x,y))
  def vector_minus_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorMinusScalarWithConvert[A,B,VB](x,y))
  def vector_minusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorMinusEquals(x,y))
  def vector_minusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorMinusEqualsScalar(x,y))  
  def vector_times[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorTimes[A,VA](x,y))
  def vector_times_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorTimesWithConvert[A,B,VB](x,y))
  def vector_times_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorTimesScalar[A,VA](x,y))
  def vector_times_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorTimesScalarWithConvert[A,B,VB](x,y))
  def vector_timesequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorTimesEquals(x,y))
  def vector_timesequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorTimesEqualsScalar(x,y))  
  //def vector_times_matrix[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Exp[Matrix[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorTimesMatrix[A,VA](x,y))
  def vector_dot_product[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorDotProduct(x,y))
  def vector_outer[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorOuter(x,y))
  //def vector_outer[A:Manifest:Arith,MA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: MatrixBuilder[A,MA], ctx: SourceContext) = reflectPure(VectorOuter[A,MA](x,y))
  def vector_divide[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorDivide[A,VA](x,y))
  def vector_divide_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorDivideWithConvert[A,B,VB](x,y))
  def vector_divide_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorDivideScalar[A,VA](x,y))
  def vector_divide_scalar_withconvert[A:Manifest,B:Manifest:Arith,VB:Manifest](x: Interface[Vector[A]], y: Exp[B])(implicit conv: Exp[A] => Exp[B], b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorDivideScalarWithConvert[A,B,VB](x,y))
  def vector_divideequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorDivideEquals(x,y))
  def vector_divideequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorDivideEqualsScalar(x,y))  

  def vector_sum[A:Manifest:Arith](x: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorSum(x))
  def vector_abs[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorAbs[A,VA](x))
  def vector_exp[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorExp[A,VA](x))
  
  def vector_min[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorMin(x))
  def vector_minindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorMinIndex(x))
  def vector_max[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorMax(x))
  def vector_maxindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorMaxIndex(x))
  def vector_median[A:Manifest:Ordering](x: Interface[Vector[A]])(implicit ctx: SourceContext) = reflectPure(VectorMedian(x))
  
  def vector_map[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Exp[A] => Exp[B])(implicit b: VectorBuilder[B,VB], ctx: SourceContext) = reflectPure(VectorMap[A,B,VB](x,f)) // TODO: effect if func effectful!
  def vector_mmap[A:Manifest](x: Interface[Vector[A]], f: Exp[A] => Exp[A])(implicit ctx: SourceContext) = reflectWrite(x.ops.elem)(VectorMutableMap(x,f)) // TODO: effect if func effectful!
  def vector_foreach[A:Manifest](x: Interface[Vector[A]], block: Exp[A] => Exp[Unit])(implicit ctx: SourceContext) = {
    val vf = VectorForeach(x, block) //reflectEffect(VectorForeach(x, block)) 
    reflectEffect(vf, summarizeEffects(vf.body.asInstanceOf[DeliteForeachElem[A]].func).star andAlso Simple())
  }
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest,VR:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit b: VectorBuilder[R,VR], ctx: SourceContext) = {
    reflectPure(VectorZipWith[A,B,R,VR](x,y,f))
  }
  def vector_mzipwith[A:Manifest,B:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Exp[A],Exp[B]) => Exp[A])(implicit ctx: SourceContext) = {
    reflectWrite(x.ops.elem)(VectorMutableZipWith(x,y,f))
  }
  def vector_reduce[A:Manifest:Arith](x: Interface[Vector[A]], f: (Exp[A],Exp[A]) => Exp[A])(implicit ctx: SourceContext) = reflectPure(VectorReduce(x, f))
  def vector_filter[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean])(implicit b: VectorBuilder[A,VA], ctx: SourceContext) = reflectPure(VectorFilter[A,VA](x, pred))
  def vector_find[A:Manifest,VFINDR:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean])(implicit b: VectorBuilder[Int,VFINDR], ctx: SourceContext) = reflectPure(VectorFind[A,VFINDR](x, pred))
  def vector_count[A:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean])(implicit ctx: SourceContext) = reflectPure(VectorCount(x, pred))
  //def vector_flatmap[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Exp[A] => Exp[Vector[B]])(implicit b: VectorBuilder[B,VB]) = reflectPure(VectorFlatMap(x, f))
  //def vector_partition[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean]) = t2(reflectPure(VectorPartition(x, pred)))
  //def vector_groupby[A:Manifest,K:Manifest,VA:Manifest,VVA:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[K])(implicit b: VectorBuilder[VA,VVA]) = reflectPure(VectorGroupBy(x, pred))
  

  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    // implemented via method on real data structure
    case VectorObjectRange(start, end, stride, isRow) => reflectPure(VectorObjectRange(start,end,stride,isRow))(mtype(manifest[A]),implicitly[SourceContext])
    
    // implemented as DeliteOpSingleTask and DeliteOpLoop    
    case e@VectorEquals(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorEquals(f(x),f(y))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorSlice(x,start,end) => reflectPure(new { override val original = Some(f,e) } with VectorSlice(f(x),f(start),f(end))(e.mA,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorContains(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorContains(f(x),f(y))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDistinct(x) => reflectPure(new { override val original = Some(f,e) } with VectorDistinct(f(x))(e.mA, e.mR, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorClone(x) => reflectPure(new { override val original = Some(f,e) } with VectorClone(f(x))(e.mA, e.mR, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorRepmat(x,i,j) => reflectPure(new { override val original = Some(f,e) } with VectorRepmat(f(x),f(i),f(j))(e.mA,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    //case e@VectorRepmat(x,i,j) => reflectPure(new { override val original = Some(f,e) } with VectorRepmat(f(x),f(i),f(j))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMkString(x,sep) => reflectPure(new { override val original = Some(f,e) } with VectorMkString(f(x),f(sep))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorConcatenate(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorConcatenate(f(x),f(y))(e.mA, e.mR, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMedian(x) => reflectPure(new { override val original = Some(f,e) } with VectorMedian(f(x))(e.mA, e.o))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@VectorOuter(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorOuter(f(x),f(y))(e.mA, e.a))(mtype(manifest[A]),implicitly[SourceContext])    
    //case e@VectorOuter(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorOuter(f(x),f(y))(e.mA,e.a,e.mMA,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorPlus(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorPlusWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorPlusWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorPlusScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorPlusScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorPlusScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorPlusScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorMinus(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMinusWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorMinusWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMinusScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorMinusScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMinusScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorMinusScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorTimes(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorTimesWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorTimesWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorTimesScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorTimesScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorTimesScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorTimesScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDotProduct(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDotProduct(f(x),f(y))(e.mA, e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDivide(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDivide(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDivideWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDivideWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDivideScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDivideScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDivideScalarWithConvert(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDivideScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorSum(x) => reflectPure(new { override val original = Some(f,e) } with VectorSum(f(x))(e.mA, e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorAbs(x) => reflectPure(new { override val original = Some(f,e) } with VectorAbs(f(x))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorExp(x) => reflectPure(new { override val original = Some(f,e) } with VectorExp(f(x))(e.mA, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMin(x) => reflectPure(new { override val original = Some(f,e) } with VectorMin(f(x))(e.mA,e.o,e.p))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@VectorMax(x) => reflectPure(new { override val original = Some(f,e) } with VectorMax(f(x))(e.mA,e.o,e.p))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@VectorMinIndex(x) => reflectPure(new { override val original = Some(f,e) } with VectorMinIndex(f(x))(e.mA,e.o,e.p))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@VectorMaxIndex(x) => reflectPure(new { override val original = Some(f,e) } with VectorMaxIndex(f(x))(e.mA,e.o,e.p))(mtype(manifest[A]),implicitly[SourceContext])    
    case e@VectorMap(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorMap(f(x),f(p))(e.mA,e.mB,e.mVB,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorZipWith(x,y,func) => reflectPure(new { override val original = Some(f,e) } with VectorZipWith(f(x),f(y),f(func))(e.mA,e.mB,e.mR,e.mVR,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorReduce(x,func) => reflectPure(new { override val original = Some(f,e) } with VectorReduce(f(x),f(func))(e.mA, e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorFilter(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorFilter(f(x),f(p))(e.mA, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorFind(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorFind(f(x),f(p))(e.mA, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorCount(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorCount(f(x),f(p))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])
    
    // reflected
    case Reflect(e@VectorEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorEquals(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorSlice(x,start,end), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorSlice(f(x),f(start),f(end))(e.mA, e.mR, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorContains(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorContains(f(x),f(y))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDistinct(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDistinct(f(x))(e.mA, e.mR, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorClone(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorClone(f(x))(e.mA, e.mR, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorRepmat(x,i,j), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorRepmat(f(x),f(i),f(j))(e.mA,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    //case Reflect(e@VectorRepmat(x,i,j), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorRepmat(f(x),f(i),f(j))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMkString(x,sep), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMkString(f(x),f(sep))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorConcatenate(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorConcatenate(f(x),f(y))(e.mA, e.mR, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMedian(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMedian(f(x))(e.mA, e.o), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorOuter(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorOuter(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    //case Reflect(e@VectorOuter(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorOuter(f(x),f(y))(e.mA,e.a,e.mMA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlus(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlusWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlusWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlusScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlusScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlusScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlusScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlusEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlusEquals(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlusEqualsScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlusEqualsScalar(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMinus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinus(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMinusWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinusWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMinusScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinusScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMinusScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinusScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMinusEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinusEquals(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMinusEqualsScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinusEqualsScalar(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorTimes(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorTimes(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorTimesWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorTimesWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorTimesScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorTimesScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorTimesScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorTimesScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorTimesEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorTimesEquals(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorTimesEqualsScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorTimesEqualsScalar(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDotProduct(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDotProduct(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDivide(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDivide(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDivideWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDivideWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDivideScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDivideScalar(f(x),f(y))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDivideScalarWithConvert(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDivideScalarWithConvert(f(x),f(y))(e.mA, e.mB, e.a, e.mVB, e.conv, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDivideEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDivideEquals(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDivideEqualsScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDivideEqualsScalar(f(x),f(y))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorSum(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorSum(f(x))(e.mA, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorAbs(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorAbs(f(x))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorExp(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorExp(f(x))(e.mA, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@VectorMin(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMin(f(x))(e.mA, e.o, e.p), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@VectorMax(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMax(f(x))(e.mA, e.o, e.p), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@VectorMinIndex(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinIndex(f(x))(e.mA, e.o, e.p), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@VectorMaxIndex(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMaxIndex(f(x))(e.mA, e.o, e.p), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@VectorMap(a,p), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMap(f(a),f(p))(e.mA,e.mB,e.mVB,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorForeach(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorForeach(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorZipWith(x,y,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorZipWith(f(x),f(y),f(func))(e.mA,e.mB,e.mR,e.mVR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorReduce(x,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorReduce(f(x),f(func))(e.mA,e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorFilter(x,p), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorFilter(f(x),f(p))(e.mA,e.mVA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorFind(x,p), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorFind(f(x),f(p))(e.mA,e.mVA,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorCount(x,p), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorCount(f(x),f(p))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMutableMap(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMutableMap(f(x),f(g))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMutableZipWith(x,y,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMutableZipWith(f(x),f(y),f(func))(e.mA,e.mB), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPPrint(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPPrint(f(x))(f(e.block)), mapOver(f,u), f(es)))(mtype(manifest[A]))

    // allocations
    case Reflect(e@VectorObjectRange(s,o,d,r), u, es) => reflectMirrored(Reflect(VectorObjectRange(f(s),f(o),f(d),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??


  /////////////////////
  // aliases and sharing

  // TODO: precise sharing info for other IR types (default is conservative)

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case VectorRepmat(a,i,j) => Nil
    case VectorClone(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case VectorRepmat(a,i,j) => Nil
    case VectorClone(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case VectorRepmat(a,i,j) => Nil
    case VectorClone(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case VectorRepmat(a,i,j) => syms(a)
    case VectorClone(a) => syms(a)
    case _ => super.copySyms(e)
  }  
}

trait VectorOpsExpOpt extends VectorOpsExp { this: OptiLAExp =>

  // TODO aks: debug pattern matching with interfaces
  /*
  override def vector_plus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit bldr: VectorBuilder[A,VA]) = (x.ops.elem, y.ops.elem) match {
    // (TB + TD) == T(B + D)
    case (Def(t1@VectorTimes(a, b)), Def(t2@VectorTimes(c, d))) if (a == c) => vector_times[A,VA](x.ops.toIntf(a), vector_plus[A,VA](x.ops.wrap(b),y.ops.wrap(d)))
    // ...
    case _ => super.vector_plus(x, y)
  }
  
  override def vector_plusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = (x, y) match {
    // remove runtime check on zero vector being same length as argument
    case (a, Def(VectorObjectZeros(len))) => ()
    //case (Def(VectorObjectZeros(len)), b) => b  // this is unsafe because we lose the effectful operation (e.g. accumulation)
    case _ => super.vector_plusequals(x,y)
  }

  override def vector_times[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = (x, y) match {
    case _ => super.vector_times(x, y)
  }
  */
  override def vector_equals[A:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit ctx: SourceContext) = (x.ops.elem, y.ops.elem) match {
    case (a,b) if (a == b) => unit(true) // same symbol
    case _ => super.vector_equals(x,y)
  }
  
}

trait BaseGenVectorOps extends GenericFatCodegen {
  val IR: VectorOpsExp
  import IR._

}

trait ScalaGenVectorOps extends BaseGenVectorOps with ScalaGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
//     case VectorObjectRange(start, end, stride, isRow) => emitValDef(sym, "new generated.scala.RangeVectorImpl(" + quote(start) + "," + quote(end) + "," + quote(stride) + "," + quote(isRow) + ")")
    case VectorObjectRange(start, end, stride, isRow) => emitValDef(sym, "new generated.scala.RangeVector(" + quote(start) + "," + quote(end) + "," + quote(stride) + "," + quote(isRow) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenVectorOps extends BaseGenVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case VectorObjectRange(start, end, stride, isRow) => stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,%s,%s);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(start),quote(end),quote(stride),quote(isRow)))

    /* Specialized CUDA code generations for DeliteOpSingleTasks */
    /*
    case VectorObjectZeros(len) =>
        currDim += 1
        val currDimStr = getCurrDimStr()
        setCurrDimLength(quote(len))
        emitVectorAlloc(sym,"%s".format(quote(len)),"true",false) //needs to allocate with new symbol
        stream.println(addTab()+"if(%s < %s) {".format(currDimStr,quote(len)))
        tabWidth += 1
        stream.println(addTab()+"%s.update(%s,0);".format(quote(sym),currDimStr))
        tabWidth -= 1
        stream.println(addTab()+"}")
        currDim -= 1
    */
    
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenVectorOps extends BaseGenVectorOps with OpenCLGenFat with OpenCLGenDataStruct {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVectorOps extends BaseGenVectorOps with CGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

