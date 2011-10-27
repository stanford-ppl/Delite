package ppl.dsl.optila.vector

import java.io.{PrintWriter}
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import scala.reflect.SourceContext

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
  
  // TODO (aks): retained for initial app compability; remove and transition to using DenseVector and SparseVector objects directly
  object Vector {
    def apply[A:Manifest](len: Int, isRow: Boolean) = densevector_obj_new(unit(len), unit(isRow)) // needed to resolve ambiguities
    def apply[A](len: Rep[Int], isRow: Rep[Boolean])(implicit mA: Manifest[A], o: Overloaded1) = densevector_obj_new(len, isRow)
//    def apply[A:Manifest](xs: A*) = {
//      val out = densevector_obj_new[A](unit(0),unit(true))
//      // interpreted (not lifted)
//      xs.foreach { out += unit(_) }
//      out
//    }
    def apply[A](xs: Rep[A]*)(implicit mA: Manifest[A], o: Overloaded2) = {
      val out = densevector_obj_new[A](unit(0),unit(true))
      // interpreted (not lifted)
      xs.foreach { out += _ }
      out.unsafeImmutable // return immutable object
    }

    def dense[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) = densevector_obj_new(len, isRow)//dense_densevector_obj_new(len, isRow)
    //def sparse[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]) = sparsevector_obj_new(len, isRow)
    
    def flatten[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]]) = densevector_obj_flatten(pieces)
    def ones(len: Rep[Int]) = densevector_obj_ones(len)
    def onesf(len: Rep[Int]) = densevector_obj_onesf(len)
    def zeros(len: Rep[Int]) = densevector_obj_zeros(len)
    def zerosf(len: Rep[Int]) = densevector_obj_zerosf(len)
    def rand(len: Rep[Int]) = densevector_obj_rand(len)
    def randf(len: Rep[Int]) = densevector_obj_randf(len)
    def range(start: Rep[Int], end: Rep[Int], stride: Rep[Int] = 1, isRow: Rep[Boolean] = unit(true)) =
      vector_obj_range(start, end, stride, isRow)
    def uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean] = unit(true)) =
      densevector_obj_uniform(start, step_size, end, isRow)
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
    implicit def mV[B:Manifest]: Manifest[V[B]]         
    implicit def toOps[B:Manifest](x: Rep[V[B]]): VecOpsCls[B]
    implicit def toIntf[B:Manifest](x: Rep[V[B]]): Interface[Vector[B]]        
    implicit def builder[B:Manifest]: VectorBuilder[B,V[B]]    
    
    type VA = V[A] // temporary for easy compatibility with old stuff
    
    type Self <: Vector[A]
    implicit def wrap(x: Rep[Self]): Interface[Vector[A]]
    val elem: Rep[Self] 
    val x = elem
    
    // DeliteCollection
    def dcSize = length
    def dcApply(n: Rep[Int]): Rep[A] = apply(n)
    def dcUpdate(n: Rep[Int], y: Rep[A]) = update(n,y)
    
    // conversions
    def toBoolean(implicit conv: Rep[A] => Rep[Boolean]) =  map(e => conv(e))
    def toDouble(implicit conv: Rep[A] => Rep[Double]) =  map(e => conv(e))
    def toFloat(implicit conv: Rep[A] => Rep[Float]) = map(e => conv(e))
    def toInt(implicit conv: Rep[A] => Rep[Int]) = map(e => conv(e))
    def toLong(implicit conv: Rep[A] => Rep[Long]) = map(e => conv(e))
    
    // accessors
    def length: Rep[Int] 
    def isRow: Rep[Boolean] 
    def apply(n: Rep[Int]): Rep[A] 
    def isEmpty = length == 0
    def first = apply(0)
    def last = apply(repArithToArithOps(length) - 1) // TODO: why doesn't this get invoked implicitly?
    def indices = (0::length)
    def drop(count: Rep[Int]) = slice(count, length)
    def take(count: Rep[Int]) = slice(0, count)
    def slice(start: Rep[Int], end: Rep[Int]): Rep[VA] = vector_slice[A,VA](x, start, end)
    def contains(y: Rep[A]): Rep[Boolean] = vector_contains(x,y)
    def distinct: Rep[VA] = vector_distinct[A,VA](x)  
    
    // general
    def t: Rep[VA] // TODO: move to type-system
    def mt(): Rep[VA]
    def cloneL(): Rep[VA] = vector_clone[A,VA](x) 
    def mutable(): Rep[VA] = vector_mutable_clone[A,VA](x)
    def pprint(): Rep[Unit] = vector_pprint(x)
    def replicate(i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]] = vector_repmat(x,i,j)
    def mkString(sep: Rep[String] = unit("")): Rep[String] = vector_mkstring(x, sep)      
    
    // data operations
    // most of these these can't operate on Interfaces because they happen at runtime right now...
    // (would need to use a more generic setter field, but Struct should make this obsolete anyways)    
    
    // TODO: should these be moved to another interface, e.g. MutableVecInterface? how do we deal with immutable vectors that
    // don't implement these (e.g. RangeVector). Instead of Interface, would clients require a MutableInterface[...]? (yet another type constructor... :( )
    def update(n: Rep[Int], y: Rep[A]): Rep[Unit]
    def +=(y: Rep[A]): Rep[Unit] = insert(length,y)
    def ++(y: Interface[Vector[A]]): Rep[VA] = vector_concatenate[A,VA](x,y)    
    def ++=(y: Rep[VA]) = insertAll(length,y)
    def copyFrom(pos: Rep[Int], y: Rep[VA]): Rep[Unit]
    def insert(pos: Rep[Int], y: Rep[A]): Rep[Unit]
    def insertAll(pos: Rep[Int], y: Rep[VA]): Rep[Unit]
    def remove(pos: Rep[Int]) = removeAll(pos,1)
    def removeAll(pos: Rep[Int], len: Rep[Int]): Rep[Unit]
    def trim(): Rep[Unit]
    def clear(): Rep[Unit]
    
    // arithmetic operations
    
    // we only need to go through this gymnastic hack when we have different return values for different ops;
    // usually this would just return Rep[VA] (i.e. the same as the lhs)
    // would be really nice if we could use a default value here (VA), but the overrides don't seem to work...
    // could overload for a more specific static type, but can't override +(y: Interface[Vector[A]]) in SparseVecOps because the return value Dense is not a subtype of VA
    type VPLUSR
    implicit val mVPLUSR: Manifest[VPLUSR]
    implicit val vplusBuilder: VectorBuilder[A,VPLUSR]    
    def vplusToIntf(x: Rep[VPLUSR]): Interface[Vector[A]]
    //val plusInfo: OpInfo[A,VPLUSR,Interface[Vector[A]]]    
    //def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_plus[A,VPLUSR](x,y)(manifest[A], implicitly[Arith[A]], plusInfo.mR, plusInfo.b)
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_plus[A,VPLUSR](x,y)
    def +(y: Rep[VA])(implicit a: Arith[A]): Rep[VA] = vector_plus[A,VA](x,y) // needed for Arith        
    def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_plus_scalar[A,VPLUSR](x,y) 
    def +=(y: Interface[Vector[A]])(implicit a: Arith[A]) = { vector_plusequals[A](x,y); elem }
    def +=(y: Rep[VA])(implicit a: Arith[A]) = { vector_plusequals[A](x,y); elem }
    def :+=(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_plusequals_scalar[A](x,y) 
    
    type VMINUSR
    implicit val mVMINUSR: Manifest[VMINUSR]
    implicit val vminusBuilder: VectorBuilder[A,VMINUSR]    
    def vminusToIntf(x: Rep[VMINUSR]): Interface[Vector[A]]
    def -(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_minus[A,VMINUSR](x,y)
    def -(y: Rep[VA])(implicit a: Arith[A]) = vector_minus[A,VA](x,y)
    def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_minus_scalar[A,VMINUSR](x,y)
    def -=(y: Interface[Vector[A]])(implicit a: Arith[A]) = { vector_minusequals[A](x,y); x }
    def -=(y: Rep[VA])(implicit a: Arith[A]) = { vector_minusequals[A](x,y); x }    
    def -=(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_minusequals_scalar[A](x,y)
    
    type VTIMESR
    implicit val mVTIMESR: Manifest[VTIMESR]
    implicit val vtimesBuilder: VectorBuilder[A,VTIMESR]    
    def vtimesToIntf(x: Rep[VTIMESR]): Interface[Vector[A]]    
    //def *[B](y: Rep[DenseVector[B]])(implicit mB: Manifest[B], a: Arith[A], conv: Rep[B] => Rep[A]) = densevector_times_withconvert(x,y,conv)
    //def *[B](y: Rep[DenseVector[B]])(implicit mB: Manifest[B], aB: Arith[B], conv: Rep[A] => Rep[B], o: Overloaded1) = densevector_times_withconvertright(x,y,conv)    
    // TODO: need to extend Arith to support this using CanXX dispatch
    // Rep[DenseVector[Double]] * Rep[RangeVector] (Rep[DenseVector[Double]] * Interface[Vector[Int]])    
    def *(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_times[A,VTIMESR](x,y)        
    def *(y: Rep[VA])(implicit a: Arith[A]) = vector_times[A,VA](x,y)
    def *(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_times_scalar[A,VTIMESR](x,y)
    def *=(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_timesequals[A](x,y)    
    def *=(y: Rep[VA])(implicit a: Arith[A]) = vector_timesequals[A](x,y)
    def *=(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_timesequals_scalar[A](x,y)    
    //def *(y: Rep[Matrix[A]])(implicit a: Arith[A],o: Overloaded2) = vector_times_matrix[A,VTIMESR](x,y)
    def **(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_outer[A](x,y)
    def *:*(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_dot_product(x,y)
    def dot(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_dot_product(x,y)

    def /(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_divide[A,VA](x,y)    
    def /(y: Rep[VA])(implicit a: Arith[A]) = vector_divide[A,VA](x,y)
    def /(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_divide_scalar[A,VA](x,y)    
    def /=(y: Interface[Vector[A]])(implicit a: Arith[A]) = vector_divideequals[A](x,y)    
    def /=(y: Rep[VA])(implicit a: Arith[A]) = vector_divideequals[A](x,y)
    def /=(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = vector_divideequals_scalar[A](x,y)
    
    def sum(implicit a: Arith[A]) = vector_sum(x)
    def abs(implicit a: Arith[A]) = vector_abs[A,VA](x)
    def exp(implicit a: Arith[A]) = vector_exp[A,VA](x)    
    
    def sort(implicit o: Ordering[A]): Rep[VA]
    def min(implicit o: Ordering[A], mx: HasMinMax[A]) = vector_min(x)
    def minIndex(implicit o: Ordering[A], mx: HasMinMax[A]) = vector_minindex(x)
    def max(implicit o: Ordering[A], mx: HasMinMax[A]) = vector_max(x)
    def maxIndex(implicit o: Ordering[A], mx: HasMinMax[A]) = vector_maxindex(x)
    def median(implicit o: Ordering[A]) = vector_median(x)
    def :>(y: Interface[Vector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a > b }
    def :<(y: Interface[Vector[A]])(implicit o: Ordering[A]) = zip(y) { (a,b) => a < b }    
    
    // bulk operations
    def map[B:Manifest](f: Rep[A] => Rep[B]): Rep[V[B]] = vector_map[A,B,V[B]](x,f)
    def mmap(f: Rep[A] => Rep[A]): Rep[Self] = { vector_mmap(x,f); elem }
    def foreach(block: Rep[A] => Rep[Unit]): Rep[Unit] = vector_foreach(x, block)
    def zip[B:Manifest,R:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[R]): Rep[V[R]] = vector_zipwith[A,B,R,V[R]](x,y,f)
    def mzip[B:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[A]): Rep[Self] = { vector_mzipwith(x,y,f); elem }
    def reduce(f: (Rep[A],Rep[A]) => Rep[A])(implicit a: Arith[A]): Rep[A] = vector_reduce(x,f)
    def filter(pred: Rep[A] => Rep[Boolean]): Rep[VA] = vector_filter[A,VA](x,pred)
    
    // type VFINDR
    // implicit val mVFINDR: Manifest[VFINDR]
    // implicit val vfindBuilder: VectorBuilder[Int,VFINDR]    
    // def vfindToIntf(x: Rep[VFINDR]): Interface[Vector[Int]]        
    //def find(pred: Rep[A] => Rep[Boolean]): Rep[VFINDR] = vector_find[A,VFINDR](x,pred)
    def find(pred: Rep[A] => Rep[Boolean]): Rep[V[Int]] = vector_find[A,V[Int]](x,pred)
    
    def count(pred: Rep[A] => Rep[Boolean]): Rep[Int] = vector_count(x, pred)
    // def flatMap[B:Manifest](f: Rep[A] => Rep[V[B]]): Rep[V[B]] = vector_flatmap[A,B,V[B]](x,f)
    // def partition(pred: Rep[A] => Rep[Boolean]): (Rep[VA], Rep[VA])  = vector_partition[A,VA](x,pred)
    // def groupBy[K:Manifest](pred: Rep[A] => Rep[K]): Rep[V[VA]] = vector_groupby[A,K,VA,V[VA]](x,pred)                        
  }
  
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
    def toLong(implicit conv: Rep[A] => Rep[Long]) = intf.ops.toIntf(intf.ops.toLong)
    
    def length = intf.ops.length
    def isRow = intf.ops.isRow    
    def apply(n: Rep[Int]) = intf.ops.apply(n)
    def isEmpty = intf.ops.isEmpty
    def first = intf.ops.first
    def last = intf.ops.last
    def indices = intf.ops.indices
    def drop(count: Rep[Int]) = intf.ops.toIntf(intf.ops.drop(count))
    def take(count: Rep[Int]) = intf.ops.toIntf(intf.ops.take(count))
    def slice(start: Rep[Int], end: Rep[Int]) = intf.ops.toIntf(intf.ops.slice(start,end))
    def contains(y: Rep[A]): Rep[Boolean] = intf.ops.contains(y)
    def distinct = intf.ops.toIntf(intf.ops.distinct)    
    
    def t = intf.ops.t
    def mt() = intf.ops.mt
    def cloneL() = intf.ops.toIntf(intf.ops.cloneL)
    def mutable() = intf.ops.toIntf(intf.ops.mutable)
    def pprint() = intf.ops.pprint
    def replicate(i: Rep[Int], j: Rep[Int]) = intf.ops.replicate(i,j)
    def mkString(sep: Rep[String] = unit("")) = intf.ops.mkString(sep)
    
    def update(n: Rep[Int], y: Rep[A]) = intf.ops.update(n,y)
    def +=(y: Rep[A]) = intf.ops.+=(y)    
    def ++(y: Interface[Vector[A]]) = intf.ops.toIntf(intf.ops.++(y))
    //def ++=(y: Rep[intf.ops.V[A]]) = intf.ops.++=(y)
    //def copyFrom(pos: Rep[Int], y: Rep[intf.ops.V[A]]) = intf.ops.copyFrom(pos,y)
    def insert(pos: Rep[Int], y: Rep[A]) = intf.ops.insert(pos,y)
    //def insertAll(pos: Rep[Int], y: Rep[intf.ops.V[A]]) = intf.ops.insertAll(pos,y)
    def remove(pos: Rep[Int]) = intf.ops.remove(pos)
    def removeAll(pos: Rep[Int], len: Rep[Int]) = intf.ops.removeAll(pos,len)
    def trim() = intf.ops.trim
    def clear() = intf.ops.clear    
    
    // //def +(y: Rep[intf.ops.V[A]])(implicit a: Arith[A]) = intf.ops.toIntf(intf.ops.+(y)) // doesn't work, would need dynamic type of ops
    def +(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.vplusToIntf(intf.ops.+(y))    
    def +(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = intf.ops.vplusToIntf(intf.ops.+(y))
    def -(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.vminusToIntf(intf.ops.-(y))    
    def -(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = intf.ops.vminusToIntf(intf.ops.-(y))
    def *(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.vtimesToIntf(intf.ops.*(y))    
    def *(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = intf.ops.vtimesToIntf(intf.ops.*(y))  
    //def *(y: Rep[Matrix[A]])(implicit a: Arith[A],o: Overloaded2) = intf.ops.vtimesToIntf(intf.ops.*(y))
    def **(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.**(y)
    def *:*(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.*:*(y)
    def dot(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.dot(y)
    def /(y: Interface[Vector[A]])(implicit a: Arith[A]) = intf.ops.toIntf(intf.ops./(y))    
    def /(y: Rep[A])(implicit a: Arith[A], o: Overloaded1) = intf.ops.toIntf(intf.ops./(y))      
    
    def sum(implicit a: Arith[A]) = intf.ops.sum
    def abs(implicit a: Arith[A]) = intf.ops.toIntf(intf.ops.abs)
    def exp(implicit a: Arith[A]) = intf.ops.toIntf(intf.ops.exp)        
    
    def sort(implicit o: Ordering[A]) = intf.ops.toIntf(intf.ops.sort)
    def min(implicit o: Ordering[A], mx: HasMinMax[A]) = intf.ops.min
    def minIndex(implicit o: Ordering[A], mx: HasMinMax[A]) = intf.ops.minIndex
    def max(implicit o: Ordering[A], mx: HasMinMax[A]) = intf.ops.max
    def maxIndex(implicit o: Ordering[A], mx: HasMinMax[A]) = intf.ops.maxIndex
    def median(implicit o: Ordering[A]) = intf.ops.median
    def :>(y: Interface[Vector[A]])(implicit o: Ordering[A]) = intf.ops.:>(y)
    def :<(y: Interface[Vector[A]])(implicit o: Ordering[A]) = intf.ops.:<(y)
    
    def map[B:Manifest](f: Rep[A] => Rep[B]) = intf.ops.toIntf(intf.ops.map(f))
    def mmap(f: Rep[A] => Rep[A]) = intf.ops.wrap(intf.ops.mmap(f))
    def foreach(block: Rep[A] => Rep[Unit]) = intf.ops.foreach(block)
    def zip[B:Manifest,R:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[R]) = intf.ops.toIntf(intf.ops.zip(y)(f))
    def mzip[B:Manifest](y: Interface[Vector[B]])(f: (Rep[A],Rep[B]) => Rep[A]) = intf.ops.wrap(intf.ops.mzip(y)(f))
    def reduce(f: (Rep[A],Rep[A]) => Rep[A])(implicit a: Arith[A]) = intf.ops.reduce(f)
    def filter(pred: Rep[A] => Rep[Boolean]) = intf.ops.toIntf(intf.ops.filter(pred))
    //def find(pred: Rep[A] => Rep[Boolean]) = intf.ops.vfindToIntf(intf.ops.find(pred))
    def find(pred: Rep[A] => Rep[Boolean]) = intf.ops.toIntf(intf.ops.find(pred))
    def count(pred: Rep[A] => Rep[Boolean]) = intf.ops.count(pred)
    // def flatMap[B:Manifest](f: Rep[A] => Rep[V[B]]) = intf.ops.toIntf(intf.ops.flatMap(f))
    // def partition(pred: Rep[A] => Rep[Boolean]) = { val a = intf.ops.partition(pred); (intf.ops.toIntf(a._1), intf.ops.toIntf(a._2)) }
    // def groupBy[K:Manifest](pred: Rep[A] => Rep[K]) = intf.ops.toIntf(intf.ops.groupBy(pred))
  }
  
  def EmptyVector[A](implicit mA: Manifest[A]): Rep[DenseVector[A]] = (mA match {
    // these don't allocate any memory
    case Manifest.Double => densevector_empty_double
    case Manifest.Float => densevector_empty_float
    case Manifest.Int => densevector_empty_int
    // allocates a dummy polymorphic class
    case _ => densevector_empty[A]
  }).asInstanceOf[Rep[DenseVector[A]]]

  def ZeroVector[A](length: Rep[Int], isRow: Rep[Boolean] = unit(true))(implicit mA: Manifest[A]): Rep[DenseVector[A]] = (mA match {
    case Manifest.Double => densevector_zero_double(length, isRow)
    case Manifest.Float => densevector_zero_float(length, isRow)
    case Manifest.Int => densevector_zero_int(length, isRow)
    case _ => throw new IllegalArgumentException("No ZeroVector exists of type " + mA)
  }).asInstanceOf[Rep[DenseVector[A]]]

  // object defs
  def densevector_obj_new[A:Manifest](len: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[A]]
  def densevector_obj_fromseq[A:Manifest](xs: Rep[Seq[A]]): Rep[DenseVector[A]]
  def densevector_obj_ones(len: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_onesf(len: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_zeros(len: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_zerosf(len: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_rand(len: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_randf(len: Rep[Int]): Rep[DenseVector[Float]]
  def vector_obj_range(start: Rep[Int], end: Rep[Int], stride: Rep[Int], isRow: Rep[Boolean]): Rep[RangeVector]
  def densevector_obj_uniform(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean]): Rep[DenseVector[Double]]
  def densevector_obj_flatten[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]]): Rep[DenseVector[A]]

  // class defs
  def vector_slice[A:Manifest,VA:Manifest](x: Interface[Vector[A]], start: Rep[Int], end: Rep[Int])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_contains[A:Manifest](x: Interface[Vector[A]], y: Rep[A]): Rep[Boolean]
  def vector_distinct[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  
  def vector_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_mutable_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_pprint[A:Manifest](x: Interface[Vector[A]]): Rep[Unit]
  def vector_repmat[A:Manifest](x: Interface[Vector[A]], i: Rep[Int], j: Rep[Int]): Rep[Matrix[A]]
  def vector_mkstring[A:Manifest](x: Interface[Vector[A]], sep: Rep[String]): Rep[String]  
  
  def vector_concatenate[A:Manifest,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  
  def vector_plus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_plus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]): Rep[VA] 
  def vector_plusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]): Rep[Unit]
  def vector_plusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]): Rep[Unit]   
  def vector_minus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_minus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]): Rep[VA] 
  def vector_minusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]): Rep[Unit]
  def vector_minusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]): Rep[Unit] 
  def vector_times[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_times_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]): Rep[VA] 
  def vector_timesequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]): Rep[Unit]
  def vector_timesequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]): Rep[Unit] 
  //def vector_times_matrix[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[Matrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_dot_product[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]): Rep[A]
  def vector_outer[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]): Rep[Matrix[A]]  
  def vector_divide[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_divide_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]): Rep[VA] 
  def vector_divideequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]): Rep[Unit]
  def vector_divideequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]): Rep[Unit] 
  
  def vector_sum[A:Manifest:Arith](x: Interface[Vector[A]]): Rep[A]
  def vector_abs[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_exp[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  
  def vector_min[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]): Rep[A]
  def vector_minindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]): Rep[Int]
  def vector_max[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]): Rep[A]
  def vector_maxindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]): Rep[Int]
  def vector_median[A:Manifest:Ordering](x: Interface[Vector[A]]): Rep[A]  
  
  def vector_map[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Rep[A] => Rep[B])(implicit b: VectorBuilder[B,VB]): Rep[VB]
  def vector_mmap[A:Manifest](x: Interface[Vector[A]], f: Rep[A] => Rep[A]): Rep[Unit]
  def vector_foreach[A:Manifest](x: Interface[Vector[A]], block: Rep[A] => Rep[Unit]): Rep[Unit]
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest,VR:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Rep[A],Rep[B]) => Rep[R])(implicit b: VectorBuilder[R,VR]): Rep[VR]
  def vector_mzipwith[A:Manifest,B:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Rep[A],Rep[B]) => Rep[A]): Rep[Unit]
  def vector_reduce[A:Manifest:Arith](x: Interface[Vector[A]], f: (Rep[A],Rep[A]) => Rep[A]): Rep[A]
  def vector_filter[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_find[A:Manifest,VFINDR:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean])(implicit b: VectorBuilder[Int,VFINDR]): Rep[VFINDR]
  def vector_count[A:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean]): Rep[Int]
  //def vector_flatmap[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Rep[A] => Rep[Vector[B]])(implicit b: VectorBuilder[B,VB]): Rep[VB]
  //def vector_partition[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[Boolean]): (Rep[VA], Rep[VA])
  //def vector_groupby[A:Manifest,K:Manifest,VA:Manifest,VVA:Manifest](x: Interface[Vector[A]], pred: Rep[A] => Rep[K])(implicit b: VectorBuilder[VA,VVA]): Rep[VVA]           
  
  // other defs
  def densevector_empty_double: Rep[DenseVector[Double]]
  def densevector_empty_float: Rep[DenseVector[Float]]
  def densevector_empty_int: Rep[DenseVector[Int]]
  def densevector_empty[A:Manifest]: Rep[DenseVector[A]]
  def densevector_zero_double(length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[Double]]
  def densevector_zero_float(length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[Float]]
  def densevector_zero_int(length: Rep[Int], isRow: Rep[Boolean]): Rep[DenseVector[Int]]
}

trait VectorOpsExp extends VectorOps with DeliteCollectionOpsExp with VariablesExp with BaseFatExp {

  this: VectorImplOps with OptiLAExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure

  case class VectorObjectRange(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean])
    extends Def[RangeVector]
  case class DenseVectorNew[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[A]] {    
    val mA = manifest[A]
  }
  case class DenseVectorEmptyDouble() extends Def[DenseVector[Double]]
  case class DenseVectorEmptyFloat() extends Def[DenseVector[Float]]
  case class DenseVectorEmptyInt() extends Def[DenseVector[Int]]
  case class DenseVectorEmpty[A:Manifest]() extends Def[DenseVector[A]] {
    val mA = manifest[A]
  }
  case class DenseVectorZeroDouble(length: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[Double]]
  case class DenseVectorZeroFloat(length: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[Float]]
  case class DenseVectorZeroInt(length: Exp[Int], isRow: Exp[Boolean]) extends Def[DenseVector[Int]]

  /////////////////////////////////////////////////
  // implemented via kernel embedding (sequential)

  case class DenseVectorObjectFromSeq[A:Manifest](xs: Exp[Seq[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_fromseq_impl(xs)))

  case class DenseVectorObjectOnes(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_ones_impl(len)))

  case class DenseVectorObjectOnesF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_onesf_impl(len)))

  case class DenseVectorObjectZeros(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(reflectPure(DenseVectorNew[Double](len, Const(true))))) //densevector_obj_zeros_impl(len)))

  case class DenseVectorObjectZerosF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(reflectPure(DenseVectorNew[Float](len, Const(true))))) //densevector_obj_zerosf_impl(len)))
    
  case class DenseVectorObjectRand(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_rand_impl(len)))

  case class DenseVectorObjectRandF(len: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_randf_impl(len)))

  case class DenseVectorObjectUniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_uniform_impl(start, step_size, end, isRow)))

  case class DenseVectorObjectFlatten[A:Manifest](pieces: Exp[DenseVector[DenseVector[A]]])
    extends DeliteOpSingleTask(reifyEffectsHere(densevector_obj_flatten_impl(pieces)))

  case class VectorSlice[A:Manifest,VA:Manifest](x: Interface[Vector[A]], start: Exp[Int], end: Exp[Int])(implicit b: VectorBuilder[A,VA])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_slice_impl[A,VA](x,start,end)))

  case class VectorContains[A:Manifest](x: Interface[Vector[A]], y: Exp[A])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_contains_impl[A](x, y)))

  case class VectorDistinct[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_distinct_impl[A,VA](x)))

  case class VectorClone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_clone_impl[A,VA](x)))

  case class VectorPPrint[A](x: Interface[Vector[A]])(block: Exp[Unit]) // stupid limitation...
    extends DeliteOpSingleTask(block)
    // reifyEffects(densevector_pprint_impl[A](x))

  case class VectorRepmat[A:Manifest](x: Interface[Vector[A]], i: Exp[Int], j: Exp[Int])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_repmat_impl[A](x,i,j)))

  case class VectorMkString[A:Manifest](x: Interface[Vector[A]], sep: Exp[String])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_mkstring_impl[A](x, sep)))

  case class VectorConcatenate[A:Manifest,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_concatenate_impl[A,VA](x,y)))

  // case class VectorTimesMatrix[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Exp[Matrix[A]])(implicit b: VectorBuilder[A,VA])
  //     extends DeliteOpSingleTask(reifyEffectsHere(vector_times_matrix_impl[A,VA](x,y)))
  
  case class VectorOuter[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_outer_impl[A](x,y))) {
      //TODO: should mixin implicit accessors
      def m = manifest[A]
      def a = implicitly[Arith[A]]
    }
    
  case class VectorMedian[A:Manifest:Ordering](x: Interface[Vector[A]])
    extends DeliteOpSingleTask(reifyEffectsHere(vector_median_impl[A](x)))

    
  ///////////////////////////////////////
  // implemented via parallel delite ops

  abstract class VectorArithmeticMap[A:Manifest:Arith,VA:Manifest](implicit val b: VectorBuilder[A,VA]) extends DeliteOpMap[A,A,VA] {
    val intf: Interface[Vector[A]]
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    
    def alloc = b.alloc(intf.length, intf.isRow)
    val size = copyTransformedOrElse(_.size)(intf.length)

    def m = manifest[A]
    def mVA = manifest[VA]
    def a = implicitly[Arith[A]]
  }

  abstract class VectorArithmeticZipWith[A:Manifest:Arith,VA:Manifest](implicit val b: VectorBuilder[A,VA]) extends DeliteOpZipWith[A,A,A,VA] {
    val intfA: Interface[Vector[A]]
    val intfB: Interface[Vector[A]]
    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[A]]]
    
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)

    def m = manifest[A]
    def mVA = manifest[VA]
    def a = implicitly[Arith[A]]
  }
  
  abstract class VectorArithmeticIndexedLoop[A:Manifest:Arith] extends DeliteOpIndexedLoop {
    val intf: Interface[Vector[A]]
    val size = copyTransformedOrElse(_.size)(intf.length)
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  abstract class VectorArithmeticReduce[A:Manifest:Arith] extends DeliteOpReduce[A] {
    val intf: Interface[Vector[A]]
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]    
    val size = copyTransformedOrElse(_.size)(intf.length)
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }
  
  case class VectorPlus[A:Manifest:Arith,VA:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticZipWith[A,VA] {

    def func = (a,b) => a + b
  }

  case class VectorPlusScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e + y
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

  case class VectorMinusScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e - y
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

  case class VectorTimesScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e * y
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
    val size = intfA.length
    val zero = implicitly[Arith[A]].empty
    
    def m = manifest[A]
    def a = implicitly[Arith[A]]
  }

  
  case class VectorDivide[A:Manifest:Arith,VA:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticZipWith[A,VA] {

    def func = (a,b) => a / b
  }

  case class VectorDivideScalar[A:Manifest:Arith,VA:Manifest](intf: Interface[Vector[A]], y: Exp[A])(implicit b: VectorBuilder[A,VA])
    extends VectorArithmeticMap[A,VA] {

    def func = e => e / y
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
  }

  case class VectorMax[A:Manifest:Ordering:HasMinMax](intf: Interface[Vector[A]]) 
    extends DeliteOpReduce[A] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]
    val size = copyTransformedOrElse(_.size)(intf.length)
    val zero = implicitly[HasMinMax[A]].minValue
    def func = (a,b) => if (a > b) a else b
  }

  case class VectorMinIndex[A:Manifest:Ordering:HasMinMax](intfB: Interface[Vector[A]]) 
    extends DeliteOpZipWithReduceTuple[Int,A,Int,A] {
    
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val inA = copyOrElse(_.inA)(0::intfB.length)
    val size = copyTransformedOrElse(_.size)(intfB.length)
    val zero = (copyTransformedOrElse(_.zero._1)(unit(0)),copyTransformedOrElse(_.zero._2)(implicitly[HasMinMax[A]].maxValue)) // 0 sensible? maybe -1?
    def zip = (a,b) => (a,b)
    def reduce = (a,b) => (if (a._2 < b._2) a._1 else b._1, if (a._2 < b._2) a._2 else b._2)

    val m = manifest[A]
    val o = implicitly[Ordering[A]]
    val p = implicitly[HasMinMax[A]]
  }
  
  case class VectorMaxIndex[A:Manifest:Ordering:HasMinMax](intfB: Interface[Vector[A]]) 
    extends DeliteOpZipWithReduceTuple[Int,A,Int,A] {

    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val inA = copyOrElse(_.inA)(0::intfB.length)
    val size = copyTransformedOrElse(_.size)(intfB.length)
    val zero = (copyTransformedOrElse(_.zero._1)(unit(0)),copyTransformedOrElse(_.zero._2)(implicitly[HasMinMax[A]].minValue)) // 0 sensible? maybe -1?
    def zip = (a,b) => (a,b)
    def reduce = (a,b) => (if (a._2 > b._2) a._1 else b._1, if (a._2 > b._2) a._2 else b._2)

    val m = manifest[A]
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
    def mVB = manifest[VB]
  }

  case class VectorMutableMap[A:Manifest](intf: Interface[Vector[A]], block: Exp[A] => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(intf.length)
    def func = i => intf(i) = block(intf(i))

    val m = manifest[A]
  }

  case class VectorForeach[A:Manifest](intf: Interface[Vector[A]], func: Exp[A] => Exp[Unit])
    extends DeliteOpForeach[A] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    def sync = n => List()
    val size = copyTransformedOrElse(_.size)(intf.length)
  }

  case class VectorZipWith[A:Manifest,B:Manifest,R:Manifest,VR:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]],
                                                                         func: (Exp[A], Exp[B]) => Exp[R])(implicit b: VectorBuilder[R,VR])
    extends DeliteOpZipWith[A,B,R,VR] {

    val inA = intfA.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val inB = intfB.ops.elem.asInstanceOf[Exp[Vector[B]]]  
    def alloc = b.alloc(intfA.length, intfA.isRow)
    val size = copyTransformedOrElse(_.size)(intfA.length)
  }

  case class VectorMutableZipWith[A:Manifest,B:Manifest](intfA: Interface[Vector[A]], intfB: Interface[Vector[B]],
                                                        block: (Exp[A], Exp[B]) => Exp[A])
    extends DeliteOpIndexedLoop {

    val size = copyTransformedOrElse(_.size)(intfA.length)
    def func = i => intfA(i) = block(intfA(i),intfB(i))
  }

  // note: we may want to factor 'HasEmpty' out of 'Arith' to make this more general, if the need arises.
  case class VectorReduce[A:Manifest:Arith](intf: Interface[Vector[A]], func: (Exp[A], Exp[A]) => Exp[A])
    extends DeliteOpReduce[A] {
    
    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val size = copyTransformedOrElse(_.size)(intf.length)
    val zero = implicitly[Arith[A]].empty
  }

  case class VectorFilter[A:Manifest,VA:Manifest](intf: Interface[Vector[A]], cond: Exp[A] => Exp[Boolean])(implicit val b: VectorBuilder[A,VA])
    extends DeliteOpFilter[A,A,VA] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    def alloc = b.alloc(0, intf.isRow)
    def func = e => e 
    val size = copyTransformedOrElse(_.size)(intf.length)

    def m = manifest[A]  
    def mVA = manifest[VA]
  }

  case class VectorFind[A:Manifest,VFINDR:Manifest](intf: Interface[Vector[A]], cond: Exp[A] => Exp[Boolean])(implicit val b: VectorBuilder[Int,VFINDR])
    extends DeliteOpFilter[A,Int,VFINDR] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    def alloc = b.alloc(0,unit(true))
    def func = e => v // should we make available and use a helper function like index(e)?
    val size = copyTransformedOrElse(_.size)(intf.length)

    def m = manifest[A]  
    def mVA = manifest[VFINDR]
  }

  case class VectorCount[A:Manifest](intf: Interface[Vector[A]], cond: Exp[A] => Exp[Boolean]) 
    extends DeliteOpFilterReduce[A,Int] {

    val in = intf.ops.elem.asInstanceOf[Exp[Vector[A]]]  
    val size = copyTransformedOrElse(_.size)(intf.length)
    val zero = unit(0)
    def func = e => unit(1)
    def reduce = (a,b) => a + b   

    def m = manifest[A]
  }
  
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
    
  def isDense[A](x: Exp[DeliteCollection[A]]) = isSubtype(x.Type.erasure,classOf[DenseVector[A]])  
  def asDense[A](x: Exp[DeliteCollection[A]]) = x.asInstanceOf[Exp[DenseVector[A]]]
  
  def isRange[A](x: Exp[DeliteCollection[A]]) = isSubtype(x.Type.erasure,classOf[RangeVector])  
  def asRange[A](x: Exp[DeliteCollection[A]]) = x.asInstanceOf[Exp[RangeVector]]
  
  def isView[A](x: Exp[DeliteCollection[A]]) = isSubtype(x.Type.erasure,classOf[VectorView[A]])
  def asView[A](x: Exp[DeliteCollection[A]]) = x.asInstanceOf[Exp[VectorView[A]]]  
  
  override def dc_size[A:Manifest](x: Exp[DeliteCollection[A]]) = { 
    if (isDense(x)) asDense(x).length
    else if (isRange(x)) asRange(x).length
    else if (isView(x)) asView(x).length
    else super.dc_size(x)
  }
  
  override def dc_apply[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int]) = {
    if (isDense(x)) asDense(x).apply(n)
    else if (isRange(x)) (asRange(x).apply(n)).asInstanceOf[Exp[A]]
    else if (isView(x)) asView(x).apply(n)
    else {
      Predef.println("couldn't find dc_apply for " + x.Type.toString)
      Predef.println("isView: " + isView(x))
      super.dc_apply(x,n)    
    }
  }
  
  override def dc_update[A:Manifest](x: Exp[DeliteCollection[A]], n: Exp[Int], y: Exp[A]) = {
    if (isDense(x)) asDense(x).update(n,y)
    else if (isRange(x)) asRange(x).update(n,y.asInstanceOf[Exp[Int]])
    else if (isView(x)) asView(x).update(n,y)
    else super.dc_update(x,n,y)        
  }
  
  
  /////////////////////
  // object interface

  def densevector_obj_new[A:Manifest](len: Exp[Int], isRow: Exp[Boolean]) = reflectMutable(DenseVectorNew[A](len, isRow)) //XXX
  def densevector_obj_fromseq[A:Manifest](xs: Exp[Seq[A]]) = reflectPure(DenseVectorObjectFromSeq(xs)) //XXX
  def densevector_obj_ones(len: Exp[Int]) = reflectPure(DenseVectorObjectOnes(len))
  def densevector_obj_onesf(len: Exp[Int]) = reflectPure(DenseVectorObjectOnesF(len))
  def densevector_obj_zeros(len: Exp[Int]) = reflectPure(DenseVectorObjectZeros(len))
  def densevector_obj_zerosf(len: Exp[Int]) = reflectPure(DenseVectorObjectZerosF(len))
  def densevector_obj_rand(len: Exp[Int]) = reflectEffect(DenseVectorObjectRand(len))
  def densevector_obj_randf(len: Exp[Int]) = reflectEffect(DenseVectorObjectRandF(len))
  def vector_obj_range(start: Exp[Int], end: Exp[Int], stride: Exp[Int], isRow: Exp[Boolean]) = reflectPure(VectorObjectRange(start, end, stride, isRow))
  def densevector_obj_uniform(start: Exp[Double], step_size: Exp[Double], end: Exp[Double], isRow: Exp[Boolean]) = reflectPure(DenseVectorObjectUniform(start, step_size, end, isRow))
  def densevector_obj_flatten[A:Manifest](pieces: Exp[DenseVector[DenseVector[A]]]) = reflectPure(DenseVectorObjectFlatten(pieces))


  /////////////////////
  // class interface
  
  def vector_slice[A:Manifest,VA:Manifest](x: Interface[Vector[A]], start: Exp[Int], end: Exp[Int])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorSlice[A,VA](x, start, end))
  def vector_contains[A:Manifest](x: Interface[Vector[A]], y: Exp[A]) = reflectPure(VectorContains(x,y))
  def vector_distinct[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorDistinct[A,VA](x))
  
  def vector_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorClone[A,VA](x))
  def vector_mutable_clone[A:Manifest,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectMutable(VectorClone[A,VA](x))
  def vector_pprint[A:Manifest](x: Interface[Vector[A]]) = reflectEffect(VectorPPrint(x)(reifyEffectsHere(vector_pprint_impl[A](x))))  
  def vector_repmat[A:Manifest](x: Interface[Vector[A]], i: Rep[Int], j: Rep[Int]) = reflectPure(VectorRepmat(x,i,j))
  def vector_mkstring[A:Manifest](x: Interface[Vector[A]], sep: Rep[String]) = reflectPure(VectorMkString(x,sep))
  
  def vector_concatenate[A:Manifest,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorConcatenate[A,VA](x,y))
  
  def vector_plus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorPlus[A,VA](x,y))
  def vector_plus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorPlusScalar[A,VA](x,y))
  def vector_plusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = reflectWrite(x.ops.elem)(VectorPlusEquals(x,y))
  def vector_plusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]) = reflectWrite(x.ops.elem)(VectorPlusEqualsScalar(x,y))
  def vector_minus[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorMinus[A,VA](x,y))
  def vector_minus_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorMinusScalar[A,VA](x,y))
  def vector_minusequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = reflectWrite(x.ops.elem)(VectorMinusEquals(x,y))
  def vector_minusequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]) = reflectWrite(x.ops.elem)(VectorMinusEqualsScalar(x,y))  
  def vector_times[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorTimes[A,VA](x,y))
  def vector_times_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorTimesScalar[A,VA](x,y))
  def vector_timesequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = reflectWrite(x.ops.elem)(VectorTimesEquals(x,y))
  def vector_timesequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]) = reflectWrite(x.ops.elem)(VectorTimesEqualsScalar(x,y))  
  //def vector_times_matrix[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Exp[Matrix[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorTimesMatrix[A,VA](x,y))
  def vector_dot_product[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = reflectPure(VectorDotProduct(x,y))
  def vector_outer[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = reflectPure(VectorOuter(x,y))
  def vector_divide[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorDivide[A,VA](x,y))
  def vector_divide_scalar[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]], y: Rep[A])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorDivideScalar[A,VA](x,y))
  def vector_divideequals[A:Manifest:Arith](x: Interface[Vector[A]], y: Interface[Vector[A]]) = reflectWrite(x.ops.elem)(VectorDivideEquals(x,y))
  def vector_divideequals_scalar[A:Manifest:Arith](x: Interface[Vector[A]], y: Rep[A]) = reflectWrite(x.ops.elem)(VectorDivideEqualsScalar(x,y))  

  def vector_sum[A:Manifest:Arith](x: Interface[Vector[A]]) = reflectPure(VectorSum(x))
  def vector_abs[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorAbs[A,VA](x))
  def vector_exp[A:Manifest:Arith,VA:Manifest](x: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorExp[A,VA](x))
  
  def vector_min[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]) = reflectPure(VectorMin(x))
  def vector_minindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]) = reflectPure(VectorMinIndex(x))
  def vector_max[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]) = reflectPure(VectorMax(x))
  def vector_maxindex[A:Manifest:Ordering:HasMinMax](x: Interface[Vector[A]]) = reflectPure(VectorMaxIndex(x))
  def vector_median[A:Manifest:Ordering](x: Interface[Vector[A]]) = reflectPure(VectorMedian(x))
  
  def vector_map[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Exp[A] => Exp[B])(implicit b: VectorBuilder[B,VB]) = reflectPure(VectorMap[A,B,VB](x,f)) // TODO: effect if func effectful!
  def vector_mmap[A:Manifest](x: Interface[Vector[A]], f: Exp[A] => Exp[A]) = reflectWrite(x.ops.elem)(VectorMutableMap(x,f)) // TODO: effect if func effectful!
  def vector_foreach[A:Manifest](x: Interface[Vector[A]], block: Exp[A] => Exp[Unit]) = {
    val vf = VectorForeach(x, block) //reflectEffect(VectorForeach(x, block)) 
    reflectEffect(vf, summarizeEffects(vf.body.asInstanceOf[DeliteForeachElem[A]].func).star)
  }
  def vector_zipwith[A:Manifest,B:Manifest,R:Manifest,VR:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Exp[A],Exp[B]) => Exp[R])(implicit b: VectorBuilder[R,VR]) = {
    reflectPure(VectorZipWith[A,B,R,VR](x,y,f))
  }
  def vector_mzipwith[A:Manifest,B:Manifest](x: Interface[Vector[A]], y: Interface[Vector[B]], f: (Exp[A],Exp[B]) => Exp[A]) = {
    reflectWrite(x.ops.elem)(VectorMutableZipWith(x,y,f))
  }
  def vector_reduce[A:Manifest:Arith](x: Interface[Vector[A]], f: (Exp[A],Exp[A]) => Exp[A]) = reflectPure(VectorReduce(x, f))
  def vector_filter[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean])(implicit b: VectorBuilder[A,VA]) = reflectPure(VectorFilter[A,VA](x, pred))
  def vector_find[A:Manifest,VFINDR:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean])(implicit b: VectorBuilder[Int,VFINDR]) = reflectPure(VectorFind[A,VFINDR](x, pred))
  def vector_count[A:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean]) = reflectPure(VectorCount(x, pred))
  //def vector_flatmap[A:Manifest,B:Manifest,VB:Manifest](x: Interface[Vector[A]], f: Exp[A] => Exp[Vector[B]])(implicit b: VectorBuilder[B,VB]) = reflectPure(VectorFlatMap(x, f))
  //def vector_partition[A:Manifest,VA:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[Boolean]) = t2(reflectPure(VectorPartition(x, pred)))
  //def vector_groupby[A:Manifest,K:Manifest,VA:Manifest,VVA:Manifest](x: Interface[Vector[A]], pred: Exp[A] => Exp[K])(implicit b: VectorBuilder[VA,VVA]) = reflectPure(VectorGroupBy(x, pred))
  
  ///////////
  // other
  
  def densevector_empty_double = DenseVectorEmptyDouble()
  def densevector_empty_float = DenseVectorEmptyFloat()
  def densevector_empty_int = DenseVectorEmptyInt()
  def densevector_empty[A:Manifest] = DenseVectorEmpty[A]()
  def densevector_zero_double(length: Exp[Int], isRow: Exp[Boolean]) = DenseVectorZeroDouble(length, isRow)
  def densevector_zero_float(length: Exp[Int], isRow: Exp[Boolean]) = DenseVectorZeroFloat(length, isRow)
  def densevector_zero_int(length: Exp[Int], isRow: Exp[Boolean]) = DenseVectorZeroInt(length, isRow)


  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    // implemented as DeliteOpSingleTask and DeliteOpLoop
    case e@DenseVectorObjectOnes(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectOnes(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectOnesF(x) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectOnesF(f(x)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@DenseVectorObjectUniform(x,y,z,w) => reflectPure(new { override val original = Some(f,e) } with DenseVectorObjectUniform(f(x),f(y),f(z),f(w)))(mtype(manifest[A]),implicitly[SourceContext])
    
    case e@VectorOuter(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorOuter(f(x),f(y))(e.m, e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorPlus(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorPlus(f(x),f(y))(e.m, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMinus(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorMinus(f(x),f(y))(e.m, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorTimes(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorTimes(f(x),f(y))(e.m, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorTimesScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorTimesScalar(f(x),f(y))(e.m, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDotProduct(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDotProduct(f(x),f(y))(e.m, e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorDivideScalar(x,y) => reflectPure(new { override val original = Some(f,e) } with VectorDivideScalar(f(x),f(y))(e.m, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorSum(x) => reflectPure(new { override val original = Some(f,e) } with VectorSum(f(x))(e.m, e.a))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorAbs(x) => reflectPure(new { override val original = Some(f,e) } with VectorAbs(f(x))(e.m, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorExp(x) => reflectPure(new { override val original = Some(f,e) } with VectorExp(f(x))(e.m, e.a, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorFilter(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorFilter(f(x),f(p))(e.m, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorFind(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorFind(f(x),f(p))(e.m, e.mVA, e.b))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorCount(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorCount(f(x),f(p))(e.m))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMinIndex(x) => reflectPure(new { override val original = Some(f,e) } with VectorMinIndex(f(x))(e.m,e.o,e.p))(mtype(manifest[A]),implicitly[SourceContext])
    case e@VectorMap(x,p) => reflectPure(new { override val original = Some(f,e) } with VectorMap(f(x),f(p))(e.mA,e.mB,e.mVB,e.b))(mtype(manifest[A]),implicitly[SourceContext])
    
    case Reflect(e@VectorTimesScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorTimesScalar(f(x),f(y))(e.m, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorDivideScalar(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorDivideScalar(f(x),f(y))(e.m, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlus(f(x),f(y))(e.m, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMinus(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMinus(f(x),f(y))(e.m, e.a, e.mVA, e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorForeach(a,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorForeach(f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPlusEquals(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPlusEquals(f(x),f(y))(e.m, e.a), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorMutableMap(x,g), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorMutableMap(f(x),f(g))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorPPrint(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with VectorPPrint(f(x))(f(e.block)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    
    // allocations
    case Reflect(e@DenseVectorObjectZeros(x), u, es) => reflectMirrored(Reflect(DenseVectorObjectZeros(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@VectorObjectRange(s,o,d,r), u, es) => reflectMirrored(Reflect(VectorObjectRange(f(s),f(o),f(d),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@DenseVectorNew(l,r), u, es) => reflectMirrored(Reflect(DenseVectorNew(f(l),f(r))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
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
//     case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new generated.scala.VectorImpl[" + remap(v.mA) + "](" + quote(length) + "," + quote(isRow) + ")")
//     case VectorObjectRange(start, end, stride, isRow) => emitValDef(sym, "new generated.scala.RangeVectorImpl(" + quote(start) + "," + quote(end) + "," + quote(stride) + "," + quote(isRow) + ")")
//     case DenseVectorZeroDouble(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorDoubleImpl(" + quote(length) + ", " + quote(isRow) + ")")
//     case DenseVectorZeroFloat(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorFloatImpl(" + quote(length) + ", " + quote(isRow) + ")")
//     case DenseVectorZeroInt(length, isRow) => emitValDef(sym, "new generated.scala.ZeroVectorIntImpl(" + quote(length) + ", " + quote(isRow) + ")")
//     case DenseVectorEmptyDouble() => emitValDef(sym, "generated.scala.EmptyVectorDoubleImpl")
//     case DenseVectorEmptyFloat() => emitValDef(sym, "generated.scala.EmptyVectorFloatImpl")
//     case DenseVectorEmptyInt() => emitValDef(sym, "generated.scala.EmptyVectorIntImpl")
//     case v@DenseVectorEmpty() => emitValDef(sym, "new generated.scala.EmptyVectorImpl[" + remap(v.mA) + "]")
    case v@DenseVectorNew(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[" + remap(v.mA) + "]")+"(" + quote(length) + "," + quote(isRow) + ")")
    case VectorObjectRange(start, end, stride, isRow) => emitValDef(sym, "new generated.scala.RangeVector(" + quote(start) + "," + quote(end) + "," + quote(stride) + "," + quote(isRow) + ")")
    case DenseVectorZeroDouble(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Double]")+"(" + quote(length) + ", " + quote(isRow) + ")")
    case DenseVectorZeroFloat(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Float]")+"(" + quote(length) + ", " + quote(isRow) + ")")
    case DenseVectorZeroInt(length, isRow) => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Int]")+"(" + quote(length) + ", " + quote(isRow) + ")")
    case DenseVectorEmptyDouble() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Double]")+"(0,true)")
    case DenseVectorEmptyFloat() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Float]")+"(0,true)")
    case DenseVectorEmptyInt() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[Int]")+"(0,true)")
    case v@DenseVectorEmpty() => emitValDef(sym, "new " + remap("generated.scala.DenseVector[" + remap(v.mA) + "]")+"(0,true)")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenVectorOps extends BaseGenVectorOps with CudaGenFat with CudaGenDataStruct {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    // Only allow allocating primitive type Vectors
    case DenseVectorNew(length, isRow) => {
      stream.println(addTab()+"%s *devPtr;".format(remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"DeliteCudaMalloc((void**)&devPtr,%s*sizeof(%s));".format(quote(length),remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,devPtr);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(length),quote(isRow)))
    }

    /* Specialized CUDA code generations for DeliteOpSingleTasks */
    /*
    case VectorObjectRange(start, end, stride, isRow) =>
      stream.println(addTab()+"RangeVector %s;".format(quote(sym)))
      stream.println(addTab()+"%s.start = %s;".format(quote(sym),quote(start)))
      stream.println(addTab()+"%s.end = %s;".format(quote(sym),quote(end)))
      stream.println(addTab()+"%s.stride = %s;".format(quote(sym),quote(stride)))
      stream.println(addTab()+"%s.isRow = %s;".format(quote(sym),quote(isRow)))


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

    // Only allow allocating primitive type Vectors
    case DenseVectorNew(length, isRow) => {
      stream.println(addTab()+"%s *devPtr;".format(remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"DeliteOpenCLMalloc((void**)&devPtr,%s*sizeof(%s));".format(quote(length),remap(sym.Type.typeArguments(0))))
      stream.println(addTab()+"%s *%s_ptr = new %s(%s,%s,devPtr);".format(remap(sym.Type),quote(sym),remap(sym.Type),quote(length),quote(isRow)))
    }
	
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenVectorOps extends BaseGenVectorOps with CGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case DenseVectorObjectZeros(len) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("memset(%s_data,0,sizeof(%s)*%s);".format(quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.length = %s;".format(quote(sym),quote(len)))
      stream.println("%s.isRow = true;".format(quote(sym)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))
    case DenseVectorNew(len,isRow) =>
      stream.println("%s *%s_data = malloc(sizeof(%s)*%s);".format(remap(sym.Type.typeArguments(0)),quote(sym),remap(sym.Type.typeArguments(0)),quote(len)))
      stream.println("%s %s;".format(remap(sym.Type),quote(sym)))
      stream.println("%s.length = %s;".format(quote(sym),quote(len)))
      stream.println("%s.isRow = %s;".format(quote(sym),quote(isRow)))
      stream.println("%s.data = %s_data;".format(quote(sym),quote(sym)))

    case _ => super.emitNode(sym, rhs)
  }
}

