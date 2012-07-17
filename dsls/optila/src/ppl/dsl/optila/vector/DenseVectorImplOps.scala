package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila.{DenseVector,DenseMatrix}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

trait DenseVectorImplOps { this: OptiLA =>
  def densevector_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[A]]): Rep[DenseVector[A]]
  def densevector_obj_fromunliftedseq_impl[A:Manifest](xs: Seq[Rep[A]]): Rep[DenseVector[A]]
  def densevector_obj_ones_impl(length: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_onesf_impl(length: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_zeros_impl(length: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_zerosf_impl(length: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_rand_impl(length: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_randf_impl(length: Rep[Int]): Rep[DenseVector[Float]]
  def densevector_obj_uniform_impl(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean]): Rep[DenseVector[Double]]
  def densevector_obj_flatten_impl[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]]): Rep[DenseVector[A]]
  
  def densevector_apply_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int]): Rep[A]
  def densevector_update_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
  def densevector_insert_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
  def densevector_insertall_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], xs: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_copyfrom_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], xs: Rep[DenseVector[A]]): Rep[Unit]  
  def densevector_removeall_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def densevector_trim_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_clear_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] 
  def densevector_mutabletrans_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] 
  
  def densevector_sort_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_times_matrix_impl[A:Manifest:Arith](v: Rep[DenseVector[A]], m: Rep[DenseMatrix[A]]): Rep[DenseVector[A]]
}

trait DenseVectorImplOpsStandard extends DenseVectorImplOps {
  this: OptiLACompiler with OptiLALift =>

  //////////////////////////
  // kernel implementations

  // TODO: support for asserts and other debugging tools
  
  def densevector_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[A]]) = {
    val v = DenseVector[A](xs.length, true)
    for (i <- 0 until xs.length) {
      v(i) = xs(i)
    }
    v.unsafeImmutable
  }
  
  def densevector_obj_fromunliftedseq_impl[A:Manifest](xs: Seq[Rep[A]]) = {
    val out = densevector_obj_new[A](unit(0),unit(true))
    // interpreted (not lifted)
    xs.foreach { out += _ }
    out.unsafeImmutable // return immutable object    
  }

  def densevector_obj_ones_impl(length: Rep[Int]) = DenseVector[Double](length, true) mmap { e => 1. } 

  def densevector_obj_onesf_impl(length: Rep[Int]) = DenseVector[Float](length, true) mmap { e => 1f }

  def densevector_obj_zeros_impl(length: Rep[Int]) = DenseVector[Double](length, true) mmap { e => 0. } // required for compatibility with non-JVM generators

  def densevector_obj_zerosf_impl(length: Rep[Int]) = DenseVector[Float](length, true) mmap { e => 0f }

  def densevector_obj_rand_impl(length: Rep[Int]) = DenseVector[Double](length, true) mmap { e => random[Double] }

  def densevector_obj_randf_impl(length: Rep[Int]) = DenseVector[Float](length, true) mmap { i => random[Float] }

  def densevector_obj_uniform_impl(start: Rep[Double], step_size: Rep[Double], end: Rep[Double], isRow: Rep[Boolean]) = {
    val length = ceil((end-start)/step_size).AsInstanceOf[Int]
    (0::length) map { i => step_size*i + start }
  }

  def densevector_obj_flatten_impl[A:Manifest](pieces: Rep[DenseVector[DenseVector[A]]]) = { // TODO: flatMap implementation
    if (pieces.length == 0){
      DenseVector[A](0, pieces.isRow).unsafeImmutable
    }
    else {
      val sizes = pieces map { e => e.length }
      val (total,begins) = densevector_precumulate[Int](sizes, 0)((_: Rep[Int]) + (_: Rep[Int]))
      val result = DenseVector[A](total, pieces.isRow)
      for (i <- 0 until pieces.length) {
        result.copyFrom(begins(i), pieces(i))
      }
      result.unsafeImmutable
    }
  }

  private def densevector_precumulate[A:Manifest](v: Rep[DenseVector[A]], identity: Rep[A])(func: (Rep[A],Rep[A]) => Rep[A]): (Rep[A], Rep[DenseVector[A]]) = {
    if (v.length == 0) {
      ((identity,DenseVector[A](0,v.isRow).unsafeImmutable))
    } else {
      val result = DenseVector[A](0, v.isRow)
      var accum = identity
      var i = unit(0)
      while (i < v.length) {
        result += accum
        accum = func(accum, v(i))
        i += 1
      }
      (accum,result.unsafeImmutable)
    }
  }
    
  def densevector_apply_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int]): Rep[A] = {
    val d = densevector_raw_data(v)
    d(pos)
  }
  
  def densevector_update_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    val d = densevector_raw_data(v)
    darray_unsafe_update(d,pos,x) 
  }
  
  def densevector_insert_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    densevector_insertspace(v,pos,1)
    densevector_update(v,pos,x)
  }

  def densevector_insertall_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], xs: Rep[DenseVector[A]]): Rep[Unit] = {
    densevector_insertspace(v,pos,xs.length)
    densevector_copyfrom(v, pos, xs)
  }

  def densevector_copyfrom_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], xs: Rep[DenseVector[A]]): Rep[Unit] = {
    //chkRange(pos, pos + xs.length)
    var i = 0
    val d = densevector_raw_data(v)
    while (i < xs.length) {
      darray_unsafe_update(d,pos+i,xs(i))
      i += 1
    }
  }

  def densevector_removeall_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    //chkRange(pos, pos + len)
    val data = densevector_raw_data(v)
    darray_unsafe_copy(data, pos + len, data, pos, v.length - (pos + len))
    densevector_set_length(v, v.length - len)
  }

  def densevector_trim_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] = {
    val data = densevector_raw_data(v)
    if (v.length < data.length) {
      val outData = DeliteArray[A](v.length)
      darray_unsafe_copy(data, 0, outData, 0, v.length)
      densevector_set_raw_data(v, outData.unsafeImmutable)
    }
  }
  
  def densevector_clear_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] = {
    densevector_set_length(v, 0)
    densevector_set_raw_data(v, (DeliteArray[A](0)).unsafeImmutable)
  }

  def densevector_mutabletrans_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] = {
    densevector_set_isrow(v,!v.isRow)
  }

  protected def densevector_insertspace[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    densevector_ensureextra(v,len)
    val data = densevector_raw_data(v)
    darray_unsafe_copy(data, pos, data, pos + len, v.length - pos)
    densevector_set_length(v, v.length + len)
  }

  protected def densevector_ensureextra[A:Manifest](v: Rep[DenseVector[A]], extra: Rep[Int]): Rep[Unit] = {
    val data = densevector_raw_data(v)
    if (data.length - v.length < extra) {
      densevector_realloc(v, v.length + extra)
    }
  }

  protected def densevector_realloc[A:Manifest](v: Rep[DenseVector[A]], minLen: Rep[Int]): Rep[Unit] = {  
    val data = densevector_raw_data(v)
    var n = Math.max(4, data.length * 2)
    while (n < minLen) n = n*2
    val d = DeliteArray[A](n)
    darray_unsafe_copy(data, 0, d, 0, v.length)
    densevector_set_raw_data(v, d.unsafeImmutable)
  }
  
  def densevector_sort_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]): Rep[DenseVector[A]] = {
    // inefficient! 2 copies (one to do the trim)
    // should be able to provide an index range to array sort
    val out = v.mutable()
    out.trim()
    val data = densevector_raw_data(out).sort    
    densevector_set_raw_data(out, data)
    out
  }
  
  def densevector_times_matrix_impl[A:Manifest:Arith](v: Rep[DenseVector[A]], m: Rep[DenseMatrix[A]]) = {
    //v.chkVecMatAgree(v, m)
    val v_trans = v.t
    m.t.mapRowsToVector { a_row => a_row *:* v_trans }
  }
}