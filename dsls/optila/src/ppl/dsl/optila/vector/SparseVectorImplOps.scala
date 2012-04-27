package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila.{DenseVector, DenseMatrix, SparseVector, SparseMatrix}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

trait SparseVectorImplOps { this: OptiLA =>
  def sparsevector_obj_flatten_impl[A:Manifest](pieces: Rep[SparseVector[SparseVector[A]]]): Rep[SparseVector[A]]
  
  def sparsevector_apply_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int]): Rep[A]
  def sparsevector_update_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
  def sparsevector_insert_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
  def sparsevector_insertall_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], xs: Rep[SparseVector[A]]): Rep[Unit]
  def sparsevector_copyfrom_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], xs: Rep[SparseVector[A]]): Rep[Unit]  
  def sparsevector_removeall_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit]
  def sparsevector_trim_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit]
  def sparsevector_clear_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] 
  def sparsevector_mutabletrans_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] 
  
  def sparsevector_sort_impl[A:Manifest:Ordering](v: Rep[SparseVector[A]]): Rep[SparseVector[A]]
  def sparsevector_times_matrix_impl[A:Manifest:Arith](v: Rep[SparseVector[A]], m: Rep[SparseMatrix[A]]): Rep[SparseVector[A]]
}

trait SparseVectorImplOpsStandard extends SparseVectorImplOps {
  this: OptiLACompiler with OptiLALift =>

  //////////////////////////
  // kernel implementations

  def sparsevector_obj_flatten_impl[A:Manifest](pieces: Rep[SparseVector[SparseVector[A]]]): Rep[SparseVector[A]] = { 
    throw new UnsupportedOperationException("tbd")
  }
    
  def sparsevector_apply_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int]): Rep[A] = {
    val d = sparsevector_raw_data(v)
    //d.getOrElse(pos) { defaultValue[A] }
    throw new UnsupportedOperationException("tbd")
  }
  
  def sparsevector_update_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    val d = sparsevector_raw_data(v)
    if (x != defaultValue[A])
      hashmap_unsafe_update(d,pos,x) 
  }
  
  def sparsevector_insert_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    // need to shift everything - probably have to construct a new map
    throw new UnsupportedOperationException("tbd")
  }

  def sparsevector_insertall_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], xs: Rep[SparseVector[A]]): Rep[Unit] = {
    // need to shift everything - probably have to construct a new map
    throw new UnsupportedOperationException("tbd")
  }

  def sparsevector_copyfrom_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], xs: Rep[SparseVector[A]]): Rep[Unit] = {
    //chkRange(pos, pos + xs.length)
    var i = 0
    while (i < xs.length) {
      sparsevector_update_impl(v,pos+i,xs(i))
      i += 1
    }
  }

  def sparsevector_removeall_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    // need to shift everything - probably have to construct a new map
    throw new UnsupportedOperationException("tbd")
  }

  def sparsevector_trim_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] = { /* nothing needed */ }
  
  def sparsevector_clear_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] = {
    sparsevector_set_length(v, 0)
    val d = sparsevector_raw_data(v)
    //hashmap_unsafe_clear(d)
    throw new UnsupportedOperationException("tbd")
  }

  def sparsevector_mutabletrans_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] = {
    sparsevector_set_isrow(v,!v.isRow)
  }
  
  def sparsevector_sort_impl[A:Manifest:Ordering](v: Rep[SparseVector[A]]): Rep[SparseVector[A]] = {
    throw new UnsupportedOperationException("tbd")
  }
  
  def sparsevector_times_matrix_impl[A:Manifest:Arith](v: Rep[SparseVector[A]], m: Rep[SparseMatrix[A]]): Rep[SparseVector[A]] = {
    throw new UnsupportedOperationException("tbd")
  }
}