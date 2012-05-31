package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.delite.framework.datastructures.DeliteArray
import ppl.dsl.optila.{DenseVector, DenseMatrix, SparseVector, SparseMatrix}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

/* SparseVector implementation notes
 * 
 * if we know the number of non-zeros in the output sparse vector:
 * 
 * data[T] is allocated ahead of time to be newNNZ
 * indices is allocated ahead of time to be newNNZ
 * 
 * e.g. 0-10, 10-20, 20-30
 *      dcUpdate(0)...dcUpdate(9) | dcUpdate(10)...dcUpdate(19)
 *      nonzeros: 3, 7, 12
 * 
 * in the simple but bad way: let each proc operate on the logical sizes which are >> nnz, and then figure out how the writes coexist
 *   guarantee: there will be space for the update; but how does each proc know where to do the write?
 *   p0 will skip updating until i == 3, and then it will want to set indices(0) to 3, indices(1) to 7
 *   p1 will skip updating i == 12, and then it will want to set ??? to 12. should be indices(2), but how does it know? -> from offset computed by .nnz of p0
 *    - need something other than simple update here that takes into account offset for the scan phase
 * 
 * in the better way: each proc should operate on nnz 
 *    - how does this work?
 *    - each proc allocates a buffer and appends nz values to it; reduce at end? (all collects become appends)
 *    - in delite ops, this is another usage of the generalized buffer/append/parallel write structure in scan
 * 
 * explicit maps over nz values (e.g. mapNNZ) can be done at the sparsevectorops layer to operate on the underlying arrays
 * 
 * what about allocation? how/when do we decide if the output should be sparse or dense? how is that communicated to delite?
 *  for now, we want to always preserve sparsity. 
 *    e.g., a map over a sparsevector should return a sparsevector
 *    in this case, we will want to communicate that the parallel operations follow the buffer-append-reduce model
 *      - cannot avoid iterating over all the logical elements, because the logical operation is defined on all of them?
 *      - what about zips? should be O(nnz(A)+nnz(B)): how can we represent this with delite ops?
 */

trait SparseVectorImplOps { this: OptiLA =>
  def sparsevector_obj_flatten_impl[A:Manifest](pieces: Rep[SparseVector[SparseVector[A]]]): Rep[SparseVector[A]]
  
  def sparsevector_apply_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int]): Rep[A]
  def sparsevector_update_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
  def sparsevector_append_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit]
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
  
  def sparsevector_mutabletrans_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] = {
    sparsevector_set_isrow(v,!v.isRow)
  }
  
  def bsearch(a: Rep[DeliteArray[Int]], _start: Rep[Int], _end: Rep[Int], pos: Rep[Int]): Rep[Int] = {
    // binary search index for pos
    var start = _start
    var end = _end
    var mid = (start+end)/2
    var found = false    
    while (!found && (start <= end)) {
      mid = (start+end)/2
      if (pos > a(mid)) {        
        start = mid + 1
      }
      else if (pos < a(mid)) {
        end = mid - 1
      }
      else {
        found = true
      }
    }

    if (found) mid 
    else {
      // maps to a reversible negative number representing the index to insert at if not found
      if (_end < _start) ~(_start) 
      else if (pos > a(mid)) ~(mid+1) 
      else ~mid 
    }
  } 
     
  protected def sparsevector_find_offset[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int]): Rep[Int] = {
    val indices = sparsevector_raw_indices(v)  
    bsearch(indices, 0, v.nnz-1, pos)
  }
  
  def sparsevector_apply_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int]): Rep[A] = {
    val data = sparsevector_raw_data(v)
    val offRaw = sparsevector_find_offset(v, pos)
    if (offRaw > -1) data(offRaw) else defaultValue[A]
  }
  
  def sparsevector_update_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {    
    val offRaw = sparsevector_find_offset(v, pos)
    if (offRaw > -1) darray_unsafe_update(sparsevector_raw_data(v), offRaw, x)
    else {
      if (x != defaultValue[A]) {
        val off = ~offRaw
        sparsevector_insertspace(v, off, 1)
        darray_unsafe_update(sparsevector_raw_indices(v), off, pos)
        darray_unsafe_update(sparsevector_raw_data(v), off, x)        
      }
    }    
  }
     
  // what is this method called in the delite collection interface? is the signature correct?
  // dc_scanupdate(x: DeliteCollection[A], chunkOffset: Int, elemOffset: Int, pos: Int, x: A)
  // TODO aks: how does DeliteOps get logicalPos in the scan phase???
  // def sparsevector_rawupdate_impl[A:Manifest](v: Rep[SparseVector[A]], physPos: Rep[Int], logicalPos: Rep[Int], x: Rep[A]): Rep[Unit] = {    
  //   val i = sparsevector_raw_indices(v)    
  //   val d = sparsevector_daw_data(v)
  //   array_unsafe_update(i,physPos) = logicalPos
  //   array_unsafe_update(d,physPos) = x
  // }
  
  /* 
   * Grows the logical SparseVector
   */
   
  // don't need to compute offset if we are appending.
  // the guarantee is that pos > all existing indices in the sparse vector.   
  def sparsevector_append_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    sparsevector_insert_at_off(v, v.nnz, pos, x)
  }
   
  def sparsevector_insert_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    val offRaw = sparsevector_find_offset(v, pos)
    val off = if (offRaw > -1) offRaw else ~offRaw    
    sparsevector_insert_at_off(v, off, pos, x)
  }
  
  protected def sparsevector_insert_at_off[A:Manifest](v: Rep[SparseVector[A]], off: Rep[Int], pos: Rep[Int], x: Rep[A]): Rep[Unit] = {
    sparsevector_insertspace(v, off, 1) 
    val data = sparsevector_raw_data(v)
    val indices = sparsevector_raw_indices(v)    
    darray_unsafe_update(indices, off, pos)
    darray_unsafe_update(data, off, x)
    for (i <- off+1 until v.nnz) {
      darray_unsafe_update(indices, i, indices(i) + 1)
    }
    
    sparsevector_set_length(v, v.length + 1)    
  }

  def sparsevector_insertall_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], xs: Rep[SparseVector[A]]): Rep[Unit] = {
    val offRaw = sparsevector_find_offset(v, pos)
    val off = if (offRaw > -1) offRaw else ~offRaw
    
    sparsevector_insertspace(v, off, xs.nnz)
    val data = sparsevector_raw_data(v)
    val indices = sparsevector_raw_indices(v)    
    for (i <- 0 until xs.nnz) {
      darray_unsafe_update(indices, i+off, sparsevector_raw_indices(xs).apply(i)+pos)
      darray_unsafe_update(data, i+off, sparsevector_raw_data(xs).apply(i))
    }    
    
    for (i <- off+xs.nnz until v.nnz) {
      darray_unsafe_update(indices, i, indices(i) + xs.length)
    }
    
    sparsevector_set_length(v, v.length + xs.length)
  }

  def sparsevector_copyfrom_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], xs: Rep[SparseVector[A]]): Rep[Unit] = {
    // val dataFrom = sparsevector_raw_data(xs)
    // val indicesFrom = sparsevector_raw_indices(xs)    
    //     
    // for (i <- 0 until indicesFrom.length) {
    //   sparsevector_update_impl(v, pos+indicesFrom(i), dataFrom(i))
    // }
    
    for (i <- 0 until xs.length) {
      val e = xs(i)
      // all elements from xs should be written to v at elements pos to xs.length, overwriting existing elements in v
      if (e != defaultValue[A]) {
        sparsevector_update_impl(v, pos+i, e)
      }
      // need to preserve zeros from xs to maintain the semantics of copyFrom, too
      // we could 'unset', but that's less efficient than just setting the value to 0      
      else if (v(pos+i) != defaultValue[A]) {
        sparsevector_update_impl(v, pos+i, defaultValue[A])
      }
    }
  }

  def sparsevector_removeall_impl[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    val data = sparsevector_raw_data(v)
    val indices = sparsevector_raw_indices(v)
    val startRaw = sparsevector_find_offset(v, pos)
    val start = if (startRaw > -1) startRaw else ~startRaw
    val endRaw = sparsevector_find_offset(v, pos+len)
    val end = if (endRaw > -1) endRaw else ~endRaw
    val remaining = v.nnz - end 
    darray_unsafe_copy(data, end, data, start, remaining)
    darray_unsafe_copy(indices, end, indices, start, remaining)
    sparsevector_set_length(v, v.length - len)
    sparsevector_set_nnz(v, start+remaining)
  }
  
  def sparsevector_clear_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] = {
    sparsevector_set_length(v, 0)
    sparsevector_set_nnz(v, 0)
    sparsevector_set_raw_data(v, (DeliteArray[A](0)).unsafeImmutable)
    sparsevector_set_raw_indices(v, (DeliteArray[Int](0)).unsafeImmutable)
  }
  
  /*
   * Grows the internal arrays representing a SparseVector, but does not modify the logical size of the vector.
   * Data and position arrays are always grown in lock-step.
   */
  protected def sparsevector_insertspace[A:Manifest](v: Rep[SparseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    sparsevector_ensureextra(v,len)
    val data = sparsevector_raw_data(v)
    val indices = sparsevector_raw_indices(v)
    darray_unsafe_copy(data, pos, data, pos + len, v.nnz - pos)
    darray_unsafe_copy(indices, pos, indices, pos + len, v.nnz - pos)
    sparsevector_set_nnz(v, v.nnz + len)
  }

  protected def sparsevector_ensureextra[A:Manifest](v: Rep[SparseVector[A]], extra: Rep[Int]): Rep[Unit] = {
    val data = sparsevector_raw_data(v)
    if (data.length - v.nnz < extra) {
      sparsevector_realloc(v, v.nnz + extra)
    }
  }

  protected def sparsevector_realloc[A:Manifest](v: Rep[SparseVector[A]], minLen: Rep[Int]): Rep[Unit] = {  
    val data = sparsevector_raw_data(v)
    val indices = sparsevector_raw_indices(v)
    var n = Math.max(4, data.length * 2)
    while (n < minLen) n = n*2
    val newData = DeliteArray[A](n)
    val newIndices = DeliteArray[Int](n)
    darray_unsafe_copy(data, 0, newData, 0, v.nnz)
    darray_unsafe_copy(indices, 0, newIndices, 0, v.nnz)
    sparsevector_set_raw_data(v, newData.unsafeImmutable)
    sparsevector_set_raw_indices(v, newIndices.unsafeImmutable)
  }
  
  def sparsevector_trim_impl[A:Manifest](v: Rep[SparseVector[A]]): Rep[Unit] = { 
    val data = sparsevector_raw_data(v)
    val indices = sparsevector_raw_indices(v)
    if (v.nnz < data.length) {
      val outData = DeliteArray[A](v.nnz)
      val outIndices = DeliteArray[Int](v.nnz)
      darray_unsafe_copy(data, 0, outData, 0, v.nnz)
      darray_unsafe_copy(indices, 0, outIndices, 0, v.nnz)
      sparsevector_set_raw_data(v, outData.unsafeImmutable)
      sparsevector_set_raw_indices(v, outIndices.unsafeImmutable)
    }    
  }
      
  def sparsevector_sort_impl[A:Manifest:Ordering](v: Rep[SparseVector[A]]): Rep[SparseVector[A]] = {
    throw new UnsupportedOperationException("tbd")
  }
  
  def sparsevector_times_matrix_impl[A:Manifest:Arith](v: Rep[SparseVector[A]], m: Rep[SparseMatrix[A]]): Rep[SparseVector[A]] = {
    throw new UnsupportedOperationException("tbd")
  }
}