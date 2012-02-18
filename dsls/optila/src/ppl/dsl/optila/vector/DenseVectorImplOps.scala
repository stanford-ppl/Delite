package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila.{DenseVector,DenseMatrix}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

trait DenseVectorImplOps { this: OptiLA =>
  def densevector_obj_fromseq_impl[A:Manifest](xs: Rep[Seq[A]]): Rep[DenseVector[A]]
  def densevector_obj_ones_impl(length: Rep[Int]): Rep[DenseVector[Double]]
  def densevector_obj_onesf_impl(length: Rep[Int]): Rep[DenseVector[Float]]
  //def densevector_obj_zeros_impl(length: Rep[Int]): Rep[DenseVector[Double]]
  //def densevector_obj_zerosf_impl(length: Rep[Int]): Rep[DenseVector[Float]]
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
  def densevector_slice_impl[A:Manifest](v: Rep[DenseVector[A]], start: Rep[Int], end: Rep[Int]): Rep[DenseVector[A]]
  def densevector_concatenate_impl[A:Manifest](v1: Rep[DenseVector[A]], v2: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_times_matrix_impl[A:Manifest:Arith](v: Rep[DenseVector[A]], m: Rep[DenseMatrix[A]]): Rep[DenseVector[A]]
  def densevector_outer_impl[A:Manifest:Arith](v1: Rep[DenseVector[A]], v2: Rep[DenseVector[A]]): Rep[DenseMatrix[A]]
  def densevector_pprint_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit]
  def densevector_repmat_impl[A:Manifest](m: Rep[DenseVector[A]], i: Rep[Int], j: Rep[Int]): Rep[DenseMatrix[A]]
  def densevector_trans_impl[A](v: Rep[DenseVector[A]])(implicit mA: Manifest[A], vA: Manifest[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_median_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]): Rep[A]
  def densevector_filter_impl[A:Manifest](v: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]): Rep[DenseVector[A]]
  def densevector_partition_impl[A:Manifest](v: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]): (Rep[DenseVector[A]],Rep[DenseVector[A]])
  def densevector_contains_impl[A:Manifest](v: Rep[DenseVector[A]], elem: Rep[A]): Rep[Boolean]
  def densevector_distinct_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[DenseVector[A]]
  def densevector_min_index_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]): Rep[Int]
  def densevector_max_index_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]): Rep[Int]
  def densevector_find_impl[A:Manifest](v: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]): Rep[DenseVector[Int]]
  def densevector_mkstring_impl[A:Manifest](v: Rep[DenseVector[A]], sep: Rep[String]): Rep[String]
  def densevector_groupby_impl[A:Manifest,K:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[K]): Rep[DenseVector[DenseVector[A]]]
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

  def densevector_obj_ones_impl(length: Rep[Int]) = DenseVector[Double](length, true) mmap { e => 1. } 

  def densevector_obj_onesf_impl(length: Rep[Int]) = DenseVector[Float](length, true) mmap { e => 1f }

  //def densevector_obj_zeros_impl(length: Rep[Int]) = DenseVector[Double](length, true)

  //def densevector_obj_zerosf_impl(length: Rep[Int]) = DenseVector[Float](length, true)

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
    array_unsafe_update(d,pos,x) 
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
      array_unsafe_update(d,pos+i,xs(i))
      i += 1
    }
  }

  def densevector_removeall_impl[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    //chkRange(pos, pos + len)
    val data = densevector_raw_data(v)
    array_unsafe_copy(data, pos + len, data, pos, v.length - (pos + len))
    densevector_set_length(v, v.length - len)
  }

  def densevector_trim_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] = {
    val data = densevector_raw_data(v)
    if (v.length < data.length) {
      val outData = NewArray[A](v.length)
      array_unsafe_copy(data, 0, outData, 0, v.length)
      densevector_set_raw_data(v, outData.unsafeImmutable)
    }
  }
  
  def densevector_clear_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] = {
    densevector_set_length(v, 0)
    densevector_set_raw_data(v, (NewArray[A](0)).unsafeImmutable)
  }

  def densevector_mutabletrans_impl[A:Manifest](v: Rep[DenseVector[A]]): Rep[Unit] = {
    densevector_set_isrow(v,!v.isRow)
  }

  protected def densevector_insertspace[A:Manifest](v: Rep[DenseVector[A]], pos: Rep[Int], len: Rep[Int]): Rep[Unit] = {
    densevector_ensureextra(v,len)
    val data = densevector_raw_data(v)
    array_unsafe_copy(data, pos, data, pos + len, v.length - pos)
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
    val d = NewArray[A](n)
    array_unsafe_copy(data, 0, d, 0, v.length)
    densevector_set_raw_data(v, d.unsafeImmutable)
  }
  
  def densevector_sort_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]): Rep[DenseVector[A]] = {
    // inefficent! 3 copies
    val trimmedV = v.mutable()
    trimmedV.trim()
    val data = densevector_raw_data(trimmedV).sort    
    val out = DenseVector[A](0, v.isRow)
    densevector_set_length(out, v.length)
    densevector_set_raw_data(out, data.sort)
    out.unsafeImmutable
  }
  
  def densevector_slice_impl[A:Manifest](v: Rep[DenseVector[A]], start: Rep[Int], end: Rep[Int]) = { // TODO: use DeliteOp
    //v.chkRange(start, end)
    val out = DenseVector[A](end-start, v.isRow)
    for (i <- start until end){
      out(i-start) = v(i)
    }
    out.unsafeImmutable
  }

  def densevector_concatenate_impl[A:Manifest](v1: Rep[DenseVector[A]], v2: Rep[DenseVector[A]]) = {
    // this check doesn't work with nil densevectors
    //if (v1.isRow != v2.isRow) {
    //  println("error: trying to concatenate row and column densevectors")
      // TODo: need an exception throwing mechanism in generated code -- could be External, but needs to accessible from Base
    //}
    
    // even if one of the vectors is empty, this operation should semantically result in a copy (which is very unfortunate if we use it to do flatMap-reduces in delite ops)
    
    //if (v1.IsInstanceOf[EmptyVector[A]]) v2
    //else if (v2.IsInstanceOf[EmptyVector[A]]) v1
    //else {
      val out = DenseVector[A](v1.length+v2.length, v1.isRow)
      for (i <- 0 until v1.length){
        out(i) = v1(i)
      }
      for (i <- 0 until v2.length){
        out(i+v1.length) = v2(i)
      }
      out.unsafeImmutable
    //}
  }

  def densevector_times_matrix_impl[A:Manifest:Arith](v: Rep[DenseVector[A]], m: Rep[DenseMatrix[A]]) = {
    //v.chkVecMatAgree(v, m)
    val v_trans = v.t
    m.t.mapRowsToVector { a_row => a_row *:* v_trans }
  }

  def densevector_outer_impl[A:Manifest:Arith](collA: Rep[DenseVector[A]], collB: Rep[DenseVector[A]]) = {
    val out = DenseMatrix[A](collA.length, collB.length)
    for (i <- 0 until collA.length ){
      for (j <- 0 until collB.length ){
        out(i,j) = collA(i)*collB(j)
      }
    }
    out.unsafeImmutable
  }

  def densevector_pprint_impl[A:Manifest](v: Rep[DenseVector[A]]) = {
    if (v.isRow){
      print("[ ")
      for (i <- 0 until v.length){
        print(v(i)); print(" ");
      }
      print("]\\n")
    }
    else{
      for (i <- 0 until v.length){
        print("[")
        print(v(i))
        print(" ]\\n")
      }
    }
  }

  def densevector_repmat_impl[A:Manifest](v: Rep[DenseVector[A]], iRep: Rep[Int], jRep: Rep[Int]) = {
    if (v.isRow) {
      val out = DenseMatrix[A](iRep, jRep*v.length)
      for (col <- (0::jRep*v.length)){
        val colToRep = col % v.length
        var rI = unit(0)
        while(rI < iRep){
          out(rI, col) = v(colToRep)
          rI += 1
        }
      }
      out.unsafeImmutable
    }
    else {
      val out = Matrix[A](iRep*v.length, jRep)
      for (row <- (0::iRep*v.length)){
        val rowToRep = row % v.length
        var cI = unit(0)
        while(cI < jRep){
          out(row, cI) = v(rowToRep)
          cI += 1
        }
      }
      out.unsafeImmutable
    }
  }

  def densevector_trans_impl[A](v: Rep[DenseVector[A]])(implicit mA: Manifest[A], vA: Manifest[DenseVector[A]]) = {
    val out = DenseVector[A](v.length, !v.isRow)
    for (i <- 0 until v.length){
      out(i) = v(i)
    }
    out.unsafeImmutable
  }

  def densevector_median_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]) = {
    // TODO: this isn't the proper definition of median
    val x = v.sort
    x(x.length / 2)
  }

  def densevector_filter_impl[A:Manifest](v: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]) = {
    val result = DenseVector[A](0, v.isRow)
    for (i <- 0 until v.length) {
      val x = v(i)
      if (pred(x)) result += x
    }

    result.unsafeImmutable
  }

  def densevector_partition_impl[A:Manifest](v: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]) = {
    val resultT = DenseVector[A](0, v.isRow)
    val resultF = DenseVector[A](0, v.isRow)
    for (i <- 0 until v.length) {
      val x = v(i)
      (if (pred(x)) resultT else resultF) += x
    }

    (resultT.unsafeImmutable, resultF.unsafeImmutable)
  }

  def densevector_contains_impl[A:Manifest](v: Rep[DenseVector[A]], elem: Rep[A]): Rep[Boolean] = {
    var i = unit(0)
    var found = false
    while (i < v.length && !found) {
      if (v(i) == elem) found = true
      i += 1
    }
    found
  }

  def densevector_distinct_impl[A:Manifest](v: Rep[DenseVector[A]]) = {
    val result = DenseVector[A](0, v.isRow)
    var i = unit(0)
    while (i < v.length) {
     if (!result.contains(v(i))) result += v(i)
     i += 1
    }
    result.unsafeImmutable
  }

  def densevector_min_index_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]) = {
    var minIndex = 0
    var min = v(0)
    var j = 1
    while( j < v.length ){
      if (v(j) < min) {
        min = v(j)
        minIndex = j
      }
      j += 1
    }

    minIndex
  }

  def densevector_max_index_impl[A:Manifest:Ordering](v: Rep[DenseVector[A]]) = {
    var maxIndex = 0
    var max = v(0)
    var j = 1
    while( j < v.length ){
      if (v(j) > max) {
        max = v(j)
        maxIndex = j
      }
      j += 1
    }

    maxIndex
  }

  def densevector_find_impl[A:Manifest](v: Rep[DenseVector[A]], pred: Rep[A] => Rep[Boolean]) = {
    val indices = DenseVector[Int](0)
    for (i <- 0 until v.length) {
      if (pred(v(i))) indices += i
    }
    indices.unsafeImmutable.asInstanceOf[Rep[DenseVector[Int]]]
  }

  def densevector_mkstring_impl[A:Manifest](v: Rep[DenseVector[A]], sep: Rep[String]) = {
    var s = ""
    for (i <- 0 until v.length) {
      s = s + v(i)
      s = s + sep
    }
    s
  }
  
  def densevector_groupby_impl[A:Manifest,K:Manifest](x: Rep[DenseVector[A]], pred: Rep[A] => Rep[K]): Rep[DenseVector[DenseVector[A]]] = {
    val groups = HashMap[K,DenseVector[A]]()

    var i = 0
    while (i < x.length) {
      val key = pred(x(i))      
      if (!(groups contains key)) {
        groups(key) = DenseVector[A](0,x.isRow).unsafeImmutable        
      }
      //groups(key) += x(i)
      groups(key) = groups(key) :+ x(i) // inefficient, but have to follow nested mutable rule
      i += 1
    }

    val out = DenseVector[DenseVector[A]](0,true)
    for (v <- groups.values) {
      out += v.unsafeImmutable       
    }    
    out.unsafeImmutable
  }
}