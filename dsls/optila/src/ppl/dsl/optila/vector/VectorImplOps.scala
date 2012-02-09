package ppl.dsl.optila.vector

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optila.{Vector,Matrix,DenseVector,DenseMatrix}
import ppl.dsl.optila.{OptiLALift, OptiLACompiler, OptiLA}

trait VectorImplOps { this: OptiLA =>  
  def vector_equals_impl[A:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]]): Rep[Boolean]
  def vector_slice_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]], start: Rep[Int], end: Rep[Int])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_contains_impl[A:Manifest](v: Interface[Vector[A]], elem: Rep[A]): Rep[Boolean]
  def vector_distinct_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]  
  def vector_clone_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]  
  def vector_pprint_impl[A:Manifest](v: Interface[Vector[A]]): Rep[Unit]
  def vector_repmat_impl[A:Manifest](v: Interface[Vector[A]], i: Rep[Int], j: Rep[Int]): Rep[DenseMatrix[A]]
  def vector_mkstring_impl[A:Manifest](v: Interface[Vector[A]], sep: Rep[String]): Rep[String]
  def vector_concatenate_impl[A:Manifest,VA:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  
  //def vector_times_matrix_impl[A:Manifest:Arith,VA:Manifest](v: Interface[Vector[A]], m: Rep[DenseMatrix[A]])(implicit b: VectorBuilder[A,VA]): Rep[VA]
  def vector_outer_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]): Rep[DenseMatrix[A]]  
  
  def vector_median_impl[A:Manifest:Ordering](v: Interface[Vector[A]]): Rep[A]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: OptiLACompiler with OptiLALift =>

  //////////////////////////
  // kernel implementations
  
  def vector_equals_impl[A:Manifest](x: Interface[Vector[A]], y: Interface[Vector[A]]) = {
    if (x.length != y.length || x.isRow != y.isRow) {
      false
    }
    else {
      var foundDiff = false
      var i = 0
      while (i < x.length && !foundDiff) {
        if (x(i) != y(i))
          foundDiff = true
        i += 1
      }
      !foundDiff
    }
  }
  
  def vector_slice_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]], start: Rep[Int], end: Rep[Int])(implicit b: VectorBuilder[A,VA]) = { // TODO: use DeliteOp
    //v.chkRange(start, end)
    val resultOut = b.alloc(end-start, v.isRow)
    val result = b.toIntf(resultOut)    
    for (i <- start until end){
      result(i-start) = v(i)
    }
    resultOut.unsafeImmutable
  }
  
  def vector_contains_impl[A:Manifest](v: Interface[Vector[A]], elem: Rep[A]): Rep[Boolean] = {
    var i = unit(0)
    var found = false
    while (i < v.length && !found) {
      if (v(i) == elem) found = true
      i += 1
    }
    found
  }

  def vector_distinct_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = {
    val resultOut = b.alloc(0, v.isRow)
    val result = b.toIntf(resultOut) // this is sub-optimal - passing toIntf as an implicit does't kick in all the time    
    var i = unit(0)
    while (i < v.length) {
     if (!result.contains(v(i))) result += v(i)
     i += 1
    }
    resultOut.unsafeImmutable
  }
  
  def vector_clone_impl[A:Manifest,VA:Manifest](v: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = {
    val resultOut = b.alloc(v.length, v.isRow)
    val result = b.toIntf(resultOut) 
    var i = unit(0)
    while (i < v.length) {
     result(i) = v(i)
     i += 1
    }
    resultOut.unsafeImmutable
  }
    
  def vector_pprint_impl[A:Manifest](v: Interface[Vector[A]]) = {
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

  def vector_repmat_impl[A:Manifest](v: Interface[Vector[A]], iRep: Rep[Int], jRep: Rep[Int]) = {
    if (v.isRow) {
      val out = DenseMatrix[A](iRep, jRep*v.length)
      for (col <- 0 until jRep*v.length){
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
      val out = DenseMatrix[A](iRep*v.length, jRep)
      for (row <- 0 until iRep*v.length){
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
  
  def vector_mkstring_impl[A:Manifest](v: Interface[Vector[A]], sep: Rep[String]) = {
    var s = ""
    for (i <- 0 until v.length) {
      s = s + v(i)
      s = s + sep
    }
    s    
  }
  
  def vector_concatenate_impl[A:Manifest,VA:Manifest](v1: Interface[Vector[A]], v2: Interface[Vector[A]])(implicit b: VectorBuilder[A,VA]) = {
    // should use static rewritings or static overloading to do this
    //if (v1.IsInstanceOf[EmptyVector[A]]) v2
    //else if (v2.IsInstanceOf[EmptyVector[A]]) v1
    //else {
      val outAlloc = b.alloc(v1.length+v2.length, v1.isRow)
      val out = b.toIntf(outAlloc)
      for (i <- 0 until v1.length){
        out(i) = v1(i)
      }
      for (i <- 0 until v2.length){
        out(i+v1.length) = v2(i)
      }
      outAlloc.unsafeImmutable
    //}      
  }
  
  // def vector_times_matrix_impl[A:Manifest:Arith,VA:Manifest](v: Interface[Vector[A]], m: Rep[DenseMatrix[A]])(implicit b: VectorBuilder[A,VA]) = {    
  //   throw new UnsupportedOperationException("not implemented generically yet")
  //    val v_trans = v.t
  //    m.t.mapRowsToDenseVector { a_row => a_row *:* v_trans }    
  // }
  
  def vector_outer_impl[A:Manifest:Arith](v1: Interface[Vector[A]], v2: Interface[Vector[A]]) = {
    val out = DenseMatrix[A](v1.length, v2.length)
    for (i <- 0 until v1.length ){
      for (j <- 0 until v2.length ){
        out(i,j) = v1(i)*v2(j)
      }
    }
    out.unsafeImmutable    
  }
  
  def vector_median_impl[A:Manifest:Ordering](v: Interface[Vector[A]]) = {
    // TODO: this isn't the proper definition of median
    val x = v.sort
    x(x.length / 2)
  }
  
  
}